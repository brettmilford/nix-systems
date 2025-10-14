#!/bin/bash
# QEMU VM Snapshot Manager
# Usage: qemu-snapshot <vm_name> <action> [snapshot_name]
# Actions: create, rollback, list, delete, status

set -euo pipefail

# Configuration
SNAPSHOT_DIR="/var/lib/qemu/snapshots"
RUNTIME_DIR="/run/qemu"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_vm_exists() {
    local vm_name="$1"
    if [[ ! -d "$RUNTIME_DIR/$vm_name" ]]; then
        log_error "Runtime directory $RUNTIME_DIR/$vm_name for '$vm_name' not found."
        exit 1
    fi

    if [[ ! -S "$RUNTIME_DIR/$vm_name/monitor.sock" ]]; then
        log_error "Monitor socket $RUNTIME_DIR/$vm_name/monitor.sock for VM '$vm_name' not found."
        exit 1
    fi

    if [[ ! -S "$RUNTIME_DIR/$vm_name/qmp.sock" ]]; then
        log_error "QMP socket $RUNTIME_DIR/$vm_name/qmp.sock for VM '$vm_name' not found."
        exit 1
    fi
}

send_monitor_cmd() {
    local vm_name="$1"
    local cmd="$2"
    echo "$cmd" | socat - "UNIX-CONNECT:$RUNTIME_DIR/$vm_name/monitor.sock" 2>/dev/null || {
        log_error "Failed to send monitor command to VM '$vm_name'"
        return 1
    }
}

send_qmp_cmd() {
    local vm_name="$1"
    local cmd="$2"
    local handshake='{"execute": "qmp_capabilities"}'
    echo "${handshake}${cmd}" | socat - "UNIX-CONNECT:$RUNTIME_DIR/$vm_name/qmp.sock" 2>/dev/null || {
        log_error "Failed to send QMP command to VM '$vm_name'"
        return 1
    }
}

get_vm_status() {
    local vm_name="$1"
    send_monitor_cmd "$vm_name" "info status" | grep -o "running\|paused\|stopped" || echo "unknown"
}

list_internal_snapshots() {
    local vm_name="$1"
    log_info "Internal snapshots for VM '$vm_name':"
    send_monitor_cmd "$vm_name" "info snapshots" | grep -E "^[0-9]+" || log_warning "No internal snapshots found"
}

list_external_snapshots() {
    local vm_name="$1"
    log_info "External snapshots for VM '$vm_name':"
    if [[ -d "$SNAPSHOT_DIR/$vm_name" ]]; then
        find "$SNAPSHOT_DIR/$vm_name" -name "*.qcow2" -exec basename {} .qcow2 \; 2>/dev/null | sort || log_warning "No external snapshots found"
    else
        log_warning "No external snapshots directory found"
    fi
}

create_internal_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"

    log_info "Creating internal snapshot '$snapshot_name' for VM '$vm_name'..."

    local result
    result=$(send_monitor_cmd "$vm_name" "savevm $snapshot_name")

    if echo "$result" | grep -q "Error"; then
        log_error "Failed to create internal snapshot: $result"
        return 1
    else
        log_success "Internal snapshot '$snapshot_name' created successfully"
        return 0
    fi
}

create_external_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"
    local snapshot_file="$SNAPSHOT_DIR/$vm_name/${snapshot_name}.qcow2"

    # Create snapshot directory if it doesn't exist
    mkdir -p "$SNAPSHOT_DIR/$vm_name"

    log_info "Creating external snapshot '$snapshot_name' for VM '$vm_name'..."
    log_info "Snapshot file: $snapshot_file"

    # Use QMP for external snapshots
    local qmp_cmd='{"execute": "blockdev-snapshot-sync", "arguments": {"device": "virtio0", "snapshot-file": "'$snapshot_file'", "format": "qcow2"}}'

    local result
    result=$(send_qmp_cmd "$vm_name" "$qmp_cmd")

    if echo "$result" | grep -q '"error"'; then
        log_error "Failed to create external snapshot: $result"
        return 1
    else
        log_success "External snapshot '$snapshot_name' created successfully"

        # Create metadata file
        cat > "$SNAPSHOT_DIR/$vm_name/${snapshot_name}.meta" << EOF
timestamp=$(date +%Y%m%d_%H%M%S)
vm_name=$vm_name
snapshot_name=$snapshot_name
snapshot_file=$snapshot_file
creation_date=$(date)
EOF
        log_info "Metadata saved to ${snapshot_name}.meta"
        return 0
    fi
}

rollback_internal_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"

    log_warning "Rolling back to internal snapshot '$snapshot_name' will pause the VM briefly..."
    read -r -p "Continue? (yes/no): " confirm

    if [[ "$confirm" != "yes" ]]; then
        log_info "Rollback cancelled"
        return 1
    fi

    log_info "Rolling back VM '$vm_name' to internal snapshot '$snapshot_name'..."

    local result
    result=$(send_monitor_cmd "$vm_name" "loadvm $snapshot_name")

    if echo "$result" | grep -q "Error\|could not"; then
        log_error "Failed to rollback to internal snapshot: $result"
        return 1
    else
        log_success "Successfully rolled back to internal snapshot '$snapshot_name'"
        return 0
    fi
}

rollback_external_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"
    local snapshot_file="$SNAPSHOT_DIR/$vm_name/${snapshot_name}.qcow2"

    if [[ ! -f "$snapshot_file" ]]; then
        log_error "External snapshot file not found: $snapshot_file"
        return 1
    fi

    log_warning "Rolling back to external snapshot '$snapshot_name' requires stopping the VM!"
    log_warning "All changes since snapshot creation will be lost!"
    read -r -p "Continue? (yes/no): " confirm

    if [[ "$confirm" != "yes" ]]; then
        log_info "Rollback cancelled"
        return 1
    fi

    log_error "External snapshot rollback requires manual VM restart with different disk image."
    log_error "This script cannot automatically perform external snapshot rollbacks."
    log_info "To rollback manually:"
    log_info "1. Stop the VM"
    log_info "2. Update VM configuration to use: $snapshot_file"
    log_info "3. Restart the VM"

    return 1
}

delete_internal_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"

    log_warning "Deleting internal snapshot '$snapshot_name'..."
    read -r -p "Continue? (yes/no): " confirm

    if [[ "$confirm" != "yes" ]]; then
        log_info "Deletion cancelled"
        return 1
    fi

    log_info "Deleting internal snapshot '$snapshot_name' from VM '$vm_name'..."

    local result
    result=$(send_monitor_cmd "$vm_name" "delvm $snapshot_name")

    if echo "$result" | grep -q "Error\|could not"; then
        log_error "Failed to delete internal snapshot: $result"
        return 1
    else
        log_success "Internal snapshot '$snapshot_name' deleted successfully"
        return 0
    fi
}

delete_external_snapshot() {
    local vm_name="$1"
    local snapshot_name="$2"
    local snapshot_file="$SNAPSHOT_DIR/$vm_name/${snapshot_name}.qcow2"
    local meta_file="$SNAPSHOT_DIR/$vm_name/${snapshot_name}.meta"

    if [[ ! -f "$snapshot_file" ]]; then
        log_error "External snapshot file not found: $snapshot_file"
        return 1
    fi

    log_warning "Deleting external snapshot '$snapshot_name'..."
    read -r -p "Continue? (yes/no): " confirm

    if [[ "$confirm" != "yes" ]]; then
        log_info "Deletion cancelled"
        return 1
    fi

    log_info "Deleting external snapshot files..."
    rm -f "$snapshot_file" "$meta_file"
    log_success "External snapshot '$snapshot_name' deleted successfully"
}

show_vm_status() {
    local vm_name="$1"

    log_info "VM Status for '$vm_name':"
    echo "----------------------------------------"

    local status
    status=$(get_vm_status "$vm_name")
    echo "Status: $status"

    echo
    send_monitor_cmd "$vm_name" "info version"

    echo
    send_monitor_cmd "$vm_name" "info block" | head -10

    echo
    list_internal_snapshots "$vm_name"

    echo
    list_external_snapshots "$vm_name"
}

usage() {
    echo "Usage: ${0##*/} <vm_name> <action> [snapshot_name]"
    echo
    echo "Actions:"
    echo "  create <snapshot_name>     Create a snapshot (external by default)"
    echo "  create-int <snapshot_name> Create an internal snapshot"
    echo "  rollback <snapshot_name>   Rollback to a snapshot"
    echo "  list                       List all snapshots"
    echo "  delete <snapshot_name>     Delete a snapshot"
    echo "  status                     Show VM status and snapshots"
}

# Main script
main() {
    # Check arguments
    if [[ $# -lt 2 ]]; then
        usage
        exit 1
    fi

    # Check if running as root
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root"
        exit 1
    fi

    local vm_name="$1"
    local action="$2"
    local snapshot_name="${3:-}"

    # Validate VM exists and is running
    check_vm_exists "$vm_name"

    case "$action" in
        "create")
            if [[ -z "$snapshot_name" ]]; then
                snapshot_name="snapshot_$(date +%Y%m%d_%H%M%S)"
                log_info "No snapshot name provided, using: $snapshot_name"
            fi
            create_external_snapshot "$vm_name" "$snapshot_name"
            ;;
        "create-int")
            if [[ -z "$snapshot_name" ]]; then
                snapshot_name="snapshot_$(date +%Y%m%d_%H%M%S)"
                log_info "No snapshot name provided, using: $snapshot_name"
            fi
            create_internal_snapshot "$vm_name" "$snapshot_name"
            ;;
        "rollback")
            if [[ -z "$snapshot_name" ]]; then
                log_error "Snapshot name required for rollback"
                exit 1
            fi

            if send_monitor_cmd "$vm_name" "info snapshots" | grep -q "$snapshot_name"; then
                # rollback_internal_snapshot "$vm_name" "$snapshot_name"
                :
            elif [[ -f "$SNAPSHOT_DIR/$vm_name/${snapshot_name}.qcow2" ]]; then
                rollback_external_snapshot "$vm_name" "$snapshot_name"
            else
                log_error "Snapshot '$snapshot_name' not found"
                exit 1
            fi
            ;;
        "list")
            list_internal_snapshots "$vm_name"
            echo
            list_external_snapshots "$vm_name"
            ;;
        "delete")
            if [[ -z "$snapshot_name" ]]; then
                log_error "Snapshot name required for deletion"
                exit 1
            fi
            # Try internal first, then external
            if send_monitor_cmd "$vm_name" "info snapshots" | grep -q "$snapshot_name"; then
                delete_internal_snapshot "$vm_name" "$snapshot_name"
            elif [[ -f "$SNAPSHOT_DIR/$vm_name/${snapshot_name}.qcow2" ]]; then
                delete_external_snapshot "$vm_name" "$snapshot_name"
            else
                log_error "Snapshot '$snapshot_name' not found"
                exit 1
            fi
            ;;
        "status")
            show_vm_status "$vm_name"
            ;;
        *)
            log_error "Unknown action: $action"
            usage
            exit 1
            ;;
    esac
}

# Run main function
main "$@"
