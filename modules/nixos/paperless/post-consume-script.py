"""
Paperless NGX Post-Consumption Script with User-based Script Registration
Supports multiple post-consumption actions based on document owner.
"""

import os
import sys
import httpx
import json
import re
from datetime import datetime
from decimal import Decimal, InvalidOperation

# Configuration
PAPERLESS_URL = os.getenv('PAPERLESS_URL', 'http://localhost:8000')
API_TOKEN_PATH = os.getenv('API_TOKEN_PATH', '/run/agenix/paperless-api-token')
FINANCIAL_YEAR_FIELD_ID = 1
AMOUNT_FIELD_ID = 3

def read_api_token():
    """Read API token from agenix secret file"""
    try:
        with open(API_TOKEN_PATH, 'r') as f:
            return f.read().strip()
    except FileNotFoundError:
        print(f"ERROR: API token file not found: {API_TOKEN_PATH}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Error reading API token: {e}", file=sys.stderr)
        sys.exit(1)

def get_financial_year(date_obj):
    """
    Calculate financial year based on date.
    FY 2025 = July 1, 2024 to June 30, 2025
    """
    if date_obj.month >= 7:  # July to December
        return date_obj.year + 1
    else:  # January to June
        return date_obj.year

def parse_document_date(date_string):
    """
    Parse document creation date with robust handling of various formats.
    Handles microseconds and timezone information.
    """
    try:
        # Remove timezone info by splitting on '+' or 'Z'
        if '+' in date_string:
            date_part = date_string.split('+')[0]
        elif 'Z' in date_string:
            date_part = date_string.split('Z')[0]
        else:
            date_part = date_string

        # Remove microseconds if present (anything after the last dot in seconds)
        if '.' in date_part:
            date_part = date_part.rsplit('.', 1)[0]

        # Try datetime first, then date-only
        for fmt in ('%Y-%m-%d %H:%M:%S', '%Y-%m-%d'):
            try:
                return datetime.strptime(date_part, fmt)
            except ValueError:
                continue

        raise ValueError(f"does not match any expected format")

    except ValueError as e:
        print(f"ERROR: Error parsing date '{date_string}': {e}", file=sys.stderr)
        return None

def get_document_content(document_id):
    """Get the OCR content of a document"""
    api_token = read_api_token()
    url = f"{PAPERLESS_URL}/api/documents/{document_id}/"
    headers = {
        'Authorization': f'Token {api_token}',
        'Content-Type': 'application/json'
    }

    try:
        response = httpx.get(url, headers=headers, timeout=30)
        if response.status_code != 200:
            print(f"ERROR: Failed to get document content: {response.status_code}", file=sys.stderr)
            return None

        document_data = response.json()
        return document_data.get('content', '')
    except httpx.HTTPError as e:
        print(f"ERROR: Network error getting document content: {e}", file=sys.stderr)
        return None

def extract_amounts_from_text(text):
    """
    Extract monetary amounts from text using various patterns.
    Returns a list of amounts as Decimal objects.
    """
    amounts = []

    # Pattern 1: Currency symbol followed by amount (e.g., $123.45, €99.99)
    currency_patterns = [
        r'[\$£€¥₹₽]\s*(\d{1,3}(?:,\d{3})*(?:\.\d{2})?)',
        r'[\$£€¥₹₽]\s*(\d+(?:\.\d{2})?)',
    ]

    # Pattern 2: Amount followed by currency (e.g., 123.45 USD, 99,99 EUR)
    currency_suffix_patterns = [
        r'(\d{1,3}(?:,\d{3})*(?:\.\d{2})?)\s*(?:USD|EUR|GBP|AUD|CAD|JPY|INR|RUB)',
        r'(\d+(?:\.\d{2})?)\s*(?:USD|EUR|GBP|AUD|CAD|JPY|INR|RUB)',
    ]

    # Pattern 3: Common receipt/invoice keywords followed by amounts
    keyword_patterns = [
        r'(?:total|amount|sum|grand\s*total|subtotal|balance)\s*:?\s*[\$£€¥₹₽]?\s*(\d{1,3}(?:,\d{3})*(?:\.\d{2})?)',
        r'(?:total|amount|sum|grand\s*total|subtotal|balance)\s*:?\s*(\d+(?:\.\d{2})?)',
    ]

    # Pattern 4: Standalone amounts (more restrictive to avoid false positives)
    standalone_patterns = [
        r'\b(\d{1,3}(?:,\d{3})+\.\d{2})\b',  # Amounts with thousands separators
        r'\b(\d{2,}\.\d{2})\b',              # Amounts 10.00 and above
    ]

    all_patterns = currency_patterns + currency_suffix_patterns + keyword_patterns + standalone_patterns

    for pattern in all_patterns:
        matches = re.finditer(pattern, text, re.IGNORECASE)
        for match in matches:
            amount_str = match.group(1)
            # Remove commas and convert to Decimal
            try:
                amount_str = amount_str.replace(',', '')
                amount = Decimal(amount_str)
                if amount > 0:  # Only positive amounts
                    amounts.append(amount)
            except (InvalidOperation, ValueError):
                continue

    return amounts

def get_document_type_from_api(document_id):
    """Get document type from API if not available in environment"""
    api_token = read_api_token()
    url = f"{PAPERLESS_URL}/api/documents/{document_id}/"
    headers = {
        'Authorization': f'Token {api_token}',
        'Content-Type': 'application/json'
    }

    try:
        response = httpx.get(url, headers=headers, timeout=30)
        if response.status_code != 200:
            print(f"ERROR: Failed to get document type from API: {response.status_code}", file=sys.stderr)
            return None

        document_data = response.json()
        document_type_id = document_data.get('document_type')

        if not document_type_id:
            return None

        # Get document type name
        type_url = f"{PAPERLESS_URL}/api/document_types/{document_type_id}/"
        type_response = httpx.get(type_url, headers=headers, timeout=30)

        if type_response.status_code != 200:
            print(f"ERROR: Failed to get document type name from API: {type_response.status_code}", file=sys.stderr)
            return None

        type_data = type_response.json()
        return type_data.get('name', '').strip()

    except httpx.HTTPError as e:
        print(f"ERROR: Network error getting document type: {e}", file=sys.stderr)
        return None
    except Exception as e:
        print(f"ERROR: Unexpected error getting document type: {e}", file=sys.stderr)
        return None

def extract_document_amount(document):
    """
    Extract the total amount from a receipt or invoice document.
    Only processes documents with "Receipt" or "Invoice" document type.
    """
    # Check if document type is "Receipt" or "Invoice"
    document_type = document.get('type', '').strip()

    # If document type is not in environment, try to get it from API
    if not document_type:
        print("Document type not found in environment, fetching from API...")
        document_type = get_document_type_from_api(document['id'])
        if document_type:
            print(f"Retrieved document type from API: '{document_type}'")
        else:
            print("Could not retrieve document type from API")
            return True

    if document_type not in ['Receipt', 'Invoice', 'Bill']:
        print(f"Skipping amount extraction - document type '{document_type}' is not Receipt or Invoice")
        return True

    print(f"Document type is '{document_type}' - proceeding with amount extraction")

    # Get document content
    content = get_document_content(document['id'])
    if not content:
        print("WARNING: Could not retrieve document content for amount extraction")
        return False

    # Extract amounts
    amounts = extract_amounts_from_text(content)

    if not amounts:
        print("WARNING: No amounts found in document")
        return True  # Not necessarily an error, just no amounts found

    # Choose the largest amount as the likely total
    # Sort amounts and take the largest one
    amounts.sort(reverse=True)
    total_amount = amounts[0]

    print(f"Found {len(amounts)} amounts, using largest: ${total_amount}")

    # Store the amount in custom field
    api_token = read_api_token()
    url = f"{PAPERLESS_URL}/api/documents/{document['id']}/"
    headers = {
        'Authorization': f'Token {api_token}',
        'Content-Type': 'application/json'
    }

    try:
        # Get current document data
        response = httpx.get(url, headers=headers, timeout=30)
        if response.status_code != 200:
            print(f"ERROR: Failed to get document {document['id']}: {response.status_code}", file=sys.stderr)
            return False

        document_data = response.json()
        custom_fields = document_data.get('custom_fields', [])

        # Update or add amount field (using float for monetary field type)
        field_exists = False
        for field in custom_fields:
            if field['field'] == AMOUNT_FIELD_ID:
                field['value'] = float(total_amount)
                field_exists = True
                break

        if not field_exists:
            custom_fields.append({
                'field': AMOUNT_FIELD_ID,
                'value': float(total_amount)
            })

        # Update document
        update_data = {'custom_fields': custom_fields}
        response = httpx.patch(url, headers=headers, json=update_data, timeout=30)

        if response.status_code == 200:
            print(f"Successfully updated document {document['id']} with amount ${total_amount}")
            return True
        else:
            print(f"ERROR: Failed to update document with amount: {response.status_code}", file=sys.stderr)
            return False

    except httpx.HTTPError as e:
        print(f"ERROR: Network error: {e}", file=sys.stderr)
        return False
    except Exception as e:
        print(f"ERROR: Unexpected error in amount extraction: {e}", file=sys.stderr)
        return False

def update_financial_year(document):
    """
    Update the Financial Year custom field for a document.
    Only processes documents with "Tax" tag.
    """
    # Check if document has "Tax" tag
    document_tags = document.get('tags', '').split(',') if document.get('tags') else []
    document_tags = [tag.strip() for tag in document_tags if tag.strip()]

    if 'Tax' not in document_tags:
        print("Skipping financial year update - document does not have 'Tax' tag")
        return True

    print("Document has 'Tax' tag - proceeding with financial year update")

    api_token = read_api_token()

    # Parse the creation date using the new robust parsing function
    created_date = parse_document_date(document['created'])
    if not created_date:
        return False

    # Calculate financial year
    fy = get_financial_year(created_date)
    print(f"Setting Financial Year: {fy}")

    url = f"{PAPERLESS_URL}/api/documents/{document['id']}/"
    headers = {
        'Authorization': f'Token {api_token}',
        'Content-Type': 'application/json'
    }

    try:
        # Get current document data
        response = httpx.get(url, headers=headers, timeout=30)
        if response.status_code != 200:
            print(f"ERROR: Failed to get document {document['id']}: {response.status_code}", file=sys.stderr)
            return False

        document_data = response.json()
        custom_fields = document_data.get('custom_fields', [])

        # Update or add financial year field
        field_exists = False
        for field in custom_fields:
            if field['field'] == FINANCIAL_YEAR_FIELD_ID:
                field['value'] = fy
                field_exists = True
                break

        if not field_exists:
            custom_fields.append({
                'field': FINANCIAL_YEAR_FIELD_ID,
                'value': fy
            })

        # Update document
        update_data = {'custom_fields': custom_fields}
        response = httpx.patch(url, headers=headers, json=update_data, timeout=30)

        if response.status_code == 200:
            print(f"Successfully updated document {document['id']} with Financial Year {fy}")
            return True
        else:
            print(f"ERROR: Failed to update document: {response.status_code}", file=sys.stderr)
            return False

    except httpx.HTTPError as e:
        print(f"ERROR: Network error: {e}", file=sys.stderr)
        return False
    except Exception as e:
        print(f"ERROR: Unexpected error: {e}", file=sys.stderr)
        return False

def get_document_from_env():
    """Read document information from environment variables"""
    document = {}
    for key, value in os.environ.items():
        if key.startswith('DOCUMENT_'):
            doc_key = key[9:].lower()  # Remove 'DOCUMENT_' prefix and lowercase
            document[doc_key] = value
    return document

def setup_user_scripts():
    """Set up user script mappings"""
    return {
        'brett': [update_financial_year, extract_document_amount]
    }

def main():
    """Main script execution"""
    document = get_document_from_env()

    if not document.get('id'):
        print("ERROR: DOCUMENT_ID not found in environment variables", file=sys.stderr)
        sys.exit(1)

    if not document.get('owner'):
        print("No document owner - skipping processing")
        return

    # Get user scripts
    user_scripts = setup_user_scripts()
    scripts = user_scripts.get(document['owner'])

    if not scripts:
        print(f"No scripts configured for user '{document['owner']}' - skipping processing")
        return

    print(f"Processing {len(scripts)} scripts for user: {document['owner']}")

    # Run scripts
    success_count = 0
    for script in scripts:
        try:
            if script(document):
                success_count += 1
            else:
                print(f"WARNING: Script {script.__name__} returned failure for document {document.get('id', '?')} (owner={document.get('owner', '?')})", file=sys.stderr)
        except Exception as e:
            import traceback
            print(f"ERROR: Script {script.__name__} raised exception for document {document.get('id', '?')}: {e}", file=sys.stderr)
            traceback.print_exc(file=sys.stderr)

    if success_count != len(scripts):
        print(f"ERROR: {len(scripts) - success_count}/{len(scripts)} scripts failed for document {document.get('id', '?')} (owner={document.get('owner', '?')}, tags={document.get('tags', '')})", file=sys.stderr)
        sys.exit(1)

    print("All scripts completed successfully")

if __name__ == "__main__":
    import traceback
    try:
        main()
    except Exception as e:
        print(f"FATAL: Unhandled exception in post-consume script: {e}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)
