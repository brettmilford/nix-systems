{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.31.4";

  src = fetchFromGitHub {
    owner = "agentclientprotocol";
    repo = "claude-agent-acp";
    rev = "v${version}";
    hash = "sha256-cXTtDekC0+n1NCgTzIyGSqHEgpgdHP6EVI23L4nCbWE=";
  };

  npmDepsHash = "sha256-PmcE99h303iOH5OJ4wCwxgR+0zVJM8O5A3ZyBgPxJeM=";

  meta = {
    description = "An ACP-compatible coding agent powered by the Claude Agent SDK";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = lib.licenses.asl20;
    mainProgram = "claude-agent-acp";
  };
}
