#!/usr/bin/env bash
#
# run-copilot-init.sh
# Runs the `copilot` CLI with a multi-line prompt read from `instructions.md` to document
# a COBOL program using the COBOL Documentation Module.
#
# Usage:
#   ./run-copilot-init.sh path/to/program.cob [repo-name]
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load .env file if it exists
if [ -f "$SCRIPT_DIR/.env" ]; then
  # Source the .env file to load variables
  set -a
  . "$SCRIPT_DIR/.env"
  set +a
fi

# Defaults
COBOL_PROGRAM_PATH="${COBOL_PROGRAM_PATH:-}"
REPO_NAME="${REPO_NAME:-}"
DRY_RUN=0
# Safer default: do not allow all paths unless explicitly requested
ALLOW_ALL_PATHS=0
REFRESH=0
POSITIONAL_COUNT=0

# Simple arg parsing: positional COBOL path and repo name, plus optional flags
for arg in "$@"; do
  case "$arg" in
    --dry-run|-n)
      DRY_RUN=1
      ;;
    --refresh)
      REFRESH=1
      ;;
    --allow-all-paths)
      ALLOW_ALL_PATHS=1
      ;;
    *)
      # First positional arg is COBOL path, second is repo name
      if [ -z "$COBOL_PROGRAM_PATH" ]; then
        COBOL_PROGRAM_PATH="$arg"
        POSITIONAL_COUNT=$((POSITIONAL_COUNT + 1))
      elif [ -z "$REPO_NAME" ]; then
        REPO_NAME="$arg"
        POSITIONAL_COUNT=$((POSITIONAL_COUNT + 1))
      fi
      ;;
  esac
done

INSTRUCTIONS_PATH="$SCRIPT_DIR/instructions-init.md"
if [ "$REFRESH" -eq 1 ]; then
  INSTRUCTIONS_PATH="$SCRIPT_DIR/instructions-refresh.md"
fi

if [ ! -f "$INSTRUCTIONS_PATH" ]; then
  echo "ERROR: instructions file not found at $INSTRUCTIONS_PATH" >&2
  exit 2
fi

# Read the prompt (preserve newlines)
PROMPT_CONTENT=$(cat "$INSTRUCTIONS_PATH")

if echo "$PROMPT_CONTENT" | grep -q "<COBOL_PROGRAM_PATH>"; then
  if [ -z "$COBOL_PROGRAM_PATH" ]; then
    echo "ERROR: instructions.md contains <COBOL_PROGRAM_PATH> placeholder but no COBOL program path was provided." >&2
    echo "Please provide a COBOL path as the first argument, e.g. ./run-copilot-init.sh /path/to/program.cob repo-name" >&2
    exit 6
  fi
  # Replace the placeholder with the provided path
  PROMPT_CONTENT=$(echo "$PROMPT_CONTENT" | sed "s|<COBOL_PROGRAM_PATH>|$COBOL_PROGRAM_PATH|g")
else
  echo "ERROR: instructions.md must contain the placeholder <COBOL_PROGRAM_PATH>. Please add it and try again." >&2
  exit 7
fi

if echo "$PROMPT_CONTENT" | grep -q "<REPO_NAME>"; then
  if [ -z "$REPO_NAME" ]; then
    # Try to infer repo name from path if not provided
    if [[ "$COBOL_PROGRAM_PATH" =~ temp-repos/([^/]+)/ ]]; then
      REPO_NAME="${BASH_REMATCH[1]}"
      echo "INFO: Inferred repository name: $REPO_NAME" >&2
    else
      echo "ERROR: instructions.md contains <REPO_NAME> placeholder but no repository name was provided." >&2
      echo "Please provide a repo name as the second argument, e.g. ./run-copilot-init.sh /path/to/program.cob repo-name" >&2
      exit 8
    fi
  fi
  # Replace the placeholder with the repo name
  PROMPT_CONTENT=$(echo "$PROMPT_CONTENT" | sed "s|<REPO_NAME>|$REPO_NAME|g")
fi

echo "Running copilot with the following prompt:"
echo
echo "$PROMPT_CONTENT"
echo
echo "Invoking copilot..."

# Preflight: ensure copilot is available
if ! command -v copilot >/dev/null 2>&1; then
  cat <<'MSG' >&2
ERROR: 'copilot' executable not found on PATH.
Install the GitHub Copilot CLI and ensure it's on your PATH. Quick install (requires Node.js/npm):
  npm install -g @github/copilot
Docs: https://docs.github.com/copilot/concepts/agents/about-copilot-cli
Repo:  https://github.com/github/copilot-cli
MSG
  exit 5
fi

# Compute parent folder of current working directory
CURRENT_FOLDER="$(pwd -P)"
PARENT_FOLDER="$(dirname "$CURRENT_FOLDER")"

allow_paths_flag=""
if [ "$ALLOW_ALL_PATHS" -eq 1 ]; then allow_paths_flag="--allow-all-paths"; fi
INSTRUCTIONS_BASENAME="$(basename "$INSTRUCTIONS_PATH")"
echo "Running: copilot -p <$INSTRUCTIONS_BASENAME> --allow-all-tools $allow_paths_flag --add-dir \"$PARENT_FOLDER\""

if [ "$DRY_RUN" -eq 1 ]; then
  echo "DRY RUN: resolved prompt:"
  echo
  echo "$PROMPT_CONTENT"
  echo
  echo "DRY RUN: command to be executed:" 
  echo "copilot -p <${INSTRUCTIONS_BASENAME}> --allow-all-tools $allow_paths_flag --add-dir \"$PARENT_FOLDER\""
  exit 0
fi

# Invoke copilot, passing the prompt as a single argument
# Use printf to ensure newlines are preserved when expanding the variable
if [ "$ALLOW_ALL_PATHS" -eq 1 ]; then
  copilot -p "$PROMPT_CONTENT" --allow-all-tools --allow-all-paths --add-dir "$PARENT_FOLDER"
else
  copilot -p "$PROMPT_CONTENT" --allow-all-tools --add-dir "$PARENT_FOLDER"
fi
EXIT_CODE=$?

echo "copilot exited with code: $EXIT_CODE"
exit $EXIT_CODE
