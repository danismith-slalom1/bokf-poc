CLI-Tools overview

The CLI-Tools provide small utilities that let you document COBOL programs from the command line, without an IDE. They use the GitHub Copilot CLI to invoke the COBOL Documentation Module with AI assistance.

Prerequisites

- Node.js v22 or higher and npm v10 or higher (required by the Copilot CLI)
- GitHub Copilot CLI installed and configured (requires a valid Copilot license)
- On Windows: PowerShell v6 or higher (PowerShell 7 LTS recommended)

Installation and links

- GitHub Copilot CLI (public preview): https://github.com/github/copilot-cli
- Official docs: https://docs.github.com/copilot/concepts/agents/about-copilot-cli

Quick install (npm):

```powershell
npm install -g @github/copilot
```

Authentication

- On first run `copilot` will prompt you to login. You can also authenticate using a Personal Access Token (PAT) with the `Copilot Requests` permission and set it in the `GH_TOKEN` or `GITHUB_TOKEN` environment variable.
- Ensure your Copilot subscription or org policy allows Copilot CLI usage.

Quick start (recommended — interactive)

1. Ensure `copilot` is on your PATH and you can run `copilot --help`.
2. Preferred: run `copilot` interactively from PowerShell, Bash, or other shells and use the Copilot CLI interface to submit the instructions from `instructions-init.md`.

	Example (open Copilot in the current folder):

	```powershell
	copilot
	```

	Then paste or submit the contents of `cli-tools\instructions-init.md` into the Copilot prompt. Interactive use lets Copilot ask follow-up questions and receive human input — currently this produces more reliable results than running a single unattended command.

## Repository and Documentation Organization

**Repository Cloning Convention**:
- Clone COBOL repositories to `../temp-repos/` directory (at the same level as the one-call-finished project)
- Example: `/Users/username/projects/temp-repos/cobol-programming-course/`

**Documentation Output Convention**:
- All documentation is created in `cobol-documentation/{repo-name}/` within the one-call-finished project
- Example: `cobol-documentation/cobol-programming-course/CBL0009/CBL0009_DATA_DICTIONARY.md`
- Program-specific docs: `cobol-documentation/{repo-name}/{program-name}/`
- Repository-level docs: `cobol-documentation/{repo-name}/`

## Quick usage summary

| Flag | PowerShell | Bash | Description |
| --- | --- | --- | --- |
| COBOL Program Path | `-CobolPath '<path>'` | `<path>` (first positional arg) | Path to COBOL program to document. Required when instructions contain `<COBOL_PROGRAM_PATH>`. |
| Repository Name | `-RepoName '<name>'` | `<name>` (second positional arg) | Repository name for organizing documentation. Auto-inferred from path if in temp-repos/. |
| Dry run | `-DryRun` | `--dry-run` / `-n` | Preview the resolved prompt and the Copilot command without executing. |
| Allow all paths | `-AllowAllPaths` | `--allow-all-paths` | Opt-in: allow Copilot automatic filesystem access. Safer default is OFF. |
| Refresh mode | `-Refresh` | `--refresh` | Use `instructions-refresh.md` instead of `instructions-init.md` for updating existing documentation. |
| Add-dir scope | (automatic) | (automatic) | Scripts pass the parent folder to Copilot with `--add-dir` to scope filesystem access. |

All scripts enforce a strict placeholder contract: the instructions file MUST contain `<COBOL_PROGRAM_PATH>` and `<REPO_NAME>` placeholders. Use `-DryRun` first to verify.

Alternative: PowerShell helper script (automation, not preferred)

If you want to attempt automation, the helper script sends the full contents of `instructions-init.md` to Copilot non-interactively. Note: the script is NOT the preferred method today because Copilot may ask follow-up questions that require a human in the loop; the script does not currently provide interactive answers.

From PowerShell:

```powershell
.\run-copilot-init.ps1
```

From cmd.exe:

```cmd
powershell -ExecutionPolicy Bypass -File "cli-tools\run-copilot-init.ps1"
```

Bash (Linux/macOS)

If you're on Linux or macOS, there's a Bash helper as well. Make it executable and run it from the `cli-tools` folder or call it directly:

```bash
chmod +x ./run-copilot-init.sh
./run-copilot-init.sh
```

Or from anywhere:

```bash
bash cli-tools/run-copilot-init.sh
```

What the helper script does

- Reads `cli-tools\instructions-init.md` (or `cli-tools\instructions-refresh.md` when `--refresh`/`-Refresh` is used) and sends its contents as a prompt to `copilot -p "<prompt>" --allow-all-tools`.
- Replaces the `<COBOL_PROGRAM_PATH>` placeholder with the provided COBOL program path.
- Streams copilot's stdout/stderr to the console and returns the CLI exit code.

Customization

- If `copilot` is not on your PATH, edit the `$copilotExe` variable near the top of `run-copilot-init.ps1` to point to the full executable path.
- The prompt is sourced from `instructions-init.md` — edit that file to change what gets sent to Copilot.

Script usage (strict placeholder & dry-run)
- The scripts now enforce a strict placeholder contract: `cli-tools/instructions-init.md` MUST contain the placeholder `<COBOL_PROGRAM_PATH>`.
	- This prevents accidental runs that omit the COBOL program to documentinstructions-init.md` MUST contain the placeholder `<REPO_URL>`.
	- This prevents accidental runs that omit the repository to analyze.

- PowerShell example (required):

```powershell
.\cli-tools\run-copilot-init.ps1 -RepoUrl 'https://github.com/openai/codex' -DryRun
```

	- Use `-DryRun` to preview the resolved prompt and the exact `copilot` command without executing it.

-- Bash example (required):

```bash
./cli-tools/run-copilot-init.sh https://github.com/openai/codex --dry-run
```

Refresh mode examples

- PowerShell (use the refresh instructions file):

```powershell
.\cli-tools\run-copilot-init.ps1 -RepoUrl 'https://github.com/openai/codex' -DryRun -Refresh
```

- Bash (use the refresh instructions file):

```bash
./cli-tools/run-copilot-init.sh https://github.com/openai/codex --dry-run --refresh
```

Note: when using refresh mode, the scripts will read `cli-tools/instructions-refresh.md`. That file MUST also contain the `<REPO_URL>` placeholder (same strict placeholder contract as init mode).

Note: the Bash helper defaults to a safer mode where `--allow-all-paths` is disabled unless you explicitly opt in. To enable Copilot's automatic path access in Bash, add `--allow-all-paths`:

```bash
./cli-tools/run-copilot-init.sh https://github.com/openai/codex --allow-all-paths
```

- Exit codes of interest:
	- 0: success
	- 2: instructions-init.md not found
	- 3: failed to read instructions-init.md
	- 5: copilot not found on PATH
	- 6: placeholder present but no RepoUrl supplied
	- 7: placeholder missing from instructions-init.md

	Note about flags used when invoking Copilot

	- The helper scripts invoke Copilot with the following flags by default (behavior differs by platform):
		- `--allow-all-tools` — allows Copilot to call configured external tools (enabled by both helpers)
		- `--allow-all-paths` — allows Copilot to access file paths passed to it (we also pass `--add-dir` to scope access where possible)

		Platform differences:
		- PowerShell helper (`run-copilot-init.ps1`) now defaults to NOT including `--allow-all-paths`. To enable it, pass `-AllowAllPaths` when running the script.
		- Bash helper (`run-copilot-init.sh`) defaults to NOT including `--allow-all-paths`; add `--allow-all-paths` explicitly to enable it.

		Note: during local interactive testing you can omit `--allow-all-paths` (or avoid adding it in Bash, or omit `-AllowAllPaths` in PowerShell) if you plan to stay at your terminal and approve Copilot's prompts when it asks for path/tool access. Omitting `--allow-all-paths` reduces the agent's automatic filesystem access and requires explicit approval for each tool/path request.

		Example (PowerShell dry-run shows the command; remove `--allow-all-paths` when running interactively):

		```powershell
		.\cli-tools\run-copilot-init.ps1 -RepoUrl 'https://github.com/openai/codex' -DryRun
		# then run without allow-all-paths interactively (if you will answer approvals)
		.\cli-tools\run-copilot-init.ps1 -RepoUrl 'https://github.com/openai/codex'
		```

	If you'd like a less-strict transitional mode (for older `instructions-init.md` files), you can add a `--fallback` flag that re-enables the previous replace/prepend behavior. Otherwise the scripts will be strict by default.

	Security note

	These scripts pass `--allow-all-tools` and `--allow-all-paths` to the Copilot CLI. That grants the Copilot agent broader abilities (running tools and accessing file system paths you supply). Only run these scripts in trusted, isolated environments. Consider the following mitigations:

	- Run in a disposable or dedicated VM/container when possible.
	- Review `instructions-init.md` and the resolved prompt with `-DryRun` before execution.
	- Limit the `--add-dir` path to the minimum directory required for analysis.
	- Audit Copilot's actions during the run and avoid giving it credentials or secrets in the environment.