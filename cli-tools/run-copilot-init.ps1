<#
Runs the `copilot` CLI with a multi-line prompt to document a COBOL program using the COBOL Documentation Module.

Usage (PowerShell):
  .\run-copilot-init.ps1 -CobolPath "path/to/program.cob" -RepoName "repo-name"

Usage (cmd.exe):
  powershell -ExecutionPolicy Bypass -File "cli-tools\run-copilot-init.ps1" -CobolPath "path/to/program.cob" -RepoName "repo-name"
#>

# Accept COBOL program path and repository name to inject into the prompt
param(
  [string]$CobolPath,
  [string]$RepoName,
  [switch]$DryRun,
  # Safer default: do not allow all paths unless explicitly requested
  [switch]$AllowAllPaths = $false
  ,
  # When set, use the refresh instructions file instead of the init instructions
  [switch]$Refresh
)

# Load .env file if it exists
$envPath = Join-Path -Path $PSScriptRoot -ChildPath '.env'
if (Test-Path -Path $envPath) {
  Get-Content $envPath | ForEach-Object {
    $line = $_.Trim()
    # Skip comments and empty lines
    if ($line -and -not $line.StartsWith('#')) {
      $parts = $line -split '=', 2
      if ($parts.Count -eq 2) {
        $key = $parts[0].Trim()
        $value = $parts[1].Trim()
        # Only set if not already provided as parameter
        if ($key -eq 'COBOL_PROGRAM_PATH' -and -not $CobolPath -and $value) {
          $CobolPath = $value
        }
        if ($key -eq 'REPO_NAME' -and -not $RepoName -and $value) {
          $RepoName = $value
        }
      }
    }
  }
}

# Determine which instructions file to use (init vs refresh)
$instructionsFileName = 'instructions-init.md'
if ($Refresh) { $instructionsFileName = 'instructions-refresh.md' }
$instructionsPath = Join-Path -Path $PSScriptRoot -ChildPath $instructionsFileName
if (-not (Test-Path -Path $instructionsPath)) {
  Write-Host "ERROR: instructions file not found at $instructionsPath" -ForegroundColor Red
  exit 2
}

# Read all text from instructions.md (assume UTF8 or system default)
try {
  $prompt = Get-Content -Path $instructionsPath -Raw -ErrorAction Stop
  
  # Handle COBOL_PROGRAM_PATH placeholder
  $cobolPlaceholder = '<COBOL_PROGRAM_PATH>'
  if (-not ($prompt -match [regex]::Escape($cobolPlaceholder))) {
    Write-Host "ERROR: instructions.md must contain the placeholder $cobolPlaceholder. Please add it to the file and try again." -ForegroundColor Red
    exit 7
  }
  if (-not $CobolPath) {
    Write-Host "ERROR: instructions.md contains placeholder $cobolPlaceholder but no -CobolPath was provided. Please supply the COBOL program path with -CobolPath." -ForegroundColor Red
    exit 6
  }
  $prompt = $prompt -replace [regex]::Escape($cobolPlaceholder), $CobolPath
  
  # Handle REPO_NAME placeholder
  $repoPlaceholder = '<REPO_NAME>'
  if ($prompt -match [regex]::Escape($repoPlaceholder)) {
    if (-not $RepoName) {
      # Try to infer repo name from path if not provided
      if ($CobolPath -match 'temp-repos[/\\]([^/\\]+)[/\\]') {
        $RepoName = $Matches[1]
        Write-Host "INFO: Inferred repository name: $RepoName" -ForegroundColor Yellow
      } else {
        Write-Host "ERROR: instructions.md contains placeholder $repoPlaceholder but no -RepoName was provided. Please supply the repository name with -RepoName." -ForegroundColor Red
        exit 8
      }
    }
    $prompt = $prompt -replace [regex]::Escape($repoPlaceholder), $RepoName
  }
}
catch {
  Write-Host "Failed to read instructions file: $_" -ForegroundColor Red
  exit 3
}

# Show what will be executed
Write-Host "Running copilot with the following prompt:`n" -ForegroundColor Cyan
Write-Host $prompt -ForegroundColor Yellow
Write-Host "`nInvoking copilot...`n" -ForegroundColor Cyan

# Compute parent folder of where the script is executed (the parent of the current working directory)
$currentFolder = (Get-Location).ProviderPath
$parentFolder = Split-Path -Path $currentFolder -Parent

# Prepare the paths flag string for printing and invocation
$allowPathsArg = ''
if ($AllowAllPaths) { $allowPathsArg = '--allow-all-paths' }

if ($DryRun) {
  Write-Host "DRY RUN: resolved prompt (below)" -ForegroundColor Cyan
  Write-Host $prompt -ForegroundColor Yellow
  Write-Host "DRY RUN: command to be executed:" -ForegroundColor Cyan
  Write-Host "$copilotExe -p <$instructionsFileName> --allow-all-tools $allowPathsArg --add-dir `"$parentFolder`"" -ForegroundColor Green
  exit 0
}

# Preflight: ensure `copilot` is available on PATH and is an executable application
$copilotExe = 'copilot'
$copilotCmd = Get-Command $copilotExe -CommandType Application -ErrorAction SilentlyContinue
if (-not $copilotCmd) {
  Write-Host "ERROR: 'copilot' executable not found on PATH." -ForegroundColor Red
  Write-Host "Install the GitHub Copilot CLI and ensure it's on your PATH. Quick install (requires Node.js/npm):" -ForegroundColor Yellow
  Write-Host "  npm install -g @github/copilot" -ForegroundColor Cyan
  Write-Host "Docs: https://docs.github.com/copilot/concepts/agents/about-copilot-cli" -ForegroundColor Cyan
  Write-Host "Repo:  https://github.com/github/copilot-cli" -ForegroundColor Cyan
  exit 5
}


# Execute copilot - pass the prompt via argument
# Execute copilot directly so PowerShell resolves the executable and streams output
# Note: only include --allow-all-paths when explicitly requested

Write-Host "Running: $copilotExe -p <$instructionsFileName> --allow-all-tools $allowPathsArg --add-dir `"$parentFolder`"`n" -ForegroundColor Cyan

try {
  # Build and run the invocation dynamically so we only include the paths flag when desired
  $invokeArgs = @('--allow-all-tools')
  if ($AllowAllPaths) { $invokeArgs += '--allow-all-paths' }
  $invokeArgs += @('--add-dir', $parentFolder)
  # Prepend the prompt argument
  $invokeArgs = @('-p', $prompt) + $invokeArgs
  & $copilotExe @invokeArgs
  $exitCode = $LASTEXITCODE
} catch {
  Write-Host "Failed to invoke copilot: $_" -ForegroundColor Red
  exit 4
}

if ($null -eq $exitCode) { $exitCode = 0 }
Write-Host "copilot exited with code: $exitCode" -ForegroundColor Green
exit $exitCode
