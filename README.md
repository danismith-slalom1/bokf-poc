# BOKF POC - OmniScript Documentation Automation

This project provides automated documentation generation for OmniScript programs with integrated GitLab workflow automation.

## Table of Contents

- [Overview](#overview)
- [What is OmniScript Documenter?](#what-is-omniscript-documenter)
- [Setup Instructions](#setup-instructions)
- [Usage](#usage)
- [Repository Structure](#repository-structure)

## Overview

This workspace contains tools to automatically generate comprehensive documentation for OmniScript programs and seamlessly integrate with GitLab through automated merge requests. The complete workflow includes cloning repositories, generating documentation, and creating merge requests - all fully automated.

## What is OmniScript Documenter?

The **OmniScript Documenter** is an intelligent documentation generation system that analyzes OmniScript programs and produces comprehensive technical documentation. It consists of a multi-phase workflow that:

### Key Features

- **Automated Analysis**: Parses OmniScript syntax and extracts program structure, business rules, and data flows
- **Comprehensive Documentation**: Generates multiple documentation artifacts including:
  - Data dictionaries with detailed field descriptions
  - Call graphs showing program dependencies
  - Flow diagrams and visual representations
  - Business rules and validation logic
  - Error handling documentation
  - Integration guides
- **Multi-Phase Workflow**: Follows a structured 5-phase process:
  1. **Initial Analysis** - Program structure and overview
  2. **Data Structure Documentation** - Variables, fields, and data types
  3. **Comprehensive Documentation** - Business logic and rules
  4. **Diagram Generation** - Visual call graphs and flowcharts
  5. **Index & Summary** - Cross-references and navigation
- **Grammar-Based Parsing**: Uses specialized grammar parser for accurate OmniScript syntax analysis
- **Template-Driven**: Follows standardized templates for consistent documentation output

### Documentation Output

For each OmniScript program, the documenter generates:

```
omniscript-documentation/
  santized/                              # or {REPO-NAME}/
    {PROGRAM-NAME}/
      {PROGRAM-NAME}_OVERVIEW.md           # Program overview and comprehensive documentation
      {PROGRAM-NAME}_DATA_DICTIONARY.md    # Variables, data structures, and mutations
      {PROGRAM-NAME}_CALL_GRAPH.md         # Program dependencies and call hierarchy
      {PROGRAM-NAME}_DIAGRAMS.md           # Visual flowcharts and Mermaid diagrams
      {PROGRAM-NAME}_ERROR_HANDLING.md     # Error handling analysis and risk assessment
      {PROGRAM-NAME}_INTEGRATION_GUIDE.md  # Integration, deployment, and operational guide
      {PROGRAM-NAME}_BUSINESS_RULES.md     # Business rules and validation logic
      {PROGRAM-NAME}_CROSS_REFERENCE.md    # Cross-reference of all program elements
      {PROGRAM-NAME}_VALIDATION_REPORT.md  # Documentation quality validation
      procedures/
        {PROCEDURE_NAME}.md                # Individual procedure documentation
```

The documenter is designed to handle large OmniScript codebases and can process multiple programs in a single execution, making it ideal for enterprise-scale documentation projects.

## Setup Instructions

### Step 1: Generate GitLab Access Token and Configure Environment

#### Part A: Create Project Access Token for Documentation Repository

Create a project access token for the documentation repository:

1. Navigate to your documentation repository in GitLab
   - Example: `https://gitlab.com/your-org/omniscript-documentation`

2. Go to **Settings** → **Access Tokens**

3. Create a new project access token:
   - **Token name**: "OmniScript Documentation Automation"
   - **Role**: Developer
   - **Scopes**: Select **ALL scopes**
   - **Expiration date**: Set as needed (or no expiration)

4. Click **Create project access token**

5. **Copy the generated token** - you'll need it in the next step

**Important Notes:**
- The role must be **Developer** to allow pushing branches and creating merge requests
- Select **all scopes** to ensure the token has sufficient permissions
- This token is specifically for the documentation repository where generated docs will be pushed
- Store the token securely - you won't be able to see it again after creation

#### Part B: Configure Environment Variables

Copy the `.env.example` file to `.env` in the root of the workspace:

```bash
# Copy example to create .env file
cp .env.example .env
```

Edit the `.env` file and add your credentials (using the token from Part A):

```bash
# GitLab Authentication
GITLAB_DOCS_TOKEN=your-docs-repo-token-here        # Token from Part A
GITLAB_USERNAME=your.username
GITLAB_EMAIL=your.email@company.com

# Note: SSH keys (configured in Step 2) are used for git clone/push operations
# No token needed for source repository access
```

**Alternative: Set Environment Variables in Your Shell**

You can also add these to your shell profile (`~/.zshrc` or `~/.bashrc`):

```bash
# GitLab API token for merge request creation
export GITLAB_DOCS_TOKEN="glpat-xxxxxxxxxxxxxxxxxxxx"
export GITLAB_USERNAME="your.username"
export GITLAB_EMAIL="your.email@company.com"

# Note: SSH keys handle git clone/push - no token needed for those operations
```

Then reload your shell:

```bash
source ~/.zshrc  # or source ~/.bashrc
```

### Step 2: SSH Key Setup for GitLab

Configure SSH key for GitLab access:

```bash
# 1. Generate SSH key (if you don't have one)
ssh-keygen -t ed25519 -C "your.email@company.com" -f ~/.ssh/gitlab_ed25519

# 2. Start ssh-agent and add your key
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/gitlab_ed25519

# 3. Copy public key to clipboard (macOS)
cat ~/.ssh/gitlab_ed25519.pub | pbcopy

# 4. Add public key to GitLab:
#    - Go to GitLab → Preferences → SSH Keys
#    - Paste your public key
#    - Give it a title (e.g., "Work Laptop")
#    - Click "Add key"

# 5. Test SSH connection
ssh -T git@gitlab.com
```

### Step 3: Git Configuration

Configure your Git identity:

```bash
git config --global user.email "your.email@company.com"
git config --global user.name "Your Name"
```

### Step 4: GitHub Copilot Setup

1. **Install GitHub Copilot Extension**
   - Open VS Code
   - Go to Extensions (⌘+Shift+X on macOS)
   - Search for "GitHub Copilot"
   - Install the extension
   - Sign in with your GitHub account

2. **Verify Copilot is Active**
   - Look for the Copilot icon in the status bar (bottom right)
   - Should show "GitHub Copilot: Active"

3. **Enable Copilot Chat**
   - Install "GitHub Copilot Chat" extension
   - Open Copilot Chat panel (Ctrl+Cmd+I on macOS)

### Step 5: Verify Workspace Setup

Ensure your workspace has the following structure:

```
bokf-poc/
  ├── .env                           # Your environment variables
  ├── README.md                      # This file
  ├── omniscript-documenter/         # Documentation engine
  └── .github/
      └── agents/
          └── omniscript-gitlab-automation-agent.md  # Automation agent
```

## Usage

### Using the OmniScript GitLab Automation Agent

The automation agent handles the complete workflow from repository cloning through merge request creation.

#### Basic Usage - Single Repository

```
@omniscript-gitlab-automation-agent https://gitlab.com/company/omniscript-programs
```

#### Processing Multiple Repositories

To document multiple OmniScript repositories, provide a list of GitLab repository URLs:

```
@omniscript-gitlab-automation-agent
https://gitlab.com/company/payroll-omniscripts
https://gitlab.com/company/benefits-omniscripts
https://gitlab.com/company/finance-omniscripts
https://gitlab.com/company/reporting-omniscripts
```

Or use a comma-separated list:

```
@omniscript-gitlab-automation-agent https://gitlab.com/company/repo1, https://gitlab.com/company/repo2, https://gitlab.com/company/repo3
```

### What the Agent Does Automatically

1. **Clones Repository** - Downloads the GitLab repository to `temp-repos/`
2. **Creates Feature Branch** - Creates a branch named `docs/omniscript-documentation-{timestamp}`
3. **Generates Documentation** - Processes all OmniScript files through the 5-phase workflow
4. **Commits Changes** - Stages and commits all generated documentation
5. **Pushes to GitLab** - Pushes the feature branch to the remote repository
6. **Creates Merge Request** - Opens an MR with comprehensive description and labels

### Example Output

```bash
@omniscript-gitlab-automation-agent https://gitlab.com/bokf/omniscript-programs

# Agent output:
✓ Cloned repository to temp-repos/omniscript-programs/
✓ Created branch: docs/omniscript-documentation-2026-02-04-143022
✓ Found 5 OmniScript files
✓ Generated documentation for PAYROLL.cbl (9 doc files + procedures)
✓ Generated documentation for BENEFITS.cbl (9 doc files + procedures)
✓ Generated documentation for TIMESHEET.cbl (9 doc files + procedures)
✓ Generated documentation for EXPENSES.cbl (9 doc files + procedures)
✓ Generated documentation for REPORTS.cbl (9 doc files + procedures)
✓ Committed 213 documentation files
✓ Pushed to origin
✓ Created merge request: https://gitlab.com/bokf/omniscript-programs/-/merge_requests/123
```

### Monitoring Progress

The agent provides real-time updates as it:
- Clones repositories
- Identifies OmniScript files
- Generates documentation for each program
- Commits and pushes changes
- Creates merge requests

All operations are logged and can be tracked in the Copilot Chat panel.

### Reviewing Generated Documentation

After the agent completes:

1. Click the merge request URL provided
2. Review the generated documentation in GitLab
3. Check the file diff to see all generated artifacts
4. Approve and merge the MR when ready

Documentation will be available in the `omniscript-documentation/` directory of the repository.

## Repository Structure

```
bokf-poc/
├── .env                                    # Environment variables (copy from .env.example)
├── .env.example                            # Environment variables template
├── README.md                               # This file
├── GLOSSARY.md                             # OmniScript terminology
├── omniscript-documenter/                  # Documentation engine
│   ├── WORKFLOW.md                         # 5-phase documentation process
│   ├── CONFIG.md                           # Configuration and output structure
│   ├── INITIALIZER_PROMPT.md              # Analysis guidelines
│   ├── GRAMMAR_PARSER.md                   # OmniScript syntax parser
│   ├── MERMAID_GUIDE.md                    # Diagram generation guide
│   ├── omniscript_grammar_parser.py        # Python grammar parser
│   └── templates/                          # Documentation templates
│       ├── DOCUMENTATION_PLAN.template.md
│       ├── DOCUMENTATION_STANDARDS.template.md
│       ├── INTERACTION_LOG.template.md
│       └── OMNISCRIPT_PROGRAM_ANALYSIS.template.md
├── omniscript-documentation/               # Generated documentation output
│   └── DOCUMENTATION_REPORT.md
├── cli-tools/                              # Command-line utilities
│   ├── run-copilot-init.sh
│   └── run-copilot-init.ps1
├── temp-repos/                             # Cloned repositories (temporary)
│   └── Cisp/                               # Example: COBOL Lisp interpreter
└── .github/
    └── agents/
        └── omniscript-gitlab-automation-agent.md  # GitLab automation agent

```

## Troubleshooting

### Common Issues

**"Authentication failed"**
- For SSH operations: Verify SSH key is added to GitLab and ssh-agent
  - Test: `ssh -T git@gitlab.com`
- For API operations: Check that `GITLAB_DOCS_TOKEN` has required scopes
  - Required scopes: `api`, `read_repository`, `write_repository`
  - Verify token hasn't expired
  - Test: `curl -H "Authorization: Bearer $GITLAB_DOCS_TOKEN" https://gitlab.com/api/v4/user`

**"Branch already exists"**
- The agent creates timestamped branches to avoid conflicts
- If you see this error, a previous run may have failed
- Delete the branch manually or use a different repository clone

**"No OmniScript files found"**
- Verify repository contains OmniScript files with extension `.cbl` (COBOL format)
- Files may also have extensions: `.os`, `.omniscript`, or `.omni`
- Check that files are in the repository root or subdirectories

## Additional Resources

- [OmniScript Documenter Workflow](omniscript-documenter/WORKFLOW.md) - Complete 5-phase process
- [Configuration Guide](omniscript-documenter/CONFIG.md) - Output structure and settings
- [Grammar Parser Documentation](omniscript-documenter/GRAMMAR_PARSER.md) - Syntax analysis details
- [Agent Configuration](.github/agents/omniscript-gitlab-automation-agent.md) - Full agent specification

## Support

For issues or questions:
1. Check the troubleshooting section above
2. Review the documentation in `omniscript-documenter/`
3. Examine the generated `DOCUMENTATION_REPORT.md` for processing details
4. Contact your team's OmniScript documentation lead

---

**Last Updated:** February 4, 2026
