# GitLab Dual-Repository Workflow Guide

## Overview

The omniscript-documenter uses a dual-repository workflow:
- **Source Repository** (GitLab): Contains OmniScript source code - URL varies per request
- **Documentation Repository** (GitLab): Contains generated documentation - fixed URL

## Prerequisites

### 1. GitLab SSH Key Setup

**Security Best Practice:** Use SSH keys for source repositories and tokens only for API operations.

#### SSH Key for Source Repositories (Read-Only)
Setup SSH key for cloning source code:
- **Purpose**: Clone and read OmniScript source repositories
- **Authentication**: SSH key pair (ed25519 recommended)
- **Permissions**: Read-only access to repositories

**Setup SSH Key:**
```bash
# 1. Generate SSH key (if you don't have one)
ssh-keygen -t ed25519 -C "your.email@company.com" -f ~/.ssh/gitlab_ed25519

# 2. Start ssh-agent and add key
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/gitlab_ed25519

# 3. Copy public key
cat ~/.ssh/gitlab_ed25519.pub

# 4. Add to GitLab:
#    - Go to GitLab → Preferences → SSH Keys
#    - Paste public key
#    - Set expiration date (optional)
#    - Save

# 5. Test connection
ssh -T git@gitlab.com
# Should see: "Welcome to GitLab, @username!"
```

#### Token for Documentation Repository (API Access)
Create a Personal Access Token for documentation:
- Scopes: `read_repository`, `write_repository`, `api`
- Name: `omniscript-docs-writer`
- Use for: API calls, pushing documentation, and creating merge requests

**Why SSH + Token?**
- SSH keys don't expire and are more secure for repository access
- No token required for source repository cloning (read-only)
- Token only needed for documentation repository API operations
- Principle of least privilege - different auth methods for different purposes

Store token securely in `.env` file:
```bash
# Copy example file
cp omniscript-documenter/.env.example omniscript-documenter/.env

# Edit .env file with your configuration:
GITLAB_DOCS_TOKEN=glpat-docs-yyyyyyyyyyyyy
DOCS_REPOSITORY_URL=https://gitlab.com/your-org/omniscript-docs.git
DOCS_PROJECT_ID=12345

# Load environment variables (if running manually)
source omniscript-documenter/.env
```

### 2. GitLab CLI (Optional but Recommended)

Install glab for easier merge request management:
```bash
# macOS
brew install glab

# Linux
sudo snap install glab

# Configure
glab auth login
```

## Workflow Steps

### Step 1: Configure Environment Variables

Create and edit `.env` file:

```bash
# Copy example configuration
cp omniscript-documenter/.env.example omniscript-documenter/.env

# Edit .env with your actual values:
# - GITLAB_DOCS_TOKEN: Token for API operations (api, read_repository, write_repository)
# - DOCS_REPOSITORY_URL: Fixed URL where all docs are published
# - DOCS_PROJECT_ID: GitLab project ID for API calls
# - DEFAULT_REVIEWERS: Comma-separated GitLab usernames
# - GIT_USER_NAME and GIT_USER_EMAIL: For commits

# Set up SSH key for git operations:
# ssh-keygen -t ed25519 -C "your.email@company.com" -f ~/.ssh/gitlab_ed25519
# ssh-add ~/.ssh/gitlab_ed25519
# Add public key to GitLab → Preferences → SSH Keys
```

See [`.env.example`](./.env.example) for all available configuration options.

### Step 2: Start Documentation with Source Repository URL

Provide the source repository URL when starting documentation:

```
Use omniscript-documenter to document PAYROLL-CALC.cbl from:
Source Repository: https://gitlab.com/finance/payroll-system.git
Program Path: src/omniscript/PAYROLL-CALC.cbl
Branch: main

Generated documentation should be pushed to the configured documentation repository.
```

### Step 3: AI Agent Executes Workflow

The AI agent will:
1. Clone source repository
2. Locate program file
3. Run parser and generate documentation
4. Clone documentation repository
5. Create branch: `docs/payroll-system/PAYROLL-CALC-2026-02-04`
6. Commit all documentation files
7. Push branch
8. Create merge request

### Step 4: Review Merge Request

Navigate to the merge request in GitLab:
1. Review generated documentation
2. Verify with OMNISCRIPT experts
3. Request changes if needed (AI can iterate)
4. Approve and merge when ready

## Common Commands

### Clone Source Repository
```bash
# Use SSH (requires SSH key configured in GitLab)
git clone git@gitlab.com:org/source-repo.git
```

### Create Documentation Branch
```bash
cd /path/to/omniscript-docs
git checkout -b docs/source-repo/PROGRAM-$(date +%Y-%m-%d)
```

### Push Documentation
```bash
git add ${OMNISCRIPT_DOCS_OUTPUT_DIR}/
git commit -m "docs(PROGRAM): Generate documentation"
git push -u origin docs/source-repo/PROGRAM-$(date +%Y-%m-%d)
```

### Create Merge Request
```bash
glab mr create \
  --title "Documentation: PROGRAM from source-repo" \
  --description "Auto-generated documentation" \
  --label "documentation,omniscript" \
  --assignee @reviewer
```

## Troubleshooting

### Authentication Failures
- **SSH Issues**:
  - Test SSH connection: `ssh -T git@gitlab.com`
  - Check if SSH key is loaded: `ssh-add -l`
  - Reload SSH key: `ssh-add ~/.ssh/gitlab_ed25519`
  - Verify SSH key is added to GitLab account
- **Token Issues**:
  - Verify `GITLAB_DOCS_TOKEN` is not expired
  - Check docs token has `read_repository`, `write_repository`, and `api` scopes
  - Ensure token is exported in environment

### Push Rejected
- Verify write access to documentation repository
- Check branch protection rules
- Ensure not pushing to protected branch directly

### Merge Request Creation Fails
- Verify API scope in token
- Check project permissions
- Verify branch exists remotely

## Security Best Practices

1. **Use separate tokens** for source and documentation repositories (principle of least privilege)
2. **Never commit tokens** to git - always use `.env` files (add to `.gitignore`)
3. **Use environment variables** for all token storage
4. **Rotate tokens regularly** (recommended: every 90 days)
5. **Use minimal scopes** required:
   - Source token: `read_repository` only
   - Docs token: `read_repository`, `write_repository`, `api`
6. **Enable 2FA** on GitLab account
7. **Audit token usage** periodically through GitLab admin panel
8. **Revoke unused tokens** immediately
9. **Use different tokens** per project/team when possible
10. **Monitor token activity** for suspicious access patterns

### Two-Token Security Benefits

| Aspect | Single Token | Two Tokens |
|--------|-------------|------------|
| **Source Access Risk** | Write access to source code | Read-only (safer) |
| **Blast Radius** | All repositories compromised | Only one repository type affected |
| **Revocation Impact** | Breaks all operations | Can revoke one, keep other working |
| **Scope Minimization** | Over-privileged by necessity | Each token has minimal required scope |
| **Audit Trail** | Single token for all actions | Separate audit trails per repository type |
| **Compliance** | May violate least privilege | Follows security best practices |

## Example End-to-End Workflow

```bash
# 1. Load environment configuration
source omniscript-documenter/.env

# 2. Clone source repository (using SOURCE_TOKEN with fallback to GITLAB_TOKEN)
SOURCE_TOKEN=${GITLAB_SOURCE_TOKEN:-${GITLAB_TOKEN}}
git clone https://oauth2:${SOURCE_TOKEN}@gitlab.com/finance/payroll-system.git /tmp/source-repo
cd /tmp/source-repo

# 3. Verify program exists
ls -la src/omniscript/PAYROLL-CALC.cbl

# 4. Run parser
python3 omniscript-documenter/omniscript_grammar_parser.py /tmp/source-repo/src/omniscript/PAYROLL-CALC.cbl

# 5. Clone documentation repository (using DOCS_TOKEN with fallback to GITLAB_TOKEN)
DOCS_TOKEN=${GITLAB_DOCS_TOKEN:-${GITLAB_TOKEN}}
git clone https://oauth2:${DOCS_TOKEN}@${DOCS_REPOSITORY_URL#https://} /tmp/omniscript-docs
cd /tmp/omniscript-docs

# 6. Create branch
SOURCE_REPO_NAME="payroll-system"
PROGRAM_NAME="PAYROLL-CALC"
BRANCH_NAME="docs/${SOURCE_REPO_NAME}/${PROGRAM_NAME}-$(date +%Y-%m-%d)"
git checkout -b ${BRANCH_NAME}

# 7. Configure git (using environment variables)
git config user.name "${GIT_USER_NAME}"
git config user.email "${GIT_USER_EMAIL}"

# 8. [AI generates documentation files...]

# 9. Commit and push
git add ${OMNISCRIPT_DOCS_OUTPUT_DIR}/${SOURCE_REPO_NAME}/${PROGRAM_NAME}/
git commit -m "docs(${PROGRAM_NAME}): Generate documentation from ${SOURCE_REPO_NAME}"
git push -u origin ${BRANCH_NAME}

# 10. Create merge request (using environment variables)
glab mr create \
  --source-branch ${BRANCH_NAME} \
  --target-branch ${DOCS_TARGET_BRANCH} \
  --title "Documentation: ${PROGRAM_NAME} from ${SOURCE_REPO_NAME}" \
  --description "Auto-generated documentation from omniscript-documenter" \
  --label "${MR_LABELS}" \
  --assignee ${DEFAULT_REVIEWERS}

# 11. Cleanup
cd /
rm -rf /tmp/source-repo /tmp/omniscript-docs
```

## Directory Structure

After completing documentation, the repositories look like:

```
Source Repository (variable URL):
payroll-system/
└── src/
    └── omniscript/
        └── PAYROLL-CALC.cbl

Documentation Repository (fixed URL):
omniscript-docs/
└── ${OMNISCRIPT_DOCS_OUTPUT_DIR}/  # Env var, default: omniscript-documentation
    └── payroll-system/
        └── PAYROLL-CALC/
            ├── PAYROLL-CALC_OVERVIEW.md
            ├── PAYROLL-CALC_DATA_DICTIONARY.md
            ├── PAYROLL-CALC_CALL_GRAPH.md
            ├── PAYROLL-CALC_DIAGRAMS.md
            ├── PAYROLL-CALC_ERROR_HANDLING.md
            ├── PAYROLL-CALC_INTEGRATION_GUIDE.md
            ├── PAYROLL-CALC_BUSINESS_RULES.md
            ├── PAYROLL-CALC_CROSS_REFERENCE.md
            ├── PAYROLL-CALC_VALIDATION_REPORT.md
            └── procedures/
                └── *.md
```

## Branch Management

### Branch Naming Convention
- Format: `docs/{source-repo-name}/{program-name}-{date}`
- Examples:
  - `docs/payroll-system/PAYROLL-CALC-2026-02-04`
  - `docs/billing-app/INVOICE-PROCESSOR-2026-02-04`
  - `docs/legacy-mainframe/CUSTOMER-UPDATE-2026-02-04`

### Branch Lifecycle
1. Created automatically by AI agent
2. All documentation committed to branch
3. Pushed to documentation repository
4. Merge request created
5. Expert review
6. Merged to main (or changes requested)
7. Branch can be deleted after merge (optional)

## Merge Request Template

When creating merge requests, use this description template:

```markdown
## Generated Documentation

**Source Repository**: https://gitlab.com/org/source-repo.git
**OmniScript Program**: PROGRAM-NAME
**Documentation Date**: 2026-02-04
**Parser Version**: 1.0

### Documentation Generated
- [x] Program Overview
- [x] Data Dictionary
- [x] Call Graph
- [x] Procedures Documentation
- [x] Diagrams
- [x] Error Handling Analysis
- [x] Integration Guide
- [x] Business Rules

### Review Checklist
- [ ] Business logic accuracy verified
- [ ] Variable purposes reviewed
- [ ] Call relationships validated
- [ ] Error handling documented
- [ ] Integration contracts complete
- [ ] Business rules extracted

**Reviewer**: Please verify with OmniScript experts before merging.

### Source Commit
- Branch: main
- Commit: abc123def456
- File: src/omniscript/PROGRAM-NAME.cbl
```

## FAQ

### Q: Can I use the same repository for source and documentation?
**A**: Not recommended. The dual-repository approach separates concerns and allows different access controls.

### Q: What if I don't have write access to a documentation repository?
**A**: You need write access to push documentation. Request access or use a repository where you have permissions.

### Q: Can I document multiple programs from the same source repository?
**A**: Yes! Each program gets its own subdirectory and its own merge request branch.

### Q: How do I handle authentication in CI/CD?
**A**: Use CI/CD secret variables for tokens. Never commit tokens to code.

### Q: Can I use SSH instead of tokens?
**A**: Yes! SSH keys work for both repositories. Add your public key to GitLab.

### Q: What if the merge request creation fails?
**A**: You can still push the branch and create the MR manually in GitLab UI.

## Related Documentation

- [CONFIG.md](./CONFIG.md) - Full configuration reference
- [WORKFLOW.md](./WORKFLOW.md) - Complete workflow process
- [README.md](./README.md) - Quick start guide
- [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md) - AI agent instructions
