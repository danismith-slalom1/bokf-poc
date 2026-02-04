<!-- AGENTS
If you're looking to invoke the module, please proceed to [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)
-->

# OMNISCRIPT Documentation Module

Transform undocumented or poorly documented OMNISCRIPT programs into comprehensive, production-ready documentation using AI assistance with expert human oversight.

**🎯 Key Features**: 
- **Dual GitLab Repository Workflow** for source code and documentation separation
- **Deterministic Grammar Parser** for accurate language analysis
- **Mandatory Mermaid Diagrams** for visual program understanding
- **Enhanced Documentation** including error handling, performance analysis, testing guides, integration docs, and business rules extraction
- **Automated Merge Request Creation** with expert reviewer assignment

## Quick Start

### 1. Setup GitLab Authentication

Configure SSH key and documentation repository token:

```bash
# 1. Setup SSH key for source repositories
ssh-keygen -t ed25519 -C "your.email@company.com" -f ~/.ssh/gitlab_ed25519

# 2. Add SSH key to ssh-agent
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/gitlab_ed25519

# 3. Copy public key and add to GitLab (Preferences → SSH Keys)
cat ~/.ssh/gitlab_ed25519.pub

# 4. Test SSH connection
ssh -T git@gitlab.com

# 5. Configure documentation repository token
cp omniscript-documenter/.env.example omniscript-documenter/.env

# Edit .env with your actual values:
#   GITLAB_DOCS_TOKEN=glpat-yyyyy (with scopes: read_repository, write_repository, api)
#   DOCS_REPOSITORY_URL=https://gitlab.com/your-org/omniscript-docs.git
#   DOCS_PROJECT_ID=12345
```

**What to Configure**:

#### Source Repository (Variable - changes per request)
- URL: Provided when starting documentation (varies per request)
- Authentication: SSH key (configured in GitLab account)
- Contains: OmniScript source code files
- Format: `git@gitlab.com:org/project.git`

#### Documentation Repository (Fixed - configure once)
- URL: `DOCS_REPOSITORY_URL` in `.env` (same for all requests)
- Authentication: `GITLAB_DOCS_TOKEN` in `.env` (for API operations)
- Contains: All generated documentation

**Security Note**: SSH keys for source repositories (no token needed) and token only for documentation repository API operations follows the principle of least privilege.

See [GITLAB_WORKFLOW.md](./GITLAB_WORKFLOW.md) for complete setup instructions and [CONFIG.md](./CONFIG.md#gitlab-repository-configuration) for all available settings.

### 2. Parse the OMNISCRIPT Program (MANDATORY FIRST STEP)

### 2. Parse the OMNISCRIPT Program (MANDATORY)

Run the grammar parser for deterministic analysis:

```bash
python3 omniscript-documenter/omniscript_grammar_parser.py path/to/program.cbl
```

This generates a `*_PARSER_CONTEXT.txt` file with structured information about variables, routines, database operations, and control flow. This makes documentation significantly more accurate.

See [GRAMMAR_PARSER.md](./GRAMMAR_PARSER.md) for complete parser documentation.

### 3. Begin Documentation with Source Repository

Send this prompt to your AI agent:

⚠️ **Thoroughly review all AI-generated documentation with OMNISCRIPT experts** - _Human-in-the-loop is CRITICAL for OMNISCRIPT_<br>
✅ **Use the grammar parser first** - _Provides deterministic analysis for more accurate documentation_<br>
✅ **Configure GitLab repositories** - _Source repo URL varies, docs repo URL is fixed_<br>
✅ **Generate static analysis first** - _Cross-reference reports and call graphs are essential_<br>
✅ **Follow the iterative approach** - _Data dictionary → Procedures → Call graphs → Mutations → Synthesis_

> **Not sure if this module is right for your situation?** This module is specifically designed for documenting legacy OMNISCRIPT programs using AI assistance with expert review, with automatic GitLab integration.

#### **Send this prompt to your agent to begin**

   ```
   Use the `omniscript-documentation` module (omniscript-documenter directory) to document this OMNISCRIPT program. Follow the module's prompt (omniscript-documenter/INITIALIZER_PROMPT.md) thoroughly.
   
   Source Repository: https://gitlab.com/your-org/payroll-system.git
   Program Path: src/omniscript/PAYROLL-CALC.cbl
   Branch: main
   
   Documentation should be pushed to the configured documentation repository with a merge request for review.
   ```

##### Example

   ```
┌───────────────────────────────────────────────────────────┐
│ 📎 Add Context...                                         │
│                                                           │
│ Use omniscript-documentation module (omniscript-         │
│ documenter directory) to document PAYROLL-CALC.cbl.      │
│ Follow omniscript-documenter/INITIALIZER_PROMPT.md.      │
│                                                           │
│ Source: https://gitlab.com/finance/payroll-system.git    │
│ Program: src/omniscript/PAYROLL-CALC.cbl                 │
│ Branch: main                                             │
│                                                           │
│ Push docs to configured documentation repository.        │
│                                                           │
│ Agent ▼   Claude Sonnet 4 ▼                     🛠️ 🎤 ▶️ ▼ │
└───────────────────────────────────────────────────────────┘
   ```

## Workflow Overview

The module follows a 7-phase workflow:

1. **Phase 0: Repository Setup** - Clone source and documentation repositories
2. **Phase 1: Program Analysis** - Analyze structure and chunk strategically  
3. **Phase 2: Documentation Generation** - Create data dictionary, procedures, call graphs
4. **Phase 3: Validation** - AI self-validates and corrects documentation
5. **Phase 4: Synthesis** - Create comprehensive overview with diagrams
6. **Phase 5: Maintenance Guide** - Establish ongoing documentation practices
7. **Phase 6: Publishing** - Commit, push, and create merge request in GitLab

See [WORKFLOW.md](./WORKFLOW.md) for complete details.

## Expected End State

After documenting an OMNISCRIPT program, you will have:

### Repository Structure
Documentation is stored in the documentation repository (fixed URL):

```
omniscript-docs/  (Documentation GitLab repository)
└── ${OMNISCRIPT_DOCS_OUTPUT_DIR}/  # Env var, default: omniscript-documentation
    └── payroll-system/  (Source repository name)
        └── PAYROLL-CALC/  (Program name)
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

### GitLab Artifacts
- **Branch**: `docs/payroll-system/PAYROLL-CALC-2026-02-04`
- **Merge Request**: Created and assigned to OMNISCRIPT expert reviewers
- **Commits**: All documentation changes with source repository metadata
- **Labels**: `documentation`, `omniscript`, `automated`

### Core Documentation (Always Generated)
- **Program Overview** (consolidates index + comprehensive doc) with executive summary, architecture, embedded core diagrams, and navigation
- **Data Dictionary** for all variables with purposes, usage patterns, buffer limits, and **Variable Mutations section**
- **Procedure Documentation** for each procedure with business logic, error handling, and performance notes
- **Call Graph** showing all PERFORM relationships and control flow
- **Diagrams** with complex Mermaid visualizations:
  - Module dependencies
  - Detailed data flow with transformations
  - File I/O operation timelines
  - Variable lifecycle state machines
  
  See [MERMAID_GUIDE.md](./MERMAID_GUIDE.md) for complete diagram generation instructions.

### Enhanced Documentation (Automatically Generated)
Based on program criticality configured in CONFIG.md, the following additional documentation is generated:
- **Error Handling Analysis** - Error status handling, risks, and recovery procedures
- **Performance Analysis** - Bottlenecks, costs, and optimization opportunities
- **Testing Guide** - Standard tests, edge cases, error scenarios, and integration tests
- **Integration Guide** - Interfaces, deployment, and system requirements
- **Business Rules** - Explicit and implicit business logic with traceability
- **Maintenance Guide** - Ongoing documentation updates

For detailed configuration of enhanced documentation, see [CONFIG.md](./CONFIG.md#documentation-enhancement-settings).

## Prerequisites

Before using this module, you should have:

1. **GitLab Access**:
   - Source repository with OmniScript code (read access)
   - Documentation repository (write + API access for merge requests)
   - Personal Access Token configured
   
2. **OMNISCRIPT Source Code**: Access to the OMNISCRIPT program(s) to be documented

3. **Static Analysis Tools** (highly recommended):
   - OMNISCRIPT interpreter with cross-reference generation capability
   - OR static analysis tools for procedure and variable analysis
   
4. **Python 3**: Required for the OMNISCRIPT grammar parser

**IMPORTANT**: All AI-generated OMNISCRIPT documentation MUST be reviewed by OMNISCRIPT experts. AI tools can misinterpret OMNISCRIPT syntax, business logic, and data flows. Expert review is not optional - this is why merge requests are created automatically.

## Documentation

- **[CONFIG.md](./CONFIG.md)** - Configuration settings including GitLab repository setup
- **[WORKFLOW.md](./WORKFLOW.md)** - Complete 7-phase workflow process
- **[INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)** - Instructions for AI agents
- **[GITLAB_WORKFLOW.md](./GITLAB_WORKFLOW.md)** - Detailed GitLab integration guide
- **[GRAMMAR_PARSER.md](./GRAMMAR_PARSER.md)** - Parser documentation
- **[MERMAID_GUIDE.md](./MERMAID_GUIDE.md)** - Diagram generation guide
- **[templates/](./templates/)** - Documentation templates

## Troubleshooting

### GitLab Authentication Issues
- Verify tokens have correct scopes (read for source, write+API for docs)
- Check token expiration dates
- Ensure tokens are exported as environment variables

### Repository Clone Failures
- Verify repository URLs are correct
- Check network access to GitLab
- Confirm authentication credentials are valid

### Merge Request Creation Fails
- Verify API scope in documentation repository token
- Check project permissions in documentation repository
- Ensure branch exists before creating merge request

For more troubleshooting, see [GITLAB_WORKFLOW.md](./GITLAB_WORKFLOW.md#troubleshooting).

## Configuration Reference

All configuration is managed through the `.env` file:

| Variable | Purpose | Example |
|----------|---------|---------|
| `GITLAB_DOCS_TOKEN` | Token for API operations (merge requests) | `glpat-docs-yyyyy` |
| `DOCS_REPOSITORY_URL` | Fixed URL for all documentation | `https://gitlab.com/org/docs.git` |
| `DOCS_PROJECT_ID` | GitLab project ID for API calls | `12345` |
| `DOCS_TARGET_BRANCH` | Target branch for merge requests | `main` |
| `GIT_USER_NAME` | Git committer name | `OmniScript Bot` |
| `GIT_USER_EMAIL` | Git committer email | `bot@company.com` |
| `DEFAULT_REVIEWERS` | MR reviewers (comma-separated) | `@dev1,@dev2` |
| `MR_LABELS` | MR labels (comma-separated) | `docs,omniscript` |
| `WORK_DIR` | Base directory for temp files | `/tmp` |

**Authentication Method**: SSH keys handle all git operations (clone, push). The token is only used for GitLab API calls like creating merge requests.
See [`.env.example`](./.env.example) for complete configuration template.