# OmniScript Documentation Generation Toolkit

This repository contains tooling for automated AI-driven documentation of OmniScript programs.

## Key Directories

- `omniscript-documenter/` - Core documentation workflow and configuration
  - `WORKFLOW.md` - Complete 5-phase documentation process
  - `CONFIG.md` - Output structure, documentation priorities, and settings
  - `INITIALIZER_PROMPT.md` - Analysis guidelines and specialist prompt
  - `templates/` - Documentation templates (quality assessment, standards, analysis)

## Custom Commands

- `/project:omniscript-documentation-agent <repository-url>` - Clones a repository and orchestrates the full OmniScript documentation workflow for all OmniScript files found (*.os, *.omniscript, *.omni). See `.claude/commands/omniscript-documentation-agent.md`.
