# AE Toolkit

A collection of tools for practicing AI-accelerated engineering in your projects. Each tool addresses specific aspects of setting up, maintaining, and optimizing AI development practices on your team.

### What this is
A foundational set of tools to help you get started with AI development, designed for easy adaptation to your specific project needs.

### What this is not
A one-size-fits-all solution or a tool you'll use unchanged forever. Expect to customize and extend it as your project evolves.

## How to use

**New to AE?** Start with the [Getting Started Guide](./getting-started/README.md) to understand which tools to use for your specific situation.

1. **Choose the right tool** from the Available Tools section below based on your needs
2. **Follow each tool's README** for specific usage instructions
3. **Adapt and extend** the outputs as needed for your workflow

> Start with the appropriate tool, adapt as you go, and customize for your specific context.

## Available Tools

### [Cobol Documenter](./cobol-documenter/README.md)
Document your existing COBOL project

## Prerequisites

**Required:** Any AI coding assistant (Claude, Copilot, Cursor, etc.)

CLI tools

This repository includes a set of CLI utilities (`cli-tools/`) that use the GitHub Copilot CLI to run AE-Toolkit workflows without an IDE. To use these tools you'll need:

- GitHub Copilot CLI installed and configured (requires a valid Copilot license)
- On Windows: PowerShell 6+ is required (PowerShell 7 LTS recommended)
 - Node.js v22 or higher and npm v10 or higher (required by the Copilot CLI)

See `cli-tools/README.md` for usage examples and the `run-copilot-init.ps1` helper script.

Useful links:

- Copilot CLI repo: https://github.com/github/copilot-cli
- Copilot CLI docs: https://docs.github.com/copilot/concepts/agents/about-copilot-cli

## Toolkit Structure

```
ae-toolkit/
├── cli-tools/                   # Command-line helpers that use Copilot CLI
├── cobol-documentation/         # Generated COBOL documentation artifacts (output directory)
├── cobol-documenter/           # Transform existing projects for AI development
├── context-refresher/           # Keep AI context documentation current
├── examples/                    # Example methodologies, projects, and best practice libraries
│   ├── agents/                  # Pre-built agent definitions (Claude Code/OpenCode)
│   ├── chat-modes/              # Custom chat modes (Copilot/Cursor)
│   ├── commands/                # Reusable command templates (all tools)
│   ├── methodology-templates/   # File-based methodology templates
│   ├── projects/                # Example projects demonstrating AI concepts
│   └── rules/                   # Curated rules library (all tools)
├── getting-started/             # Guide for choosing the right toolkit modules
├── interaction-analyzer/        # Diagnose and optimize AI-human interactions
├── rules-manager/               # Deploy modular rules across AI development tools
├── scratch-management-utilities/# Maintain context across multiple AI chat sessions
├── GLOSSARY.md                  # Key terminology
└── README.md                    # This file
```

For additional resources and terminology, see the [GLOSSARY.md](./GLOSSARY.md).
