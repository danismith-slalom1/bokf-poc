<!-- AGENTS
If you're looking to invoke the module, please proceed to [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)
-->

# OMNISCRIPT Documentation Module

Transform undocumented or poorly documented OMNISCRIPT programs into comprehensive, production-ready documentation using AI assistance with expert human oversight.

**🎯 Key Features**: 
- **Deterministic Grammar Parser** for accurate language analysis
- **Mandatory Mermaid Diagrams** for visual program understanding
- **Enhanced Documentation** including error handling, performance analysis, testing guides, integration docs, and business rules extraction

## Quick Start

### 1. Parse the OMNISCRIPT Program (MANDATORY FIRST STEP)

Run the grammar parser for deterministic analysis:

```bash
python3 omniscript-documenter/omniscript_grammar_parser.py path/to/program.cbl
```

This generates a `*_PARSER_CONTEXT.txt` file with structured information about variables, routines, database operations, and control flow. This makes documentation significantly more accurate.

See [GRAMMAR_PARSER.md](./GRAMMAR_PARSER.md) for complete parser documentation.

### 2. Begin Documentation

Send this prompt to your AI agent:

⚠️ **Thoroughly review all AI-generated documentation with OMNISCRIPT experts** - _Human-in-the-loop is CRITICAL for OMNISCRIPT_<br>
✅ **Use the grammar parser first** - _Provides deterministic analysis for more accurate documentation_<br>
✅ **Generate static analysis first** - _Cross-reference reports and call graphs are essential_<br>
✅ **Follow the iterative approach** - _Data dictionary → Procedures → Call graphs → Mutations → Synthesis_

> **Not sure if this module is right for your situation?** This module is specifically designed for documenting legacy OMNISCRIPT programs using AI assistance with expert review.

#### **Send this prompt to your agent to begin**

   ```
   Use the `omniscript-documentation` module (omniscript-documenter directory) to document this OMNISCRIPT program. Follow the module's prompt (omniscript-documenter/INITIALIZER_PROMPT.md) thoroughly. I have [describe your static analysis setup: cross-reference reports, call graphs, or need help generating them].
   ```

##### Example

   ```
┌───────────────────────────────────────────────────────────┐
│ 📎 Add Context...                                         │
│                                                           │
│ Use omniscript-documentation module (omniscript-         |
| documenter directory) to document PAYROLL-CALC.os.       |
| Follow the module's prompt (omniscript-documenter/       |
| INITIALIZER_PROMPT.md). I have static analysis reports.  |
│                                                           │
│ Agent ▼   Claude Sonnet 4 ▼                     🛠️ 🎤 ▶️ ▼ │
└───────────────────────────────────────────────────────────┘
   ```

## Expected End State

After documenting an OMNISCRIPT program, you will have:

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

1. **OMNISCRIPT Source Code**: Access to the OMNISCRIPT program(s) to be documented
2. **Static Analysis Tools** (highly recommended):
   - OMNISCRIPT interpreter with cross-reference generation capability
   - OR static analysis tools for procedure and variable analysis
3. **Python 3**: Required for the OMNISCRIPT grammar parser

**IMPORTANT**: All AI-generated OMNISCRIPT documentation MUST be reviewed by OMNISCRIPT experts. AI tools can misinterpret OMNISCRIPT syntax, business logic, and data flows. Expert review is not optional.