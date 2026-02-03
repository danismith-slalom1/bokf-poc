<!-- AGENTS
If you're looking to invoke the module, please proceed to [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)
-->

# OMNISCRIPT Documentation Module

Transform undocumented or poorly documented OMNISCRIPT programs into comprehensive, production-ready documentation using AI assistance with expert human oversight.

**🎯 Now includes**: 
- **Deterministic Grammar Parser** for accurate OmniScript language analysis
- **Comprehensive Code Quality Assessment** with security, operational risk analysis, and automated quality gates
- **OmniScript Version Detection** (expected: 6.05) with upgrade readiness assessment to target version 7.5
- Error handling analysis, performance optimization, testing guides, integration docs, business rules extraction, and detailed remediation roadmaps

## Quick Start

### 1. Parse the OMNISCRIPT Program (RECOMMENDED FIRST STEP)

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
- **Program Analysis** documenting structure, sections, dependencies, and chunking strategy
- **Data Dictionary** for all variables with purposes, usage patterns, and buffer limits
- **Procedure Documentation** for each procedure with business logic, error handling, and performance notes
- **Call Graph** showing all PERFORM relationships and control flow
- **Variable Mutation Analysis** tracking state changes across the program
- **Comprehensive Program Documentation** synthesizing all components with business rules and security
- **Cross-Reference Documentation** enabling quick navigation
- **Mermaid Visual Diagrams (MANDATORY)** including:
  - Program flow flowcharts
  - Call hierarchy graphs
  - Data flow diagrams
  - Module dependencies
  - File I/O timelines
  - Variable lifecycle state machines

### Enhanced Documentation (Automatically Generated)
<!-- EDGAR_CHANGE: UPDATED - Consolidated to reference CODE_QUALITY_ASSESSMENT template -->
- **Comprehensive Code Quality Assessment** ⭐ using standardized template (`templates/CODE_QUALITY_ASSESSMENT.template.md`):
  - **Section A: Error Handling Analysis** - Error status handling, runtime scenarios, resource limits, input validation, risk assessment
  - **Section B: Best Practices Assessment** - OmniScript/COBOL API usage, integration patterns, performance patterns, deprecated features
  - **Section C: Security Assessment** - 🔴 Critical/🟠 High/🟡 Medium security risks with exploit scenarios and remediation
  - **Section D: Operational Risk Assessment** - 🔴 Critical/🟠 High/🟡 Medium/🟢 Low operational risks with impact analysis
  - **Section E: Quality Scoring** - Overall metrics, per-procedure assessments, best practice violations, performance impact
  - **Section F: Quality Gate Checks** - ✅ PASS/⚠️ PASS WITH WARNINGS/❌ FAIL determination with deployment recommendations
  - Prioritized remediation roadmap with effort estimates
- **Performance Analysis** ⭐ identifying bottlenecks, costs, and optimization opportunities
- **Testing Guide** ⭐ with standard tests, edge cases, error scenarios, and integration tests
- **Integration Guide** ⭐ documenting interfaces, deployment, and system requirements
- **Business Rules** ⭐ extracting explicit and implicit business logic with traceability
- **Data Flow Diagrams** visualizing data transformations and state changes
- **Maintenance Guide** for ongoing documentation updates

## Prerequisites

Before using this module, you should have:

1. **OMNISCRIPT Source Code**: Access to the OMNISCRIPT program(s) to be documented
2. **Static Analysis Tools** (highly recommended):
   - OMNISCRIPT interpreter with cross-reference generation capability
   - OR static analysis tools for procedure and variable analysis

## Compatibility Matrix

| AI Platform | AI Model | Status | Notes |
|---------|-----|---------------------|-------|
| **GitHub Copilot** | | | |
| | Claude Sonnet 4 | ⚠️ Experimental | Requires proper chunking and expert review |
| | GPT-4.1 | ⚠️ Experimental | May struggle with complex OMNISCRIPT patterns |
| **Claude** | | | |
| | Claude Sonnet 4 | ⚠️ Experimental | Best results with detailed prompts and context |
| **Cursor** | | | |
| | Claude Sonnet 4 | ❌ Not Tested | - |

**Legend:**
- ✅ Fully Tested - All functionality verified, high quality documentation
- ⚠️ Experimental - Basic functionality works, requires careful expert review
- ❌ Not Tested - No testing completed

**IMPORTANT**: Regardless of AI tool or testing status, **all AI-generated OMNISCRIPT documentation MUST be reviewed by OMNISCRIPT experts**. AI tools can misinterpret OMNISCRIPT syntax, business logic, and data flows. Expert review is not optional.
| | o3-mini | ❌ Not Tested | ❌ Not Tested |
| | Gemini 2.5 Pro | ❌ Not Tested | ❌ Not Tested |
| | GPT-5 | ❌ Not Tested | ❌ Not Tested |
| **Cursor** | | | |
| | GPT-4.1 | ❌ Not Tested | ❌ Not Tested |
| | Claude Sonnet 4 | ❌ Not Tested | ❌ Not Tested |
| | o3-mini | ❌ Not Tested | ❌ Not Tested |
| | Gemini 2.5 Pro | ❌ Not Tested | ❌ Not Tested |
| | GPT-5 | ❌ Not Tested | ❌ Not Tested |
| **Claude** | | | |
| | GPT-4.1 | ❌ Not Tested | ❌ Not Tested |
| | Claude Sonnet 4 | ❌ Not Tested | ✅ Fully Tested |
| | o3-mini | ❌ Not Tested | ❌ Not Tested |
| | Gemini 2.5 Pro | ❌ Not Tested | ❌ Not Tested |
| | GPT-5 | ❌ Not Tested | ❌ Not Tested |
| **GPT-5** | | | |
| | GitHub Copilot | ❌ Not Tested | ❌ Not Tested |
| | Cursor | ❌ Not Tested | ❌ Not Tested |
| | Claude | ❌ Not Tested | ❌ Not Tested |

**Legend:**
- ✅ Fully Tested - All functionality verified to work correctly
- ⚠️ Partially Tested - Basic functionality tested, some edge cases or **known issues** may exist
- ❌ Not Tested - No testing completed on this platform/LLM combination, or **significant issues** were found