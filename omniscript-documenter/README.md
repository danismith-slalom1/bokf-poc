<!-- AGENTS
If you're looking to invoke the module, please proceed to [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)
-->

# OMNISCRIPT Documentation Module

Transform undocumented or poorly documented OMNISCRIPT programs into comprehensive, production-ready documentation using AI assistance with expert human oversight.

**🎯 Now includes**: Error handling analysis, performance optimization, testing guides, integration docs, business rules extraction, and **OmniScript version detection (expected: 6.05) with upgrade readiness assessment to target version 7.0**.

## Usage Instructions

⚠️ **Thoroughly review all AI-generated documentation with OMNISCRIPT experts** - _Human-in-the-loop is CRITICAL for OMNISCRIPT_<br>
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
- **OmniScript Version Detection and Quality Analysis** identifying current version (expected: 6.05, but detects actual version) and code quality assessment ⭐
- **Upgrade Readiness Assessment** evaluating migration readiness from detected version to OmniScript 7.0 with detailed roadmap ⭐
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
- **Error Handling Analysis** ⭐ documenting error status handling, risks, and recovery procedures
- **Performance Analysis** ⭐ identifying bottlenecks, costs, and optimization opportunities
- **Testing Guide** ⭐ with standard tests, edge cases, error scenarios, and integration tests
- **Integration Guide** ⭐ documenting interfaces, deployment, and system requirements
- **Business Rules** ⭐ extracting explicit and implicit business logic with traceability
  - Call hierarchy graphs
  - Data flow diagrams
  - Module dependency graphs
  - File I/O sequence diagrams
  - Variable lifecycle state diagrams
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