<!-- AGENTS
If you're looking to invoke the module, please proceed to [INITIALIZER_PROMPT.md](./INITIALIZER_PROMPT.md)
-->

# COBOL Documentation Module

Transform undocumented or poorly documented COBOL programs into comprehensive, production-ready documentation using AI assistance with expert human oversight.

**🎯 Now includes**: Error handling analysis, performance optimization, testing guides, integration docs, and business rules extraction.

## Usage Instructions

⚠️ **Thoroughly review all AI-generated documentation with COBOL experts** - _Human-in-the-loop is CRITICAL for COBOL_<br>
✅ **Generate static analysis first** - _Cross-reference reports and call graphs are essential_<br>
✅ **Follow the iterative approach** - _Data dictionary → Paragraphs → Call graphs → Mutations → Synthesis_

> **Not sure if this module is right for your situation?** This module is specifically designed for documenting legacy COBOL programs using AI assistance with expert review.

#### **Send this prompt to your agent to begin**

   ```
   Use the `cobol-documentation` module (ai-initializer directory) to document this COBOL program. Follow the module's prompt (ai-initializer/INITIALIZER_PROMPT.md) thoroughly. I have [describe your static analysis setup: cross-reference reports, call graphs, or need help generating them].
   ```

##### Example

   ```
┌───────────────────────────────────────────────────────────┐
│ 📎 Add Context...                                         │
│                                                           │
│ Use cobol-documentation module (ai-initializer           |
| directory) to document PAYROLL-CALC.cob. Follow the      |
| module's prompt (ai-initializer/INITIALIZER_PROMPT.md).  |
| I have compiler cross-reference reports ready.           |
│                                                           │
│ Agent ▼   Claude Sonnet 4 ▼                     🛠️ 🎤 ▶️ ▼ │
└───────────────────────────────────────────────────────────┘
   ```

## Expected End State

After documenting a COBOL program, you will have:

### Core Documentation (Always Generated)
- **Program Analysis** documenting structure, divisions, dependencies, and chunking strategy
- **Data Dictionary** for all WORKING-STORAGE variables with purposes, usage patterns, and buffer limits
- **Paragraph Documentation** for each paragraph/section with business logic, error handling, and performance notes
- **Call Graph** showing all PERFORM relationships and control flow
- **Variable Mutation Analysis** tracking state changes across the program
- **Comprehensive Program Documentation** synthesizing all components with business rules and security
- **Cross-Reference Documentation** enabling quick navigation
- **Mermaid Visual Diagrams (MANDATORY)** including:
  - Program flow flowcharts
  - PERFORM hierarchy graphs
  - Data flow diagrams
  - Copybook dependencies
  - File I/O timelines
  - Variable lifecycle state machines

### Enhanced Documentation (Automatically Generated)
- **Error Handling Analysis** ⭐ documenting FILE STATUS, risks, and recovery procedures
- **Performance Analysis** ⭐ identifying bottlenecks, costs, and optimization opportunities
- **Testing Guide** ⭐ with standard tests, edge cases, error scenarios, and integration tests
- **Integration Guide** ⭐ documenting interfaces, deployment, and system requirements
- **Business Rules** ⭐ extracting explicit and implicit business logic with traceability
  - PERFORM hierarchy graphs
  - Data flow diagrams
  - Copybook dependency graphs
  - File I/O sequence diagrams
  - Variable lifecycle state diagrams
- **Maintenance Guide** for ongoing documentation updates

## Prerequisites

Before using this module, you should have:

1. **COBOL Source Code**: Access to the COBOL program(s) to be documented
2. **Static Analysis Tools** (highly recommended):
   - COBOL compiler with cross-reference generation capability
   - OR static anaCOBOL Documentation | Notes |
|---------|-----|---------------------|-------|
| **GitHub Copilot** | | | |
| | Claude Sonnet 4 | ⚠️ Experimental | Requires proper chunking and expert review |
| | GPT-4.1 | ⚠️ Experimental | May struggle with older COBOL dialects |
| **Claude** | | | |
| | Claude Sonnet 4 | ⚠️ Experimental | Best results with detailed prompts and context |
| **Cursor** | | | |
| | Claude Sonnet 4 | ❌ Not Tested | - |

**Legend:**
- ✅ Fully Tested - All functionality verified, high quality documentation
- ⚠️ Experimental - Basic functionality works, requires careful expert review
- ❌ Not Tested - No testing completed

**IMPORTANT**: Regardless of AI tool or testing status, **all AI-generated COBOL documentation MUST be reviewed by COBOL experts**. AI tools can misinterpret COBOL syntax, business logic, and data flows. Expert review is not optional.
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