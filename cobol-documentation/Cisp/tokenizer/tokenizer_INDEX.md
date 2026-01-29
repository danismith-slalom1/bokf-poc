# TOKENIZER Program - Documentation Index

## Program Overview
- **Program Name**: TOKENIZER
- **Repository**: Cisp (https://github.com/lauryndbrown/Cisp)
- **Author**: lauryn brown
- **Purpose**: Tokenize LISP input files for CISP interpreter
- **Documentation Date**: 2026-01-20

---

## Quick Navigation

### For New Developers (Start Here)
1. [Executive Summary](tokenizer_COMPREHENSIVE_DOC.md#executive-summary) - High-level overview
2. [Mermaid Diagrams](tokenizer_MERMAID_DIAGRAMS.md) - Visual program flow
3. [Main Procedure](paragraphs/MAIN-PROCEDURE.md) - Entry point documentation
4. [Data Dictionary](tokenizer_DATA_DICTIONARY.md) - Variable reference

### For Maintenance Developers
1. [Call Graph](tokenizer_CALL_GRAPH.md) - Procedure relationships
2. [Variable Mutations](tokenizer_VARIABLE_MUTATIONS.md) - State change tracking
3. [Maintenance Guide](tokenizer_COMPREHENSIVE_DOC.md#maintenance-guide) - Common tasks
4. [Individual Paragraphs](paragraphs/) - Detailed procedure docs

### For Business Analysts
1. [Business Context](tokenizer_COMPREHENSIVE_DOC.md#business-context) - Purpose and value
2. [Processing Flow](tokenizer_COMPREHENSIVE_DOC.md#processing-flow) - Step-by-step logic
3. [Limitations](tokenizer_COMPREHENSIVE_DOC.md#error-handling-and-limitations) - Known constraints

### For Auditors
1. [External Dependencies](tokenizer_COMPREHENSIVE_DOC.md#external-dependencies) - System integration
2. [Call Graph](tokenizer_CALL_GRAPH.md) - LOGGER audit points
3. [Error Handling](tokenizer_COMPREHENSIVE_DOC.md#error-handling-and-limitations) - Risk assessment

---

## Complete Documentation Catalog

### Core Documentation Files

| Document | Lines | Purpose | Audience |
|----------|-------|---------|----------|
| [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md) | 912+ | Complete program documentation | All |
| [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md) | 600+ | All variables with usage patterns | Developers |
| [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md) | 450+ | Procedure hierarchy and flow | Developers |
| [tokenizer_VARIABLE_MUTATIONS.md](tokenizer_VARIABLE_MUTATIONS.md) | 500+ | State change analysis | Developers |
| [tokenizer_MERMAID_DIAGRAMS.md](tokenizer_MERMAID_DIAGRAMS.md) | 450+ | Visual representations | All |
| [tokenizer_INDEX.md](tokenizer_INDEX.md) | This file | Navigation guide | All |

### Paragraph-Level Documentation

Located in [`paragraphs/`](paragraphs/) subdirectory:

| Paragraph | Lines in Source | Documentation | Purpose |
|-----------|----------------|---------------|---------|
| MAIN-PROCEDURE | 77-83 | [MAIN-PROCEDURE.md](paragraphs/MAIN-PROCEDURE.md) | Entry point orchestrator |
| FILE-HANDLING-PROCEDURE | 120-149 | [FILE-HANDLING-PROCEDURE.md](paragraphs/FILE-HANDLING-PROCEDURE.md) | Read LISP file |
| TOKENIZE-LISP-PROCEDURE | 150-169 | [TOKENIZE-LISP-PROCEDURE.md](paragraphs/TOKENIZE-LISP-PROCEDURE.md) | Split into tokens |
| FORMAT-LISP-PROCEDURE | 180-213 | [FORMAT-LISP-PROCEDURE.md](paragraphs/FORMAT-LISP-PROCEDURE.md) | Add paren spacing |
| CALC-LISP-LENGTH | 214-228 | [CALC-LISP-LENGTH.md](paragraphs/CALC-LISP-LENGTH.md) | Calculate string length |
| CAL-LENGTH-ALL-SYMBOLS | 84-87 | Documented in Call Graph | Loop wrapper |
| CALC-LENGTH-SYMBOL | 88-96 | Documented in Call Graph | Measure single token |
| APPEND-LISP-PROCEDURE | 97-119 | Documented in Call Graph | Concatenate lines |
| FORMAT-CHECK-PAREN-PROCEDURE | 233-244 | Documented in Call Graph | Check spacing needs |
| FORMAT-ADD-LEFT-SPACE | 246-254 | Documented in Call Graph | Insert left space |
| FORMAT-ADD-RIGHT-SPACE | 256-261 | Documented in Call Graph | Insert right space |
| FORMAT-ADD-BOTH-SPACES | 263-273 | Documented in Call Graph | Insert both spaces |
| FORMAT-PAREN-SPACE-PROCEDURE | 275-281 | Documented in Call Graph | Dispatch spacing logic |
| PRINT-SYMBOL-TABLE | 170-177 | Documented in Call Graph | Debug display |
| RESET-PARSE-FLAGS-PROCEDURE | 229-232 | Documented in Call Graph | Utility (unused) |
| PRINT-PARSE-FLAGS-PROCEDURE | Not shown | Documented in Call Graph | Utility (unused) |

---

## Program Structure Overview

### Divisions and Sections

#### IDENTIFICATION DIVISION
- Program ID: TOKENIZER
- Documentation: [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md#identification)

#### ENVIRONMENT DIVISION
- **INPUT-OUTPUT SECTION**:
  - FILE-CONTROL: LISP-FILE (dynamic assignment)
- Documentation: [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md#file-section-variables)

#### DATA DIVISION

**FILE SECTION** (Lines 16-18):
- FD LISP-FILE
- 01 IN-LISP-RECORD PIC X(200)
- Documentation: [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md#file-section-variables)

**WORKING-STORAGE SECTION** (Lines 19-62):
- 39 data items (including groups and subordinates)
- Documentation: [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md#working-storage-section-variables)

**LINKAGE SECTION** (Lines 63-72):
- LS-LISP-FILE-NAME (input)
- LS-SYMBOL-LENGTH (output - not set)
- LS-LISP-SYMBOLS structure (output)
- Documentation: [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md#linkage-section-variables)

#### PROCEDURE DIVISION (Lines 73-324)
- 16 paragraphs total
- 3 main processing procedures
- 10 supporting/utility procedures
- 3 debug/unused procedures
- Documentation: [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md)

---

## Key Algorithms and Patterns

### 1. Length Calculation with Trailing Space Elimination
- **Algorithm**: [CALC-LISP-LENGTH](paragraphs/CALC-LISP-LENGTH.md)
- **Pattern**: Delayed space counting
- **Diagram**: [Mermaid Algorithm Diagram](tokenizer_MERMAID_DIAGRAMS.md#7-variable-mutation-timeline)

### 2. Dynamic String Formatting
- **Algorithm**: [FORMAT-LISP-PROCEDURE](paragraphs/FORMAT-LISP-PROCEDURE.md)
- **Pattern**: Loop boundary extension
- **Diagram**: [Mermaid State Diagram](tokenizer_MERMAID_DIAGRAMS.md#5-formatting-algorithm-state-diagram)

### 3. Space-Delimited Tokenization
- **Algorithm**: [TOKENIZE-LISP-PROCEDURE](paragraphs/TOKENIZE-LISP-PROCEDURE.md)
- **Pattern**: UNSTRING with pointer
- **Diagram**: [Mermaid Token Extraction](tokenizer_MERMAID_DIAGRAMS.md#6-token-extraction-process)

---

## Static Analysis References

### Variable Cross-Reference
Complete cross-reference data available in [tokenizer_DATA_DICTIONARY.md](tokenizer_DATA_DICTIONARY.md):
- Where each variable is defined
- Where each variable is read
- Where each variable is modified
- Special handling (REDEFINES, OCCURS, etc.)

### PERFORM Hierarchy
Complete call graph available in [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md):
- All PERFORM relationships
- Nesting depth (up to 4 levels)
- Loop structures
- External calls

### File Operations Summary
File I/O operations documented in [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md#external-dependencies):
- OPEN INPUT LISP-FILE
- READ operations (multiple)
- CLOSE LISP-FILE
- No output files

### Conditional Logic
Decision points documented in:
- [Call Graph - Loop Structures](tokenizer_CALL_GRAPH.md#loop-structures)
- [Mermaid Program Flow](tokenizer_MERMAID_DIAGRAMS.md#1-program-flow-diagram)

---

## Data Flow Tracking

### Input → Processing → Output

**Input Sources**:
1. LISP-FILE (via LS-LISP-FILE-NAME parameter)
2. Configuration: Fixed array size (100), buffer limits (200)

**Processing Stages**:
1. File Reading → WS-IN-LISP-RECORD (concatenated)
2. Formatting → WS-IN-LISP-RECORD (spaced)
3. Tokenization → LS-SYMBOL array
4. Length Calculation → LS-SYMBOL-LEN array

**Output Targets**:
1. LS-SYMBOL(1...100) - Token strings
2. LS-SYMBOL-TABLE-SIZE - Token count
3. LS-SYMBOL-LEN(1...100) - Token lengths
4. LOGGER program - Audit records (3 calls)

**Detailed Flow**: [Mermaid Data Flow Diagram](tokenizer_MERMAID_DIAGRAMS.md#3-data-flow-diagram)

---

## Global Variable Mutation Summary

### Most Frequently Modified Variables

1. **WS-IN-LISP-RECORD** (4 procedures)
   - Documentation: [Variable Mutations - WS-IN-LISP-RECORD](tokenizer_VARIABLE_MUTATIONS.md#1-ws-in-lisp-record-modified-in-4-procedures)
   - Role: Central data buffer
   - Mutations: Initialize → Concatenate → Format → Consume

2. **WS-LISP-LENGTH** (6 procedures)
   - Documentation: [Variable Mutations - WS-LISP-LENGTH](tokenizer_VARIABLE_MUTATIONS.md#2-ws-lisp-length-modified-in-6-procedures)
   - Role: Length tracking and loop control
   - Mutations: Calculate → Accumulate → Extend dynamically

3. **WS-FORMAT-STR-INDEX** (8+ locations)
   - Documentation: [Variable Mutations - WS-FORMAT-STR-INDEX](tokenizer_VARIABLE_MUTATIONS.md#3-ws-format-str-index-modified-in-8-locations)
   - Role: Loop iteration control
   - Mutations: Initialize → Increment → Adjust → Restore

**Complete Analysis**: [tokenizer_VARIABLE_MUTATIONS.md](tokenizer_VARIABLE_MUTATIONS.md)

---

## Testing and Quality Assurance

### Test Suites
Comprehensive test cases documented in [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md#testing-recommendations):
- Unit tests (length calculation, formatting, tokenization, lengths)
- Integration tests (simple function, multi-line, edge cases)
- Stress tests (maximum tokens, overflow, extreme nesting)

### Quality Gates
- No automated validation currently implemented
- Recommended: Add unit test framework
- Recommended: Add integration tests with CISP caller

### Known Issues
Documented in [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md#error-handling-and-limitations):
- LS-SYMBOL-LENGTH parameter not populated
- String literals with spaces incorrectly tokenized
- Buffer overflow not detected
- No file error handling

---

## Maintenance and Support

### Common Maintenance Tasks
Step-by-step guides in [tokenizer_COMPREHENSIVE_DOC.md](tokenizer_COMPREHENSIVE_DOC.md#maintenance-guide):
1. Increase token capacity (>100)
2. Increase buffer size (>200 bytes)
3. Add error handling
4. Fix string literal tokenization

### Refactoring Recommendations
Priority list in [tokenizer_VARIABLE_MUTATIONS.md](tokenizer_VARIABLE_MUTATIONS.md#summary-and-recommendations):
1. Separate formatting concerns (multiple buffers)
2. Stabilize loop boundaries (constant limits)
3. Local index variables (avoid sharing)

### Technical Debt
Comprehensive list in [tokenizer_CALL_GRAPH.md](tokenizer_CALL_GRAPH.md#technical-debt):
- Unused procedures (RESET-PARSE-FLAGS, PRINT-PARSE-FLAGS)
- Unused variables (WS-PARSE-STR-CHAR, etc.)
- Debug statements (D directives)
- Magic numbers (100, 200, 2000)

---

## External Dependencies

### Required Programs
1. **LOGGER** - Audit logging subroutine
   - Documentation: [External Dependencies](tokenizer_COMPREHENSIVE_DOC.md#1-logger-program)
   - Called 3 times per execution
   - Parameters: WS-LOG-OPERATION-FLAG, WS-LOG-RECORD

### Required Files
1. **LISP Source File** - Input data
   - Documentation: [External Dependencies](tokenizer_COMPREHENSIVE_DOC.md#2-lisp-source-file)
   - Format: Line sequential text
   - Location: Specified by LS-LISP-FILE-NAME

### Calling Programs
1. **CISP** - Main LISP interpreter
   - Documentation: [External Dependencies](tokenizer_COMPREHENSIVE_DOC.md#3-calling-program-cisp)
   - Relationship: CISP → TOKENIZER (subroutine call)
   - Contract: 100-element array coordination

---

## Revision History

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-20 | 1.0 | Initial documentation generation | AI (Claude) |

---

## Documentation Standards Applied

This documentation follows the standards defined in the COBOL Documentation Workflow:
- **Data Dictionary First**: Variables documented before procedures
- **Iterative Approach**: Paragraphs → Call Graph → Mutations → Synthesis
- **Visual Diagrams**: Mermaid flowcharts, state diagrams, sequence diagrams (MANDATORY)
- **Cross-References**: Comprehensive linking between documents
- **Maintenance Focus**: Refactoring recommendations and known issues highlighted

---

## Contact and Support

For questions about this documentation:
- **Repository**: https://github.com/lauryndbrown/Cisp
- **Original Author**: lauryn brown
- **Documentation Generator**: AI-powered COBOL documentation system
- **Review Status**: ⚠️ AI-generated - Should be reviewed by COBOL experts

---

*This index provides navigation to all documentation artifacts for the TOKENIZER program. All documentation is AI-generated and should be reviewed by COBOL experts for accuracy. Last updated: 2026-01-20*
