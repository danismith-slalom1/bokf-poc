# COBOL Program Analysis Template

This template is **MANDATORY** for all AI agents performing Phase 1 COBOL program assessment. Following this template structure is **REQUIRED**. Deviating from this template structure will result in termination of the agent.

## Template Structure

Use this exact template structure when creating COBOL program analysis documents:

```markdown
# COBOL Program Analysis - [Program Name] - [Date]

## Program Identification

### Program Metadata
- **Program ID**: [Program name from IDENTIFICATION DIVISION]
- **Author**: [From AUTHOR paragraph if present]
- **Date Written**: [From DATE-WRITTEN if present]
- **Date Compiled**: [Most recent compilation date if available]
- **Source File**: [Full path to COBOL source file]
- **COBOL Dialect**: [COBOL-74, COBOL-85, Enterprise COBOL, etc.]

### Program Purpose
[Brief description of what this program does, based on comments or identification division]

## Program Structure Analysis

### Size Metrics
- **Total Lines**: [Total lines including comments]
- **Lines of Code**: [Executable COBOL statements]
- **Number of Paragraphs**: [Count of paragraphs in PROCEDURE DIVISION]
- **Number of Sections**: [Count of sections if any]

### Division Breakdown
- **IDENTIFICATION DIVISION**: Lines [X-Y]
- **ENVIRONMENT DIVISION**: Lines [X-Y] (if present)
- **DATA DIVISION**: Lines [X-Y]
  - FILE SECTION: Lines [X-Y] (if present)
  - WORKING-STORAGE SECTION: Lines [X-Y] (if present)
  - LINKAGE SECTION: Lines [X-Y] (if present)
- **PROCEDURE DIVISION**: Lines [X-Y]

## Data Analysis

### WORKING-STORAGE Structure
- **Total Variables**: [Count of 01-level items]
- **Data Groups**: [Count of major data structures]
- **Elementary Items**: [Count of lowest-level items]
- **REDEFINES Clauses**: [Count and complexity]
- **OCCURS Clauses**: [Count of arrays/tables]
- **Special Fields**: [COMP, COMP-3, binary fields, etc.]

### File Operations
- **Input Files**: [List file names and FD descriptions]
- **Output Files**: [List file names and FD descriptions]
- **File Organization**: [Sequential, Indexed, Relative]

### External Dependencies
- **COPY Books**: [List all COPY statements with member names]
- **Called Programs**: [List all CALL statements]
- **Called By**: [If known, what programs call this one]

## Procedure Division Analysis

### Program Flow Structure
- **Main Processing Sections**: [List major sections]
- **Initialization Paragraphs**: [Startup logic]
- **Main Loop Paragraphs**: [Core processing]
- **Cleanup Paragraphs**: [Termination logic]

### PERFORM Relationships
- **Entry Point**: [Main paragraph/section where execution begins]
- **PERFORM Depth**: [Maximum nesting level of PERFORM statements]
- **Loop Structures**: [PERFORM UNTIL, PERFORM VARYING counts]

### Conditional Logic
- **IF Statements**: [Approximate count]
- **EVALUATE Statements**: [Count if present]
- **Complex Conditions**: [Nested or multi-condition logic noted]

## Static Analysis Data

### Variable Cross-Reference
- **Cross-Reference Report**: [Path to generated cross-reference file]
- **High-Mutation Variables**: [Variables modified in 3+ locations]
- **Unused Variables**: [Variables defined but never referenced]
- **Read-Only Variables**: [Variables never modified]

### Call Graph Data
- **Call Graph Report**: [Path to generated call graph]
- **PERFORM Hierarchy Depth**: [Maximum call depth]
- **Recursive PERFORMS**: [Any recursive paragraph calls]

### Complexity Indicators
- **Cyclomatic Complexity**: [If available from static analysis]
- **Largest Paragraph**: [Paragraph name and line count]
- **Most Complex Paragraph**: [Based on conditional logic]

## Chunking Strategy

### Recommended Chunking Approach
[Small/Medium/Large program determination and chunking recommendation]

### Chunk Boundaries
1. **Chunk 1**: [Division/Section/Paragraph range] - Lines [X-Y]
   - Purpose: [What this chunk handles]
2. **Chunk 2**: [Division/Section/Paragraph range] - Lines [X-Y]
   - Purpose: [What this chunk handles]
3. [Continue for all planned chunks]

### Documentation Sequence
1. [First section to document and why]
2. [Second section to document and why]
3. [Continue in planned order]

## Complexity Assessment

### Overall Program Complexity
[Simple/Moderate/Complex with justification]

### Complexity Factors
- **Business Logic Complexity**: [Simple/Moderate/Complex]
- **Data Structure Complexity**: [Simple/Moderate/Complex]
- **Control Flow Complexity**: [Simple/Moderate/Complex]
- **Integration Complexity**: [Simple/Moderate/Complex]

### Documentation Depth Recommendation
[Minimal/Standard/Comprehensive documentation approach recommended]

## Known Issues and Considerations

### Code Quality Observations
[Any obvious issues: missing error handling, complex logic, technical debt]

### Documentation Challenges
[Anticipated difficulties in documenting this program]

### Expert Review Requirements
[Areas that will need close expert review]

## Summary

### Key Characteristics
[Top 3-5 most important aspects of this program]

### Critical Business Logic
[Identification of business-critical processing]

### Documentation Priority
[High/Medium/Low priority for documentation effort]

---
**IMPORTANT**: This analysis must be reviewed and confirmed by COBOL experts before proceeding to Phase 2.

**CRITICAL**: After expert confirms this analysis, the workflow MUST continue to Phase 2 (Iterative Documentation Generation). Do not proceed without explicit expert approval of this assessment.
```

## Critical Requirements

### Content Requirements
- **Program Identification**: Must capture all metadata from IDENTIFICATION DIVISION
- **Program Structure Analysis**: Must document all divisions with line numbers
- **Data Analysis**: Must inventory WORKING-STORAGE, files, and dependencies
- **Procedure Division Analysis**: Must map program flow and PERFORM relationships
- **Static Analysis Data**: Must reference generated cross-reference and call graphs
- **Chunking Strategy**: Must define clear chunk boundaries and documentation sequence

### Content Guidance
When creating the program analysis document, ensure you:
- **Provide accurate line numbers** for all divisions and major sections
- **Document all external dependencies** (COPY books, called programs, files)
- **Reference static analysis reports** that have been generated
- **Define clear chunking boundaries** appropriate for program size
- **Request expert verification** of analysis accuracy

### Format Requirements
- Use exact markdown structure as shown above
- Include the program name and date in the title
- Include all mandatory sections
- Include the footer note about expert confirmation requirement
- Maintain consistent section headers and formatting

### File Naming Convention
Save COBOL program analysis documents as:
`${COBOL_DOCS_DIR}/[PROGRAM-NAME]_ANALYSIS.md`

## Mandatory Compliance

This template structure is **NON-NEGOTIABLE**. Agents must:
1. Follow the exact section structure
2. Include all required sections
3. Maintain consistent formatting
4. Include the contextual footer note
5. Use the specified file naming convention

**Failure to comply with this template will result in agent termination.**

## Usage Notes

- This document should be placed in `${COBOL_DOCS_DIR}/`
- The program analysis is created in Phase 1.4
- COBOL experts must explicitly approve the analysis before proceeding to Phase 2
- This template enforces mandatory compliance for consistent documentation
