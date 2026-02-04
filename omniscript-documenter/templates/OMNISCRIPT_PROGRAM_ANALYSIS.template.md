# OMNISCRIPT Program Analysis Template

This template is **MANDATORY** for all AI agents performing Phase 1 OMNISCRIPT program assessment. Following this template structure is **REQUIRED**. Deviating from this template structure will result in termination of the agent.

## Template Structure

Use this exact template structure when creating OMNISCRIPT program analysis documents:

```markdown
# OMNISCRIPT Program Analysis - [Program Name] - [Date]

## Program Identification

### Source Repository Information
- **Source Repository URL**: [GitLab repository URL where source code resides]
- **Repository Name**: [Extracted repository name]
- **Branch**: [Branch being documented]
- **Commit SHA**: [Specific commit being documented]
- **Clone Path**: [Path where repository was cloned]

### Program Metadata
- **Program ID**: [Program name]
- **Source File Path**: [Path within source repository]
- **Source File**: [Full path to OMNISCRIPT source file in clone]
- **Author**: [From program comments if present]
- **Date Written**: [From metadata if present]
- **Date Modified**: [Most recent modification date if available]
- **OMNISCRIPT Version**: [Interpreter version or dialect]
- **Runtime Environment**: [Interpreter characteristics, platform requirements]

### Documentation Repository Information
- **Documentation Repository URL**: [Fixed GitLab repository URL]
- **Documentation Branch**: [Branch created for this documentation]
- **Documentation Path**: `${OMNISCRIPT_DOCS_OUTPUT_DIR}/{SOURCE-REPO-NAME}/{PROGRAM-NAME}/` (env var, default: `omniscript-documentation`)

### Program Purpose
[Brief description of what this program does, based on comments or metadata]

## Program Structure Analysis

### Size Metrics
- **Total Lines**: [Total lines including comments]
- **Lines of Code**: [Executable OMNISCRIPT statements]
- **Number of Procedures**: [Count of procedures in program]
- **Number of Modules**: [Count of modules if any]

### Module Breakdown
- **Main Module**: Lines [X-Y]
- **Data Definitions**: Lines [X-Y] (if present)
- **Procedures/Functions**: Lines [X-Y]
  - Procedure/Function 1: Lines [X-Y]
  - Procedure/Function 2: Lines [X-Y]
  - [Continue for all procedures/functions]

## Data Analysis

### Variable Structure
- **Total Variables**: [Count of defined variables]
- **Data Structures**: [Count of major data structures]
- **Arrays**: [Count of array structures]
- **Constants**: [Count of constants]
- **Dynamic Variables**: [Variables declared at runtime, if applicable]

### File Operations
- **Input Files**: [List file names and descriptions]
- **Output Files**: [List file names and descriptions]
- **File Organization**: [Sequential, Random access, etc.]
- **Database Access**: [List database objects accessed if applicable]

### External Dependencies
- **Imported Modules**: [List all import statements]
- **Called Programs**: [List all external program calls]
- **Called By**: [If known, what programs call this one]

## Procedure Analysis

### Program Flow Structure
- **Main Processing Procedures**: [List major procedures/functions]
- **Initialization Procedures**: [Startup logic]
- **Main Loop Procedures**: [Core processing]
- **Cleanup Procedures**: [Termination logic]

### Call Relationships
- **Entry Point**: [Main procedure/function where execution begins]
- **Call Depth**: [Maximum nesting level of calls]
- **Loop Structures**: [Loop constructs count]
- **Recursion**: [Any recursive function calls]

### Conditional Logic
- **IF Statements**: [Approximate count]
- **CASE/SWITCH Statements**: [Count if present]
- **WHILE/FOR Loops**: [Count loop constructs]
- **Complex Conditions**: [Nested or multi-condition logic noted]

## Static Analysis Data

### Variable Cross-Reference
- **Cross-Reference Report**: [Path to generated cross-reference file]
- **High-Mutation Variables**: [Variables modified in 3+ locations]
- **Unused Variables**: [Variables defined but never referenced]
- **Read-Only Variables**: [Variables never modified]

### Call Graph Data
- **Call Graph Report**: [Path to generated call graph]
- **Call Hierarchy Depth**: [Maximum call depth]
- **Recursive Calls**: [Any recursive procedure/function calls]
- **Cross-Module Calls**: [External program or module invocations]

### Complexity Indicators
- **Cyclomatic Complexity**: [If available from static analysis]
- **Largest Procedure**: [Procedure/function name and line count]
- **Most Complex Procedure**: [Based on conditional logic]
- **String Operations Count**: [Heavy string manipulation noted]
- **I/O Operations Count**: [File and database operations]

## Chunking Strategy

### Recommended Chunking Approach
[Small/Medium/Large program determination and chunking recommendation]

### Chunk Boundaries
1. **Chunk 1**: [Module/Procedure range] - Lines [X-Y]
   - Purpose: [What this chunk handles]
2. **Chunk 2**: [Module/Procedure range] - Lines [X-Y]
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
[Any obvious issues: missing error handling, complex logic, technical debt, string manipulation risks]

### Error Handling Maturity
- **Error Checking Present**: [Yes/No/Partial]
- **File Operation Error Handling**: [Comprehensive/Limited/None]
- **Database Error Handling**: [Comprehensive/Limited/None]
- **Input Validation**: [Present/Absent]

### Performance Considerations
- **String Operations**: [Heavy/Moderate/Light usage]
- **Loop Iterations**: [Estimated counts for major loops]
- **I/O Frequency**: [High/Medium/Low]
- **Memory Usage Pattern**: [Large data structures noted]

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
**IMPORTANT**: This analysis must be reviewed and confirmed by OMNISCRIPT experts before proceeding to Phase 2.

**CRITICAL**: After expert confirms this analysis, the workflow MUST continue to Phase 2 (Iterative Documentation Generation). Do not proceed without explicit expert approval of this assessment.
```

## Critical Requirements

### Content Requirements
- **Program Identification**: Must capture all metadata from program header
- **Program Structure Analysis**: Must document all modules with line numbers
- **Data Analysis**: Must inventory variables, files, and dependencies
- **Procedure Analysis**: Must map program flow and call relationships
- **Static Analysis Data**: Must reference generated cross-reference and call graphs
- **Chunking Strategy**: Must define clear chunk boundaries and documentation sequence

### Content Guidance
When creating the program analysis document, ensure you:
- **Provide accurate line numbers** for all modules and major sections
- **Document all external dependencies** (imported modules, called programs, files)
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
Save OMNISCRIPT program analysis documents as:
`${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_ANALYSIS.md`

Where `${OMNISCRIPT_DOCS_DIR}` refers to `${OMNISCRIPT_DOCS_OUTPUT_DIR}/{REPO-NAME}/{PROGRAM-NAME}/` (env var, default: `omniscript-documentation`)

## Mandatory Compliance

This template structure is **NON-NEGOTIABLE**. Agents must:
1. Follow the exact section structure
2. Include all required sections
3. Maintain consistent formatting
4. Include the contextual footer note
5. Use the specified file naming convention

**Failure to comply with this template will result in agent termination.**

## Usage Notes

- This document should be placed in `${OMNISCRIPT_DOCS_DIR}/` where `${OMNISCRIPT_DOCS_DIR}` = `${OMNISCRIPT_DOCS_OUTPUT_DIR}/{REPO-NAME}/{PROGRAM-NAME}/` (env var, default: `omniscript-documentation`)
- The program analysis is created in Phase 1.4
- OMNISCRIPT experts must explicitly approve the analysis before proceeding to Phase 2
- This template enforces mandatory compliance for consistent documentation
- **Example path**: `${OMNISCRIPT_DOCS_OUTPUT_DIR}/my-repo/PAYROLL/PAYROLL_ANALYSIS.md` (typically: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_ANALYSIS.md`)
