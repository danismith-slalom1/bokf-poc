# OMNISCRIPT Documentation Workflow

This document describes the step-by-step process for using AI to document OMNISCRIPT programs effectively, following industry best practices for legacy OMNISCRIPT codebase documentation.

## Overview

The OMNISCRIPT Documentation Workflow transforms undocumented or poorly documented OMNISCRIPT programs into well-documented, maintainable codebases through a fully automated process by:
1. Analyzing and chunking OMNISCRIPT program structure
2. Performing static analysis to understand variable usage and program flow
3. Iteratively documenting each component with AI-driven analysis
4. Synthesizing comprehensive documentation with call graphs and cross-references
5. Generating complete, production-ready documentation without requiring clarification

## 5-Phase Workflow

Execute these phases sequentially for each OmniScript program:

1. **Phase 1: Program Analysis and Chunking**
   - Understand program structure, identify procedures and dependencies
   - Break large programs into manageable sections
   - Perform static analysis and create program index

2. **Phase 2: Iterative Documentation Generation**
   - Generate data dictionary (FIRST priority)
   - Document each procedure individually
   - Create call graphs
   - Identify global variable mutation patterns

3. **Phase 3: Automated Validation and Self-Correction**
   - AI validates its own documentation for accuracy
   - Self-correct inconsistencies and resolve ambiguities
   - Apply documentation standards consistently

4. **Phase 4: Synthesis and Comprehensive Documentation**
   - Create master program documentation
   - Generate cross-reference documentation
   - Document integration contracts and business rules
   - Create data flow diagrams and Mermaid visualizations (MANDATORY)

5. **Phase 5: Ongoing Maintenance Process**
   - Create documentation maintenance guide
   - Establish documentation repository
   - Define metrics and quality gates

**Key Principles**:
- Use absolute paths for all file operations
- Work autonomously without user prompts
- Follow output structure defined in CONFIG.md
- Complete all phases before moving to next program

## Documentation Directory

**CRITICAL**: Throughout this workflow, `${OMNISCRIPT_DOCS_DIR}` refers to the `omniscript-documentation/{REPO-NAME}/{PROGRAM-NAME}/` directory structure at the project root. All program-specific documentation artifacts MUST be placed within the program's subdirectory under its repository folder, never at the project root level.

**For complete directory structure and file naming conventions**, see [CONFIG.md](./CONFIG.md#output-directory-structure).

## Phase 1: Program Analysis and Chunking

### 1.1 OMNISCRIPT Program Structure Analysis
- **Objective**: Understand the OMNISCRIPT program's organization, procedures, and logical boundaries
- **Actions**:
  - Identify program metadata (program name, purpose, main entry point)
  - Analyze procedure structure (main procedures, sub-procedures, functions)
  - Catalog data structures (variables, constants, arrays)
  - Map procedure hierarchy (calls, dependencies)
  - Assess program size and complexity (lines of code, number of procedures)
  - Identify external dependencies (imported modules, called programs, file I/O)

### 1.2 Chunking Strategy Implementation
- **Objective**: Break large programs into logical, manageable sections for documentation
- **Chunking Approach**:
  - **By Module**: Document each major module separately
  - **By Procedure**: For large programs, document each procedure independently
  - **By Logical Function**: Group related procedures by business function
  - **By Data Group**: Group related data structures by purpose
  - **By File Processing**: Document file handling logic as discrete units
  
**CRITICAL**: Large monolithic OMNISCRIPT programs (>1000 lines) must be chunked to avoid overwhelming the AI context window and to enable focused documentation.

### 1.3 Static Analysis Preparation
- **Objective**: Generate cross-reference data and variable usage patterns before AI documentation
- **Actions**:
  - **Variable Cross-Reference**: Create reports showing where each variable is defined, read, and modified
  - **Call Hierarchy**: Map all procedure calls to create call graph
  - **File Operations**: Identify all READ, WRITE, and file manipulation operations
  - **Conditional Logic**: Catalog all IF, CASE, and conditional statements
  - **Data Flow**: Track data transformations through the program
  - **Global Variable Mutations**: Identify variables modified in multiple locations

**Recommended Tools**:
- OMNISCRIPT interpreters with cross-reference capabilities
- Static analysis tools for procedure and variable analysis
- Custom scripts to parse OMNISCRIPT source and generate usage reports

### 1.4 Create Program Overview Shell and Documentation Plan
- **Objective**: Generate master overview shell linking all program sections for holistic documentation
- **Actions**:
  - **Create master overview document shell**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_OVERVIEW.md`
  - List all modules, procedures with line numbers
  - Document identified chunks and their logical boundaries
  - Reference static analysis reports for each section
  - Create documentation sequence plan (order in which sections will be documented)
  
**Overview Shell Structure**:
```markdown
# [PROGRAM-NAME] Program Overview

## Executive Summary
- Name: [Program ID]
- Purpose: [Brief description]
- Business Value: [Why this program matters]

## Program Structure Index
1. Main Module
   - Main Procedure (Lines XX-YY) → [Link to documentation]
   - Helper Procedures (Lines AA-BB) → [Link to documentation]
2. Data Module
   - Data Structures (Lines CC-DD) → [Link to documentation]

## Static Analysis References
- Variable Cross-Reference: [Link to report]
- Call Hierarchy: [Link to call graph]
- File Operations Summary: [Link to I/O report]
```

## Phase 2: Iterative Documentation Generation

### 2.1 Generate Data Dictionary
**First Documentation Priority**: Document all data structures before procedural code

**Process**:
1. **Extract variable declarations**: Copy all variable, constant, and data structure declarations
2. **Feed to AI with static analysis context**: Include variable cross-reference report
3. **Generate data dictionary entries** for each data item:
   - Variable name
   - Data type and size
   - Purpose and usage description
   - Initial value
   - Where read/modified (from cross-reference)
   - Special handling notes (arrays, structures, constants)

4. **Document related data groups**: Group related fields (e.g., customer records, configuration parameters)
5. **Document error handling variables**: Identify error codes, status flags, return values
6. **Document buffer sizes and limits**: Note maximum sizes, overflow risks, validation requirements
7. **Create data dictionary document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_DATA_DICTIONARY.md`
8. **Prepare Variable Mutations section**: Add placeholder section for Phase 2.4 analysis

**AI Prompt Template**:
```
Given this OMNISCRIPT program's variable declarations and the variable cross-reference report,
generate a comprehensive data dictionary entry for each data item. Include:
- Variable purpose and meaning
- Data type interpretation
- Usage patterns based on cross-reference
- Relationships to other variables
- Business logic significance
- Buffer size limits and overflow risks
- Error handling and validation requirements

[Insert variable declarations]
[Insert cross-reference report]
```

### 2.2 Document Each Procedure Individually
**Second Documentation Priority**: Document procedural logic one chunk at a time

**Process for Each Procedure**:
1. **Select next procedure** from documentation plan sequence
2. **Gather context for AI**:
   - Procedure source code
   - Data dictionary entries for variables used
   - Call hierarchy showing callers and callees
   - Related static analysis (conditional logic, I/O operations)
3. **Generate procedure documentation**:
   - Purpose and function of the procedure
   - Input requirements (expected state, parameters)
   - Processing steps with business logic explanation
   - Output and side effects (variables modified, files updated)
   - Error handling mechanisms and recovery procedures
   - Performance characteristics (loops, string operations, I/O)
   - Edge cases and boundary conditions
   - Security considerations (input validation, resource limits)
   - Error handling and edge cases
   - Dependencies on other procedures
4. **Create individual procedure document**: `${OMNISCRIPT_DOCS_DIR}/procedures/[PROCEDURE-NAME].md`

**AI Prompt Template**:
```
Document this OMNISCRIPT procedure with the following context:
- Data dictionary for variables used: [link]
- Call hierarchy: [link to callers/callees]
- Static analysis: [relevant analysis]

Explain:
1. What this procedure does (business purpose)
2. Step-by-step processing logic
3. Variables it reads and modifies
4. How it fits into the overall program flow
5. Any error conditions or special handling

[Insert procedure source code]
```

**Chunking Guidance**:
- Document 1-3 related procedures per AI interaction
- Keep context window manageable (<4000 tokens per request)
- Document called procedures before calling procedures when possible

### 2.3 Error Handling Overview

**Objective**: Identify error handling patterns during procedure documentation for detailed assessment.

**Quick Error Handling Check During Documentation**:
As you document each procedure, make note of:
- Presence or absence of error handling (try-catch, status checks, error flags)
- File operations that may fail (OPEN, READ, WRITE)
- External calls without return code validation
- Input validation gaps
- Resource limits and potential overflow conditions

**Forward Reference**:
For comprehensive error handling analysis, security assessment, and operational risk evaluation, refer to the **CODE_QUALITY_ASSESSMENT** template (templates/CODE_QUALITY_ASSESSMENT.template.md). This comprehensive template covers:

- **Section A: Error Handling Analysis** - Detailed error handling mechanisms, runtime scenarios, resource limits, input validation, and risk assessment
- **Section C: Security Assessment** - Security vulnerabilities and safety risks
- **Section D: Operational Risk Assessment** - Operational risks and impact analysis

**When to Perform Comprehensive Assessment**:
- After completing all procedure documentation (Phase 4)
- Before production deployment
- For mission-critical or high-complexity programs

**Note**: Brief error handling observations should be included in individual procedure documentation. The comprehensive quality assessment provides enterprise-level risk analysis and quality gates.

### 2.4 Create Call Graphs (Call Relationships)

**Third Documentation Priority**: Map program control flow

**Process**:
1. **Generate call hierarchy tree** from static analysis
2. **Feed to AI with procedure documentation** already created
3. **Create visual call graph** showing:
   - Entry points (main program flow)
   - Call chains and nesting
   - Conditional call paths
   - Loop structures
   - Exit points and termination conditions

4. **Create call graph document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_CALL_GRAPH.md`

**Call Graph Format**:
```markdown
# [PROGRAM-NAME] Call Graph

## Main Program Flow
1. MAIN-PROCESSING
   - CALLS INITIALIZATION
     - CALLS OPEN-FILES
     - CALLS LOAD-PARAMETERS
   - CALLS PROCESS-RECORDS (loop until end-of-file)
     - CALLS READ-INPUT-FILE
     - CALLS VALIDATE-RECORD
       - CALLS CHECK-REQUIRED-FIELDS
       - CALLS VALIDATE-AMOUNTS
     - CALLS WRITE-OUTPUT-FILE
   - CALLS CLEANUP
     - CALLS CLOSE-FILES
     - CALLS WRITE-SUMMARY

## Loop Structures
- PROCESS-RECORDS: Loop until end-of-file flag set
- ARRAY-PROCESSING: Loop over array elements
```

### 2.5 Identify Global Variable Mutation Patterns
**Fourth Documentation Priority**: Track state changes across the program

**Process**:
1. **Extract global variable list** from cross-reference report (variables modified in 3+ locations)
2. **Feed to AI with full context**:
   - Variable definitions from data dictionary
   - All procedures that modify each variable
   - Call graph showing execution order
   
3. **Document mutation patterns**:
   - Variable initialization location
   - All modification points in execution order
   - State transitions and business logic
   - Potential race conditions or unexpected mutations
   - Recommendations for refactoring if needed

4. **Add to data dictionary**: Append Variable Mutations section to `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_DATA_DICTIONARY.md`

**AI Prompt Template**:
```
Analyze the mutation patterns for these global variables that are modified in multiple locations:

For each variable:
- Where it's initialized
- All procedures that modify it (in likely execution order based on call graph)
- What triggers each modification
- The business logic behind the state changes
- Any concerns about unexpected mutations

[Insert variable list with cross-reference data]
[Insert relevant procedure documentation]
[Insert call graph sections]
```

## Phase 3: Automated Validation and Self-Correction

### 3.1 AI-Driven Validation Process
**Objective**: AI automatically validates its own documentation for accuracy and completeness

**Automated Validation Steps**:
1. **Data Dictionary Validation**:
   - Cross-reference variable purposes with actual usage patterns in code
   - Verify data type interpretations against declarations and usage
   - Validate usage patterns match cross-reference reports
   - Identify and document all variable relationships

2. **Procedure Documentation Validation**:
   - Verify business logic explanations against actual code flow
   - Confirm processing steps by tracing execution paths
   - Validate error handling by analyzing condition checks
   - Cross-check syntax interpretation with language specifications

3. **Call Graph Validation**:
   - Verify call relationships by parsing all call statements
   - Confirm conditional execution paths by analyzing IF/CASE statements
   - Validate loop structures by checking loop syntax
   - Identify all control flow paths

4. **Variable Mutation Analysis Validation**:
   - Verify state transitions by tracing all assignment statements
   - Confirm execution order by following call graph paths
   - Validate business logic by analyzing conditional mutations
   - Identify all mutation points through comprehensive code scanning

**Validation Report**: Automatically generate `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_VALIDATION_REPORT.md` containing:
- Self-identified ambiguities with best-guess resolutions
- Confidence scores for different documentation sections
- Alternative interpretations where code is ambiguous
- Assumptions made and their justifications

### 3.2 Automated Self-Correction Process
**Process**:
1. **AI analyzes its own documentation**: Compare generated docs against source code and static analysis
2. **Identify inconsistencies**: Detect contradictions between documentation and code
3. **Resolve ambiguities**: Apply best practices and common patterns to resolve unclear logic
4. **Update documentation**: Automatically revise documentation based on validation findings
5. **Iterate**: Repeat validation and correction until consistency is achieved

**Self-Correction Categories**:
- **Technical Corrections**: Fix syntax misinterpretations using language specifications
- **Logic Refinements**: Improve business logic explanations using code flow analysis
- **Clarity Enhancements**: Rewrite unclear explanations for better comprehension
- **Completeness Fixes**: Fill documentation gaps by analyzing uncovered code sections

### 3.3 Automated Documentation Standards Application
**Objective**: Apply consistent documentation standards across all generated content

**Automatically Applied Standards**:
- Consistent documentation format and structure throughout
- Standardized terminology for OMNISCRIPT constructs (procedures, modules, functions)
- Appropriate detail level based on program complexity assessment
- Common pattern documentation using established templates
- Cross-reference formatting and linking conventions

**Auto-generate standards document**: `omniscript-documentation/DOCUMENTATION_STANDARDS.md` (at root level, shared across all programs) documenting:
- Format conventions used in this documentation
- Terminology definitions and usage
- Documentation depth rationale
- Pattern templates applied
- Reference examples from this program's documentation

## Phase 4: Synthesis and Comprehensive Documentation

### 4.1 Complete Program Overview Documentation
**Objective**: Finalize consolidated overview with comprehensive program details

**Process**:
1. **Aggregate all documentation artifacts**:
   - Data dictionary (including variable mutations)
   - Individual procedure documentation
   - Call graphs
   - Validation report and self-corrections

2. **Expand overview document shell** created in Phase 1.4 to include:
   - **Executive Summary**: High-level program purpose and functionality
   - **Program Structure Index**: Navigation to all components
   - **Business Context**: What business problem this program solves
   - **Architecture Overview**: Major processing sections and their relationships
   - **Core Flow Diagrams**: Embedded Mermaid diagrams (flowchart, call hierarchy, data flow)
   - **Data Flow**: How data moves through the program from input to output
   - **Key Processing Logic**: Critical algorithms and business rules
   - **Business Rules**: Explicit and implicit business rules extracted from code
   - **Dependencies**: External programs, modules, files, databases
   - **Integration Contract**: Entry point interface and external program interfaces
   - **Error Handling**: How errors are detected and handled (or risks if absent)
   - **Performance Characteristics**: Operation costs, bottlenecks, scalability limits
   - **Security Considerations**: Input validation, resource limits, access controls
   - **Testing Strategy**: Standard tests, edge cases, error scenarios
   - **Maintenance Notes**: Known issues, technical debt, refactoring recommendations

3. **Complete overview document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_OVERVIEW.md`

**AI Prompt for Synthesis**:
```
Using all the component documentation we've created (data dictionary, procedure docs,
call graphs, mutation analysis), synthesize a comprehensive program documentation that:

1. Explains the program's purpose and business value
2. Describes the overall architecture and major processing sections
3. Documents the data flow from input to output
4. Highlights critical business logic and algorithms
5. References detailed component documentation for deep dives
6. Provides maintenance guidance for future developers

Component documentation available:
[Link to data dictionary]
[Link to procedure documentation folder]
[Link to call graphs]
[Link to mutation analysis]
[Link to validation report]
```

### 4.2 Create Cross-Reference Documentation
**Objective**: Enable developers to quickly find information across all documentation

**Cross-Reference Types**:
1. **Variable Index**: Every variable → where documented, where used
2. **Procedure Index**: Every procedure → purpose, callers, callees, documentation link
3. **File Operations Index**: Every file → READ/WRITE locations, procedure documentation
4. **Business Rule Index**: Business rules → where implemented in code
5. **Error Handling Index**: Error codes/conditions → where handled

**Create cross-reference document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_CROSS_REFERENCE.md`

### 4.3 Document Integration Contracts and External Dependencies
**Integration Priority**: Document all external interfaces and dependencies

**Integration Documentation Requirements**:
1. **Called Programs**:
   - Program name and purpose (e.g., LOGGER)
   - Interface contract (parameters passed)
   - Expected return codes or status values
   - Error handling for call failures
   - Deployment requirements

2. **Calling Program Contract**:
   - Entry point parameters
   - Expected pre-conditions
   - Post-conditions and return values
   - Parameter validation requirements
   - Usage examples from calling code

3. **File Dependencies**:
   - Input file formats and schemas
   - Output file formats and schemas
   - File naming conventions
   - File access permissions required
   - Concurrent access considerations

4. **Module Dependencies**:
   - All import/include statements
   - Module purposes and contents
   - Shared data structure definitions
   - Version compatibility requirements

5. **System Requirements**:
   - OMNISCRIPT interpreter version
   - Operating system requirements
   - Required runtime flags
   - Runtime environment settings
   - Script requirements

6. **Deployment Configuration**:
   - Installation procedures
   - Configuration files required
   - Environment variables
   - Security permissions needed

**Create integration document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_INTEGRATION_GUIDE.md`

**AI Prompt Template**:
```
Document all integration points for this OMNISCRIPT program:

1. List all external program calls with parameter details
2. Document entry point interface
3. List all module imports and their purposes
4. Document file dependencies (input/output)
5. Identify interpreter and system requirements
6. Document deployment and configuration needs

[Insert program source]
[Insert all call statements]
[Insert all import statements]
```

### 4.4 Document Business Rules and Security Requirements
**Business Priority**: Extract and document all business rules explicitly

**Business Rules Documentation Requirements**:
1. **Explicit Business Rules**:
   - Data validation rules (formats, ranges, patterns)
   - Processing rules (calculations, transformations)
   - Business logic conditions (IF statements explained)
   - Default values and their business justification
   - Rounding and precision rules

2. **Implicit Business Rules** (inferred from code):
   - Syntax rules (comment delimiters, operators)
   - Delimiter rules (spaces, parentheses)
   - Token naming conventions
   - Maximum complexity limits
   - Processing sequence requirements

3. **Business Constraints**:
   - Record count limits (e.g., 100 tokens max)
   - Field length constraints
   - Required vs optional data elements
   - Conditional processing rules

4. **Security and Compliance**:
   - Input validation and sanitization
   - File path validation requirements
   - Resource usage limits (prevent infinite loops)
   - Audit logging requirements
   - Data privacy considerations
   - Access control requirements

5. **Compliance Requirements**:
   - Regulatory requirements (if applicable)
   - Audit trail completeness
   - Data retention rules
   - Change control procedures

**Create business rules document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_BUSINESS_RULES.md`

**AI Prompt Template**:
```
Extract and document all business rules from this OMNISCRIPT program:

1. Identify explicit business rules (validations, calculations)
2. Infer implicit business rules from code patterns
3. Document business constraints (limits, requirements)
4. Document security requirements (validation, limits)
5. Identify compliance and audit requirements
6. Categorize rules by business domain

[Insert program source]
[Insert data validation code]
[Insert business logic procedures]
```

### 4.5 Create Data Flow and Transformation Diagrams
**Visualization Priority**: Show how data moves and transforms through the program

**Data Flow Documentation Requirements**:
1. **Core Diagrams** (embedded in OVERVIEW.md):
   - Program flow flowchart
   - Call hierarchy graph
   - High-level data flow diagram

2. **Complex Diagrams** (separate DIAGRAMS.md):
   - Detailed data flow with transformations
   - Variable lifecycle state machines
   - File I/O sequence diagrams
   - Module dependency graphs

3. **Data Transformation Tables**:
   - Input format → Output format
   - Before/after examples
   - Transformation rules applied

**Add to diagrams document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_MERMAID_DIAGRAMS.md`

**AI Prompt Template**:
```
Create data flow visualizations for this OMNISCRIPT program:

1. High-level data flow: Input → Processing → Output
2. Variable lifecycle state machines for major variables
3. Data transformation examples (before/after)
4. State transition diagrams

[Insert program source]
[Insert data dictionary]
[Insert variable mutation analysis]
```

### 4.6 Generate Mermaid Diagrams (MANDATORY)
**Objective**: Create Mermaid-based visual representations for every OMNISCRIPT program to aid modernization, knowledge transfer, and understanding of legacy code structure

**CRITICAL**: Mermaid diagram generation is **MANDATORY** for every repository analyzed. These visualizations are essential for understanding program flow, documenting dependencies, knowledge transfer, identifying refactoring opportunities, and compliance documentation.

**Core Diagrams** (embedded in OVERVIEW.md):
1. Program Flow Diagram (flowchart)
2. Call Hierarchy (graph)
3. Data Flow Diagram (flowchart)

**Complex Diagrams** (separate DIAGRAMS.md):
4. Module Dependency Graph (graph)
5. File I/O Operations Timeline (sequenceDiagram)
6. Variable Lifecycle State Diagram (stateDiagram-v2)

**For complete Mermaid generation instructions, examples, best practices, and troubleshooting**, see [MERMAID_GUIDE.md](./MERMAID_GUIDE.md).

**Outputs**: 
- Core diagrams embedded in `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_OVERVIEW.md`
- Complex diagrams in `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_DIAGRAMS.md`

### 4.7 Finalize Cross-References in Overview
**Objective**: Complete all navigation links in the overview document

**Actions**:
1. Update `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_OVERVIEW.md` with links to:
   - Data dictionary (with variable mutations)
   - Call graph
   - Diagrams (complex visualizations)
   - **Error handling analysis**
   - **Performance analysis**
   - **Testing guide**
   - **Integration guide**
   - **Business rules**
   - Cross-reference documentation
   - Validation report
   - Documentation standards applied

2. Add navigation paths for different user types:
   - **New developers**: Start here path (executive summary → architecture → core diagrams → key procedures)
   - **Maintenance developers**: Quick reference path (cross-reference → specific procedure → testing guide)
   - **Business analysts**: Business logic path (business rules → implementation locations → data flow)
   - **Auditors**: Compliance path (error handling → file operations → business rules → audit logging)
   - **Performance engineers**: Performance path (performance analysis → bottlenecks → optimization recommendations)
   - **QA/Testers**: Testing path (testing guide → edge cases → integration scenarios)

## Phase 5: Establish Ongoing Maintenance Process

### 5.1 Create Documentation Maintenance Guide
**Objective**: Enable teams to maintain documentation as code evolves using AI assistance

**Maintenance Guide Contents**:
1. **When to Update Documentation**:
   - After any procedure modification
   - When adding new variables
   - After changing call relationships
   - When business logic changes
   - Before releasing to production

2. **How to Use AI for Updates**:
   - Provide AI with: original documentation, changed code, reason for change
   - Automatically generate updated documentation for modified sections
   - Run automated validation on updated documentation
   - Update cross-references and master index

3. **Documentation Quality Standards**:
   - Reference the DOCUMENTATION_STANDARDS.md created in Phase 3.3
   - Include examples of good maintenance updates
   - Document common pitfalls to avoid

4. **Workflow Integration**:
   - How to integrate documentation updates into change management process
   - When to create new documentation vs. update existing
   - How to handle deprecated code sections

**Create maintenance guide**: `omniscript-documentation/MAINTENANCE_GUIDE.md` (at root level, shared across all programs)

**AI Prompt Template for Maintenance**:
```
The following procedure was modified. Update its documentation accordingly.

Original Documentation: [link or content]
Original Code: [old code]
Modified Code: [new code]
Reason for Change: [change description]

Please update the documentation to reflect:
- Changes in business logic
- Changes in variables used
- Changes in control flow
- Any new error handling
```

### 5.2 Establish Documentation Repository
**Objective**: Centralize all documentation with version control

**Repository Structure**:
```
omniscript-documentation/
├── programs/
│   ├── PROGRAM-A/
│   │   ├── PROGRAM-A_OVERVIEW.md              [Consolidated: INDEX + COMPREHENSIVE_DOC]
│   │   ├── PROGRAM-A_DATA_DICTIONARY.md       [Includes: Variable Mutations section]
│   │   ├── PROGRAM-A_CALL_GRAPH.md
│   │   ├── PROGRAM-A_DIAGRAMS.md              [Complex diagrams; core in OVERVIEW]
│   │   ├── PROGRAM-A_ERROR_HANDLING.md
│   │   ├── PROGRAM-A_CROSS_REFERENCE.md
│   │   ├── PROGRAM-A_VALIDATION_REPORT.md
│   │   └── procedures/
│   │       ├── PROCEDURE-1.md
│   │       ├── PROCEDURE-2.md
│   │       └── ...
│   └── PROGRAM-B/
│       └── ...
├── shared/
│   ├── MODULES.md
│   ├── COMMON-ROUTINES.md
│   └── ...
├── standards/
│   ├── DOCUMENTATION_STANDARDS.md
│   └── MAINTENANCE_GUIDE.md
└── README.md (repository overview)
```

**Version Control Integration**:
- Commit documentation alongside code changes
- Link documentation commits to code commits
- Use branch strategies for major documentation efforts
- Tag documentation versions with code releases

### 5.3 Create Metrics and Quality Gates
**Objective**: Measure documentation quality and completeness

**Documentation Metrics**:
1. **Coverage Metrics**:
   - % of procedures documented
   - % of variables documented
   - % of call relationships mapped
   - % of files with comprehensive documentation

2. **Quality Metrics**:
   - Automated validation pass rate
   - Documentation update frequency
   - Time from code change to documentation update
   - Confidence scores across documentation sections

3. **Maintenance Metrics**:
   - Documentation age vs. code age
   - Documentation update lag time
   - Ratio of documentation updates to code changes

**Quality Gates**:
- No code promotion without updated documentation
- Automated validation must pass with high confidence scores
- Documentation completeness check in CI/CD pipeline
- Automated documentation refresh on code changes

**Create metrics dashboard**: Track and visualize documentation health over time

## Comprehensive Code Quality Assessment (Use Independent Template)

**When to Perform**: After completing phases 1-5, or as standalone assessment for existing code.

**Template Location**: `templates/CODE_QUALITY_ASSESSMENT.template.md`

**Purpose**: Perform comprehensive code quality, security, and operational risk assessment following industry standards and best practices for production-ready code validation.

### Assessment Scope

The CODE_QUALITY_ASSESSMENT template provides structured analysis across six critical dimensions:

**Section A: Error Handling Analysis**
- Comprehensive error handling mechanisms inventory
- Runtime error scenario analysis
- Resource limits and buffer overflow assessment
- Input validation evaluation
- Risk categorization (Critical/High/Medium/Low)

**Section B: OmniScript/COBOL Best Practices**
- API usage patterns and deprecated feature detection
- Integration patterns (external calls, parameters, return codes)
- Performance patterns (loops, string operations, file I/O)
- COBOL-specific checks (GOTO usage, PERFORM, file handling, WORKING-STORAGE)

**Section C: Security and Safety Assessment**
- 🔴 Critical security risks (hardcoded credentials, injection vulnerabilities, path traversal, auth bypass)
- 🟠 High security risks (missing validation, info disclosure, insecure file handling, race conditions)
- 🟡 Medium security risks (weak sanitization, insufficient logging, deprecated crypto)
- Security posture summary and compliance checklist

**Section D: Operational Risk Assessment**
- 🔴 Critical operational risks (data corruption, crashes, unrecoverable errors, silent data loss)
- 🟠 High operational risks (performance degradation, resource exhaustion, error recovery gaps)
- 🟡 Medium operational risks (suboptimal patterns, minor leaks, maintainability issues)
- 🟢 Low risks and ⚪ informational findings

**Section E: Code Quality Scoring**
- Overall quality metrics (0-100 scores with grades A-F)
- Per-procedure quality assessment cards
- Best practice violations with specific recommendations
- Performance impact analysis
- Security posture evaluation
- Prioritized remediation roadmap

**Section F: Automated Quality Gate Checks**
- Pass/Fail/Pass-with-Warnings determination
- Deployment readiness recommendation
- Compliance checklist (regulatory, internal standards, production readiness)
- Blocking issues identification

### Integration with Documentation Workflow

**Phase 2.3 Reference**: Error handling observations during procedure documentation reference Section A of quality assessment template for comprehensive analysis.

**Phase 4 Integration**: Link quality assessment from master index and comprehensive documentation.

**Pre-Production Gate**: Mandatory quality assessment before production deployment for mission-critical programs.

**Output Document**: Create `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`

### When to Use Quality Assessment

**Mandatory For**:
- Mission-critical programs before production deployment
- Programs handling sensitive data or financial transactions
- Programs with external integrations
- Compliance and audit requirements

**Recommended For**:
- All production-bound code
- Major version releases
- After significant refactoring
- Periodic security reviews

**Optional For**:
- Simple utility programs with limited scope
- Internal development tools
- Proof-of-concept code

### Quick Reference: Assessment Priority by Program Type

| Program Type | Assessment Scope | Sections Required | Quality Gate |
|-------------|------------------|-------------------|--------------|
| **Mission-Critical** | Comprehensive (A-F) | All sections mandatory | ✅ Must PASS |
| **Standard Business Logic** | Standard (A, C, D, F) | Core sections + gate | ✅/⚠️ PASS or PASS WITH WARNINGS |
| **Utility Programs** | Targeted (A, D, F) | Error handling + risks + gate | ⚠️ Can PASS WITH WARNINGS |
| **Internal Tools** | Optional | As needed | Not required |

**For detailed instructions and template structure**, refer to `templates/CODE_QUALITY_ASSESSMENT.template.md`.

## Decision Trees

### Chunking Strategy Decision Tree
```
Program Size Assessment:
├── Small Programs (<500 lines)
│   └── Document as single unit with minimal chunking
├── Medium Programs (500-2000 lines)
│   ├── Chunk by major sections (initialization, main processing, cleanup)
│   └── Document procedures individually
└── Large Programs (>2000 lines)
    ├── Chunk by module first
    ├── Sub-chunk by procedure groups
    ├── Document related procedure groups (3-5 procedures)
    └── Create extensive cross-reference system
```

### Static Analysis Approach Decision Tree
```
Available Tools Assessment:
├── Interpreter with Cross-Reference Available
│   └── Use interpreter cross-reference output as primary source
├── Static Analysis Tool Available
│   └── Generate comprehensive reports before AI documentation
├── No Tools Available
│   ├── Parse OMNISCRIPT manually with custom scripts
│   └── Create basic cross-reference from source analysis
└── Mixed Tooling
    └── Combine outputs for comprehensive analysis
```

### Documentation Depth Decision Tree
```
Program Criticality Assessment:
├── Mission-Critical Programs
│   ├── Full comprehensive documentation (all phases)
│   ├── Multiple expert reviews
│   ├── Extensive diagrams and cross-references
│   └── Detailed maintenance guide
├── Standard Business Logic
│   ├── Standard documentation (data dictionary + procedure docs + call graph)
│   ├── Single expert review
│   └── Basic maintenance guide
└── Simple Utility Programs
    ├── Minimal documentation (overview + key procedure docs)
    ├── Peer review (non-expert acceptable)
    └── Reference to general maintenance guide
```

### Automated Validation Priority Tree
```
Documentation Validation Focus:
├── Critical Validation (Highest Confidence Required)
│   ├── Business logic interpretations
│   ├── Variable mutation patterns
│   ├── Error handling documentation
│   └── Call graph accuracy
├── Standard Validation (Medium Confidence Required)
│   ├── Data dictionary entries
│   ├── Individual paragraph documentation
│   └── File operation sequences
└── Format Validation (Basic Compliance)
    ├── Formatting and structure consistency
    ├── Cross-reference completeness
    └── Diagram generation and linking
```

## Success Criteria

### Phase Completion Checkpoints

**Phase 1 Complete When**:
- [ ] Program structure fully analyzed and documented
- [ ] Chunking strategy defined with clear boundaries
- [ ] Static analysis reports generated
- [ ] Master index created with all sections mapped

**Phase 2 Complete When**:
- [ ] Data dictionary completed for all variables (with buffer limits documented)
- [ ] Each procedure documented individually (with brief error handling notes included)
- [ ] Call graph created showing all call relationships
- [ ] Variable mutation patterns identified and documented
  

**Phase 3 Complete When**:
- [ ] Automated validation has analyzed all documentation
- [ ] All identified inconsistencies self-corrected
- [ ] Documentation standards automatically applied
- [ ] Validation report completed with confidence scores

**Phase 4 Complete When**:
- [ ] Comprehensive program documentation synthesized (with all enhancements)
- [ ] Cross-reference documentation created
- [ ] Visual diagrams generated (MANDATORY - all 6+ Mermaid diagrams)
- [ ] **Integration guide completed with deployment instructions**
- [ ] **Business rules extracted and documented**
- [ ] **Data flow diagrams created**
- [ ] Master index updated with all links (including all new document types)

**Phase 5 Complete When**:
- [ ] Maintenance guide created
- [ ] Team trained on AI-assisted documentation
- [ ] Documentation repository established
- [ ] Metrics and quality gates implemented


**Comprehensive Quality Assessment Complete When** (Using CODE_QUALITY_ASSESSMENT Template):
- [ ] Code quality assessment performed using template (templates/CODE_QUALITY_ASSESSMENT.template.md)
- [ ] Error handling comprehensively analyzed (Template Section A)
- [ ] Best practices evaluated for OmniScript/COBOL (Template Section B)
- [ ] Security vulnerabilities identified and categorized (Template Section C)
- [ ] Operational risks assessed with priority levels (Template Section D)
- [ ] Quality scoring completed for all major procedures (Template Section E)
- [ ] Quality gate status determined (PASS/PASS WITH WARNINGS/FAIL) (Template Section F)
- [ ] Remediation roadmap created with effort estimates and priorities
- [ ] Quality assessment document created: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`
- [ ] Quality assessment linked from master index and comprehensive documentation
- [ ] Expert security/operations team review completed (for mission-critical programs)

### Overall Success Indicators

**Immediate Success**:
- Documentation covers all program components
- Automated validation passes with high confidence scores
- Documentation is clear, complete, and self-consistent
- Documentation integrated into development workflow
- **Comprehensive quality assessment completed (using CODE_QUALITY_ASSESSMENT template)**
- **Quality gate determination made (PASS/PASS WITH WARNINGS/FAIL)**
- **Security vulnerabilities identified and prioritized**
- **Operational risks assessed with mitigation plans**
- **Remediation roadmap created for all critical/high priority findings**

**Long-term Success**:
- Documentation automatically updates with code changes
- Reduced time to understand and modify OMNISCRIPT programs
- Documentation provides reliable reference for program understanding
- New developers can onboard using generated documentation
- Automated validation maintains documentation quality over time

## Troubleshooting Common Issues

### AI Misunderstandings

**Issue**: AI misinterprets OMNISCRIPT syntax or semantics
**Solutions**:
- Automatically detect OMNISCRIPT version from source code indicators
- Include OMNISCRIPT language reference in AI context automatically
- Use comprehensive OMNISCRIPT specifications for validation
- Run multiple validation passes to catch inconsistencies
- Document ambiguities with confidence scores and alternative interpretations

**Prevention**: Use progressive analysis starting with data structures before procedural logic

### Context Window Limitations

**Issue**: Program section too large for AI context window
**Solutions**:
- Further chunk the section into smaller pieces
- Summarize peripheral code while detailing core logic
- Use multiple AI passes with different focus areas
- Provide summary documentation from previous chunks as context

**Prevention**: Aggressive chunking strategy from Phase 1

### Static Analysis Data Quality

**Issue**: Cross-reference reports incomplete or inaccurate
**Solutions**:
- Verify OMNISCRIPT interpreter settings
- Check for interpreter directives affecting analysis
- Manually supplement with source code inspection
- Use multiple analysis tools and cross-validate

**Prevention**: Test static analysis tools on known programs first

### Documentation Drift

**Issue**: Documentation becomes outdated as code changes
**Solutions**:
- Implement automated documentation quality gates
- Automatically detect documentation-code mismatches on commit
- Trigger automatic documentation updates on code changes
- Run automated validation on schedule to detect drift

**Prevention**: Establish automated maintenance process in Phase 5

## Best Practices Summary

### For Using AI to Document OMNISCRIPT

1. **Chunking Strategy**:
   - Break large programs into logical sections
   - Document each section separately to avoid context overflow
   - Use AI to create master index linking all sections
   - Keep chunks focused on single responsibilities

2. **Augment with Static Analysis**:
   - Generate cross-reference reports before AI documentation
   - Use tools to identify variable usage patterns
   - Feed static analysis to AI for interpretation
   - Don't rely on AI to discover relationships from source alone

3. **Iterative Approach**:
   - Start with data dictionary (all variables)
   - Document each procedure individually
   - Create call graphs showing call relationships
   - Identify global variable mutation patterns
   - Synthesize into comprehensive documentation
   - Build from components to whole

4. **Automated Validation and Self-Correction**:
   - AI generates initial documentation
   - AI automatically validates against source code and static analysis
   - AI self-corrects identified inconsistencies
   - AI documents confidence levels and ambiguities
   - Automated validation ensures documentation accuracy

## Next Steps

After completing OMNISCRIPT program documentation:

1. **Apply to More Programs**: Use lessons learned to document additional OMNISCRIPT programs
2. **Refine Process**: Update workflow based on what worked and what didn't
3. **Build Knowledge Base**: Accumulate program documentation into searchable repository
4. **Enable Modernization**: Use documentation as foundation for modernization efforts
5. **Cross-Reference Systems**: Link related programs and shared modules
6. **Train New Developers**: Use documentation for onboarding and knowledge transfer