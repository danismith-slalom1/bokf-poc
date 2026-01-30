# OMNISCRIPT Documentation Workflow

This document describes the step-by-step process for using AI to document OMNISCRIPT programs effectively, following industry best practices for legacy OMNISCRIPT codebase documentation.

## Overview

The OMNISCRIPT Documentation Workflow transforms undocumented or poorly documented OMNISCRIPT programs into well-documented, maintainable codebases through a fully automated process by:
1. Analyzing and chunking OMNISCRIPT program structure
2. Performing static analysis to understand variable usage and program flow
3. Detecting current version (expected: 6.05) and assessing upgrade readiness to OmniScript 7.5
4. Iteratively documenting each component with AI-driven analysis
5. Synthesizing comprehensive documentation with call graphs and cross-references
6. Generating complete, production-ready documentation without requiring clarification

## Documentation Directory

**CRITICAL**: Throughout this workflow, `${OMNISCRIPT_DOCS_DIR}` refers to the `omniscript-documentation/{REPO-NAME}/[PROGRAM-NAME]/` directory structure at the project root. All program-specific documentation artifacts MUST be placed within the program's subdirectory under its repository folder, never at the project root level.

**Directory Structure**:
- `omniscript-documentation/` - Root directory for all OMNISCRIPT documentation
- `omniscript-documentation/{REPO-NAME}/` - Repository-specific documentation folder
- `omniscript-documentation/{REPO-NAME}/[PROGRAM-NAME]/` - Program-specific documentation folder
- `omniscript-documentation/{REPO-NAME}/standards/` - Repository-specific standards and guidelines (optional)
- `omniscript-documentation/{REPO-NAME}/training/` - Repository-specific training materials (optional)
- `omniscript-documentation/standards/` - Cross-repository shared standards (optional)

**Example paths**:
- Data dictionary: `omniscript-documentation/my-omniscript-repo/PAYROLL/PAYROLL_DATA_DICTIONARY.md`
- Call graph: `omniscript-documentation/my-omniscript-repo/PAYROLL/PAYROLL_CALL_GRAPH.md`
- Comprehensive doc: `omniscript-documentation/my-omniscript-repo/PAYROLL/PAYROLL_COMPREHENSIVE_DOC.md`
- Procedure docs: `omniscript-documentation/my-omniscript-repo/PAYROLL/procedures/PROCESS-RECORDS.md`

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

### 1.4 Create Program Index and Documentation Plan
- **Objective**: Generate master index linking all program sections for holistic documentation
- **Actions**:
  - **Create master index document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_INDEX.md`
  - List all modules, procedures with line numbers
  - Document identified chunks and their logical boundaries
  - Reference static analysis reports for each section
  - Create documentation sequence plan (order in which sections will be documented)
  
**Index Structure**:
```markdown
# [PROGRAM-NAME] Documentation Index

## Program Overview
- Name: [Program ID]
- Purpose: [Brief description]
- Dependencies: [Imported modules, called programs]

## Modules and Procedures
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

## Phase 1.5: OmniScript Version Quality Analysis and Upgrade Readiness Assessment

**Context**: Files are expected to be OmniScript 6.05, but version detection will identify the actual version to ensure accuracy.

### 1.5.1 Current Version Identification and Quality Assessment
- **Objective**: Identify the current OmniScript version (expected: 6.05) and assess code quality specific to that version
- **Actions**:
  - **Detect OmniScript Version**: Analyze source code headers, comments, syntax patterns, and runtime directives to determine current version
    - Check file headers for version declarations
    - Analyze syntax patterns unique to specific versions
    - Look for version-specific API calls or language constructs
    - Check comments for version metadata
    - If version cannot be determined, document as "Unknown" and list observed features
  
  **Version Detection Best Practices**:
  - **Header Analysis**: Look for comments like `# OmniScript 6.05`, `Version: 5.2`, or copyright notices
  - **Syntax Patterns**: Identify version-specific keywords, operators, or language constructs
  - **API Calls**: Match function/method calls against version-specific API documentation
  - **Confidence Levels**:
    - **High**: Explicit version declaration found in headers/metadata
    - **Medium**: Version inferred from multiple consistent syntax/API patterns
    - **Low**: Version estimated from limited or conflicting indicators
    - **Unable to Detect**: No reliable version indicators found
  - **Conservative Approach**: When version is unknown, assume an older version for comprehensive upgrade analysis
  
  - **Version-Specific Feature Usage**: Document which version-specific features, syntax, or APIs are used
  - **Deprecated Feature Detection**: Identify any features marked as deprecated in current version
  - **Code Quality Metrics for Current Version**:
    - Compliance with version-specific best practices
    - Usage of outdated patterns available in current version
    - Performance patterns specific to current version
    - Security considerations for current version

### 1.5.2 OmniScript Target Version Upgrade Readiness Assessment
- **Objective**: Evaluate the program's readiness for upgrade from detected version (expected: 6.05) to OmniScript 7.5 and identify migration requirements
- **Actions**:
  - **Determine Upgrade Path**:
    - Identify current version (expected: 6.05, but verify through detection)
    - Identify target version: 7.5
    - Document version gap (e.g., 6.05 → 7.5, or actual detected version → 7.5)
    - If detected version differs from expected 6.05, note the discrepancy
  
  - **Breaking Changes Analysis**:
    - Identify syntax changes between current and target versions
    - Document removed features or deprecated APIs used in current code
    - Flag incompatible language constructs
    - Identify behavioral changes in existing features
    - Note: If current version is unknown, identify all potentially problematic patterns
  
  - **API and Feature Mapping**:
    - Map current version APIs to target version equivalents
    - Identify new target version features that could replace current implementations
    - Document API signature changes requiring code updates
    - Note removed APIs requiring alternative implementations
  
  - **Compatibility Risk Assessment**:
    - **High Risk**: Breaking changes requiring mandatory code modifications
    - **Medium Risk**: Deprecated features still functional but requiring eventual updates
    - **Low Risk**: Fully compatible code requiring no changes
    - **Opportunity**: Areas where 7.5 features could improve code quality
  
  - **Data Structure Compatibility**:
    - Assess data type changes between versions
    - Identify serialization/deserialization impacts
    - Document file format compatibility issues
    - Evaluate database schema compatibility
  
  - **Performance Implications**:
    - Identify performance improvements expected in 7.5
    - Document performance regressions to watch for
    - Note new optimization opportunities in 7.5
    - Assess memory usage pattern changes

### 1.5.3 Migration Impact Analysis
- **Objective**: Quantify the effort and risk involved in upgrading to OmniScript 7.5
- **Actions**:
  - **Code Modification Assessment**:
    - **Lines Requiring Changes**: Estimate lines of code requiring modification
    - **Procedure Impact**: List procedures requiring updates with complexity ratings
    - **Test Impact**: Identify test cases requiring updates or creation
    - **Documentation Impact**: Document sections requiring version-specific updates
  
  - **Dependency Chain Analysis**:
    - Identify external programs/modules requiring concurrent upgrades
    - Document shared libraries requiring version compatibility
    - Map integration points requiring version coordination
    - Assess third-party dependency upgrade requirements
  
  - **Migration Effort Estimation**:
    - **Trivial** (< 1 day): Cosmetic changes, simple syntax updates
    - **Minor** (1-3 days): Limited API updates, straightforward replacements
    - **Moderate** (1-2 weeks): Significant API changes, moderate refactoring
    - **Major** (2+ weeks): Extensive refactoring, architectural changes
  
  - **Risk Categorization**:
    - **Critical Risks**: Issues that could cause runtime failures
    - **High Risks**: Potential data corruption or incorrect business logic
    - **Medium Risks**: Performance degradation or usability issues
    - **Low Risks**: Minor cosmetic or non-functional impacts

### 1.5.4 Upgrade Roadmap and Recommendations
- **Objective**: Provide actionable guidance for successful upgrade to OmniScript 7.5
- **Actions**:
  - **Pre-Migration Checklist**:
    - Static analysis requirements before upgrade
    - Backup and rollback procedures
    - Test environment setup requirements
    - Dependency version compatibility verification
  
  - **Migration Sequence**:
    1. **Phase 1 - Preparation**: Backup, analysis, test environment setup
    2. **Phase 2 - Non-Breaking Updates**: Address deprecation warnings
    3. **Phase 3 - Breaking Changes**: Update incompatible code
    4. **Phase 4 - Testing**: Comprehensive regression testing
    5. **Phase 5 - Optimization**: Leverage new 7.5 features
    6. **Phase 6 - Deployment**: Production rollout with monitoring
  
  - **Code Modification Priorities**:
    - **Priority 1 - Critical**: Breaking changes causing compilation/runtime failures
    - **Priority 2 - High**: Deprecated features with removal timeline
    - **Priority 3 - Medium**: Performance or security improvements
    - **Priority 4 - Low**: Optional enhancements and modernization
  
  - **Testing Strategy**:
    - Unit tests for modified procedures
    - Integration tests for external interfaces
    - Regression tests for unchanged functionality
    - Performance tests comparing 6.05 vs 7.5 behavior
    - Security tests for new 7.5 security features
  
  - **Fallback Planning**:
    - Rollback triggers and criteria
    - Version coexistence strategy if needed
    - Gradual migration approach for complex systems
    - Parallel running considerations

### 1.5.5 Generate Upgrade Assessment Document
- **Objective**: Create comprehensive documentation of upgrade readiness and migration plan
- **Actions**:
  - **Create upgrade assessment document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_UPGRADE_ASSESSMENT.md`
  
  **Document Structure**:
  ```markdown
  # [PROGRAM-NAME] OmniScript Upgrade Assessment
  
  ## Executive Summary
  - Detected Version: [Version or "Unknown"]
  - Expected Version: 6.05
  - Target Version: 7.5
  - Overall Risk Level: [Low/Medium/High]
  - Estimated Migration Effort: [Trivial/Minor/Moderate/Major]
  - Recommended Timeline: [Duration]
  
  ## Current Version Analysis
  - Version-Specific Features Used
  - Code Quality Assessment
  - Known Issues in Current Version
  
  ## Upgrade Impact Analysis
  ### Breaking Changes
  - [List of breaking changes with line numbers and procedures]
  
  ### API Migrations Required
  - [Old API → New API mappings with usage locations]
  
  ### Deprecated Feature Usage
  - [List of deprecated features still in use]
  
  ### Compatibility Risks
  - High Risk Items: [List]
  - Medium Risk Items: [List]
  - Low Risk Items: [List]
  
  ## Migration Effort Breakdown
  - Lines of Code Requiring Changes: [Count]
  - Procedures Requiring Updates: [List with complexity]
  - Testing Requirements: [Test count and types]
  - Estimated Total Effort: [Duration]
  
  ## Upgrade Roadmap
  ### Pre-Migration Phase
  - [Checklist items]
  
  ### Migration Phases
  1. [Phase 1 details]
  2. [Phase 2 details]
  ...
  
  ### Post-Migration Validation
  - [Validation checklist]
  
  ## Recommended Actions
  - Immediate Actions: [List]
  - Pre-Upgrade Actions: [List]
  - Post-Upgrade Actions: [List]
  
  ## Benefits of Upgrading
  - Performance Improvements: [List]
  - New Features Available: [List]
  - Security Enhancements: [List]
  - Maintenance Improvements: [List]
  
  ## Risks of Not Upgrading
  - Support Timeline: [6.05 end-of-life date if known]
  - Security Vulnerabilities: [Known issues]
  - Performance Limitations: [List]
  - Feature Gaps: [List]
  ```

**AI Prompt Template for Upgrade Assessment**:
```
Analyze this OmniScript program for upgrade readiness to OmniScript 7.5:

EXPECTED VERSION: 6.05 (but verify through detection)

1. DETECT CURRENT VERSION:
   - Analyze file headers, comments, and metadata for version information
   - Identify version-specific syntax patterns and API usage
   - Document detected version (expected: 6.05)
   - If detected version differs from 6.05, note the discrepancy
   - List evidence used for version detection

2. IDENTIFY TARGET VERSION: 7.5

3. ANALYZE UPGRADE PATH:
   - If current version detected: Perform version-specific analysis
   - If current version unknown: Perform conservative analysis assuming older version
   - List all breaking changes between versions that affect this code
   - Map current APIs to their target version equivalents

4. Assess compatibility risks (High/Medium/Low)
5. Estimate migration effort (lines changed, procedures affected)
6. Recommend migration sequence and priorities
7. Document testing requirements for upgrade validation
8. List benefits of upgrading and risks of staying on current version

Context provided:
- Program source code: [Insert]
- Static analysis from Phase 1: [Insert]
- OmniScript migration guide for detected version path: [Insert or reference]
- Note: If version cannot be detected, use conservative assumptions

Generate a comprehensive upgrade assessment document following the template.
```

**Integration with Existing Documentation**:
- Reference upgrade assessment in master index (`[PROGRAM-NAME]_INDEX.md`)
- Link to upgrade assessment from comprehensive documentation
- Update error handling analysis with version-specific considerations
- Include upgrade roadmap in maintenance guide
- Add version compatibility notes to integration guide
- Document 7.5-specific features in performance analysis recommendations

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

### 2.3 Document Code Quality and Risk Assessment
**Critical Priority**: Comprehensive analysis of code quality, security and operational risks.

#### A. Error Handling Analysis

**Error Handling Documentation Requirements**:
1. **Error Status Analysis**:
   - Identify all error handling mechanisms (try-catch, status codes, error flags)
   - Document expected status codes and handling procedures
   - Note operations without error checking
   - Document recovery procedures for errors

2. **Runtime Error Scenarios**:
   - File not found or access denied
   - Invalid data format or type mismatch
   - Buffer overflow conditions
   - Division by zero or arithmetic errors
   - String overflow in concatenation operations
   - Array index out of bounds

3. **Resource Limit Documentation**:
   - Maximum file sizes supported
   - Buffer size limitations and overflow risks
   - Array bounds and size limits
   - Memory usage patterns
   - Token/record count limits

4. **Input Validation**:
   - File path validation and sanitization
   - Data format validation
   - Range checking on numeric fields
   - Special character handling

5. **Risk Assessment**:
   - High Risk: No error handling, buffer overflows possible
   - Medium Risk: Limited validation, missing error recovery
   - Low Risk: Comprehensive error handling implemented

**Create error handling document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_ERROR_HANDLING.md`

**AI Prompt Template**:
```
Analyze this OMNISCRIPT program for error handling and risks:

1. Identify all error handling mechanisms
2. List all file operations and their error handling
3. Document buffer sizes and overflow risks
4. Identify input validation mechanisms
5. List potential runtime error scenarios
6. Assess risk level (High/Medium/Low) for each category
7. Recommend error handling improvements

[Insert program source]
[Insert file I/O procedures]
```

#### B. OmniScript/COBOL Best Practices Assessment (NEW)

**OmniScript-Specific Quality Checks**:
1. **API Usage Patterns**:
   - Proper use of OmniScript APIs
   - Deprecated API usage
   - Performance-optimal API calls
   - Error handling for API failures

2. **Integration Patterns**:
   - Proper external program calling conventions
   - Parameter passing best practices
   - Return code handling
   - Transaction boundaries

3. **Performance Patterns**:
   - Efficient loop structures
   - String operation optimization
   - File I/O efficiency
   - Memory usage patterns

**COBOL-Specific Quality Checks** (if applicable):
1. **GOTO Usage**: Flag excessive GOTO statements (anti-pattern)
2. **PERFORM Best Practices**: Proper paragraph structure
3. **Data Division Organization**: Logical grouping and naming
4. **File Handling**: Proper OPEN/CLOSE sequences
5. **WORKING-STORAGE Efficiency**: Minimize memory footprint

#### C. Security and Safety Assessment (NEW)

**Security Risk Categories**:

🔴 **CRITICAL SECURITY RISKS**:
- Hardcoded credentials or API keys
- SQL injection vulnerabilities
- Command injection risks
- Insecure file path handling
- Authentication/authorization bypasses
- Sensitive data exposure in logs

🟠 **HIGH SECURITY RISKS**:
- Missing input validation
- Insufficient error information disclosure
- Insecure temporary file handling
- Race conditions in file operations
- Inadequate access controls

🟡 **MEDIUM SECURITY RISKS**:
- Weak input sanitization
- Missing bounds checking
- Insufficient logging for audit
- Deprecated cryptographic functions

#### D. Operational Risk Assessment (NEW)

**Risk Ranking System**:

🔴 **CRITICAL RISK** (Immediate Action Required):
- Data corruption potential
- System crash scenarios
- Unrecoverable errors
- Silent data loss
- Security vulnerabilities

🟠 **HIGH RISK** (Address Before Production):
- Performance degradation under load
- Resource exhaustion (memory leaks, file handles)
- Incomplete error recovery
- Data inconsistency potential
- Integration failures

🟡 **MEDIUM RISK** (Address in Next Cycle):
- Suboptimal performance patterns
- Minor resource leaks
- Inconsistent error handling
- Maintainability issues
- Technical debt accumulation

🟢 **LOW RISK** (Nice to Have):
- Code style inconsistencies
- Minor inefficiencies
- Documentation gaps
- Non-critical warnings

⚪ **INFORMATIONAL** (No Action Required):
- Best practice suggestions
- Optimization opportunities
- Refactoring recommendations

#### E. Code Quality Scoring (NEW)

For each code section, generate:

**Quality Assessment Card**:
```markdown
## Quality Assessment: [Procedure/Section Name]

### Overall Risk Level: 🟠 HIGH RISK

### Risk Breakdown:
- Security: 🔴 CRITICAL (1 issue)
- Operational: 🟠 HIGH (2 issues)
- Performance: 🟡 MEDIUM (3 issues)
- Maintainability: 🟢 LOW (5 issues)

### Critical Findings:

#### 🔴 CRITICAL: Hardcoded Database Password
- **Location**: Line 145
- **Issue**: Database password stored in plain text
- **Impact**: Complete database compromise if code is exposed
- **Recommendation**: Use environment variables or secure credential store
- **Effort**: 2 hours
- **Priority**: IMMEDIATE

#### 🟠 HIGH: Unhandled File Not Found
- **Location**: Lines 203-210
- **Issue**: No error handling for missing input file
- **Impact**: Program crash, data processing failure
- **Recommendation**: Add try-catch with graceful degradation
- **Effort**: 4 hours
- **Priority**: Before Production

[... continue for all findings ...]

### Best Practice Violations:

#### OmniScript-Specific:
- ❌ Using deprecated API `OLD_FUNCTION` (Line 89)
  - **Recommendation**: Migrate to `NEW_FUNCTION`
- ❌ Inefficient loop pattern (Lines 145-167)
  - **Recommendation**: Use bulk operations

#### COBOL-Specific (if applicable):
- ❌ Excessive GOTO usage (12 instances)
  - **Recommendation**: Refactor to structured programming
- ❌ WORKING-STORAGE not optimized (Lines 45-89)
  - **Recommendation**: Reorder for memory alignment

### Performance Impact:
- **Current**: O(n²) complexity in main loop
- **Potential**: O(n) with recommended changes
- **Estimated Improvement**: 80% faster for large datasets

### Security Posture:
- **Authentication**: ❌ Missing
- **Input Validation**: ⚠️ Partial
- **Error Disclosure**: ❌ Too verbose (exposes internals)
- **Audit Logging**: ✅ Present

### Recommended Actions (Prioritized):
1. **IMMEDIATE**: Remove hardcoded credentials (Line 145)
2. **Before Production**: Add file error handling (Lines 203-210)
3. **Next Sprint**: Optimize loop performance (Lines 145-167)
4. **Technical Debt**: Refactor GOTO statements (12 instances)

#### F. Automated Quality Gate Checks (NEW)

**Quality Gates** (Pass/Fail Criteria):

✅ **PASS Criteria**:
- No CRITICAL security risks
- No CRITICAL operational risks
- All HIGH risks have mitigation plans
- Error handling present for all I/O operations
- Input validation for all external data

❌ **FAIL Criteria**:
- Any CRITICAL security risk present
- Hardcoded credentials detected
- No error handling for file operations
- SQL injection vulnerabilities present
- Missing input validation on user data

**Create quality assessment document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`

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

4. **Create mutation analysis document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_VARIABLE_MUTATIONS.md`

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

### 4.1 Create Master Program Documentation
**Objective**: Synthesize all component documentation into comprehensive program overview

**Process**:
1. **Aggregate all documentation artifacts**:
   - Data dictionary
   - Individual procedure documentation
   - Call graphs
   - Variable mutation analysis
   - Validation report and self-corrections

2. **Generate comprehensive program documentation** that includes:
   - **Executive Summary**: High-level program purpose and functionality
   - **Business Context**: What business problem this program solves
   - **Version Information**: Detected OmniScript version and upgrade readiness status
   - **Architecture Overview**: Major processing sections and their relationships
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

3. **Create master program document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_COMPREHENSIVE_DOC.md`

**AI Prompt for Synthesis**:
```
Using all the component documentation we've created (data dictionary, procedure docs,
call graphs, mutation analysis, upgrade assessment), synthesize a comprehensive program documentation that:

1. Explains the program's purpose and business value
2. Identifies detected OmniScript version and upgrade readiness to target version
3. Describes the overall architecture and major processing sections
4. Documents the data flow from input to output
5. Highlights critical business logic and algorithms
6. Summarizes upgrade considerations and migration roadmap
7. References detailed component documentation for deep dives
8. Provides maintenance guidance for future developers

Component documentation available:
[Link to upgrade assessment]
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
1. **Data Flow Diagram** (high-level):
   - Input sources → Processing stages → Output destinations
   - Data transformations at each stage
   - State changes through the program

2. **Variable Lifecycle Diagrams** (detailed):
   - When each variable is created/initialized
   - Where it's modified (mutation points)
   - Where it's consumed/read
   - Final state at program end

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

**CRITICAL**: Mermaid diagram generation is **MANDATORY** for every repository analyzed. These visualizations are essential for:
- Understanding program flow without reading raw OMNISCRIPT
- Documenting complex dependencies for modernization efforts
- Knowledge transfer to developers unfamiliar with OMNISCRIPT
- Identifying refactoring opportunities
- Compliance and audit documentation

**Required Mermaid Diagrams for Each Program**:

1. **Program Flow Diagram (flowchart)**:
   - Main program logic from start to termination
   - Decision points (IF/CASE statements)
   - Loop structures
   - Major processing sections
   - Error handling paths

   ```mermaid
   flowchart TD
       Start([Program Start]) --> Init[INITIALIZATION]
       Init --> Read{READ-INPUT-FILE}
       Read -->|EOF| Close[CLOSE-FILES]
       Read -->|Success| Process[PROCESS-RECORD]
       Process --> Validate{VALIDATE-DATA}
       Validate -->|Valid| Write[WRITE-OUTPUT]
       Validate -->|Invalid| Error[ERROR-HANDLING]
       Write --> Read
       Error --> Read
       Close --> End([Program End])
   ```

2. **Call Hierarchy (graph)**:
   - All procedure call relationships
   - Nested call structures
   - Execution order and dependencies
   - Loop procedures vs one-time execution

   ```mermaid
   graph TD
       Main[MAIN-PROCESSING] --> Init[INITIALIZATION]
       Main --> Process[PROCESS-RECORDS]
       Process --> Read[READ-INPUT-FILE]
       Process --> Validate[VALIDATE-RECORD]
       Process --> Write[WRITE-OUTPUT-FILE]
       Validate --> Check1[CHECK-NUMERIC-FIELDS]
       Validate --> Check2[CHECK-DATE-FIELDS]
       Main --> Term[TERMINATION]
   ```

3. **Data Flow Diagram (flowchart)**:
   - Input files and their processing
   - Data transformations through program variables
   - Output file generation
   - Variable mutations and state changes

   ```mermaid
   flowchart LR
       Input[(INPUT-FILE)] --> Read[Read Records]
       Read --> Vars[Program Variables<br/>Data Transformation]
       Vars --> Calc[Business Logic<br/>Calculations]
       Calc --> Format[Format Output]
       Format --> Write[Write Records]
       Write --> Output[(OUTPUT-FILE)]
   ```

4. **Module Dependency Graph (graph)**:
   - All import/include statements and their relationships
   - Shared data structures across programs
   - Nested module inclusions
   - Dependency hierarchy

   ```mermaid
   graph TB
       Program[MAIN-PROGRAM] --> Mod1[IMPORT CUSTOMER-MODULE]
       Program --> Mod2[IMPORT DATE-UTILITIES]
       Program --> Mod3[IMPORT ERROR-CODES]
       Mod2 --> Mod4[IMPORT COMMON-UTILS]
       Mod3 --> Mod4
   ```

5. **File I/O Operations Timeline (sequenceDiagram)**:
   - All file operations in execution order
   - OPEN/READ/WRITE/CLOSE sequences
   - Multi-file coordination
   - Transaction boundaries

   ```mermaid
   sequenceDiagram
       participant P as Program
       participant I as Input File
       participant O as Output File
       participant L as Log File
       
       P->>I: OPEN INPUT
       P->>O: OPEN OUTPUT
       P->>L: OPEN EXTEND
       loop Until EOF
           P->>I: READ
           I-->>P: Record Data
           P->>P: Process Logic
           P->>O: WRITE
           P->>L: WRITE Log Entry
       end
       P->>I: CLOSE
       P->>O: CLOSE
       P->>L: CLOSE
   ```

6. **Variable Lifecycle State Diagram (stateDiagram-v2)** (for complex global variables):
   - Variable initialization
   - State transitions through processing
   - Final states and conditions

   ```mermaid
   stateDiagram-v2
       [*] --> Initialized: INITIALIZATION
       Initialized --> Processing: READ-RECORD
       Processing --> Validated: VALIDATE-DATA
       Processing --> Invalid: Validation Failed
       Validated --> Written: WRITE-OUTPUT
       Invalid --> Error: ERROR-HANDLING
       Written --> Processing: Next Record
       Error --> Processing: Next Record
       Processing --> [*]: END-OF-FILE
   ```

**Mermaid Generation Process**:

1. **Analyze Program Structure**: Use static analysis and documentation created in previous phases
2. **Generate Mermaid Code**: For each required diagram type, create valid Mermaid syntax
3. **Validate Rendering**: Test all Mermaid diagrams render correctly (use online editor: https://mermaid.live)
4. **Embed in Documentation**: Include Mermaid code blocks in dedicated diagrams document
5. **Cross-Reference**: Link diagrams from master index and relevant section documentation

**AI Prompt Template for Mermaid Generation**:
```
Using the call graph, data dictionary, and procedure documentation, generate Mermaid diagrams for this OMNISCRIPT program:

1. Program Flow (flowchart): Main processing logic with decision points
2. Call Hierarchy (graph): All procedure call relationships
3. Data Flow (flowchart): Input → Processing → Output flow
4. Module Dependencies (graph): All import statements and relationships
5. File I/O Timeline (sequenceDiagram): All file operations in order
6. Variable Lifecycles (stateDiagram-v2): For variables [list complex variables]

Ensure all Mermaid syntax is valid and diagrams are clear and readable.

Context:
[Insert call graph]
[Insert data dictionary summary]
[Insert file operations list]
[Insert import statements list]
```

**Output Structure**:
Create **mandatory** diagrams document: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_MERMAID_DIAGRAMS.md`

Document should contain:
- Brief introduction to each diagram type
- The Mermaid code block (renders automatically in most Markdown viewers)
- A text description of key insights from the diagram
- References to detailed documentation sections

**Example Diagrams Document Structure**:
```markdown
# [PROGRAM-NAME] Visual Diagrams

## 1. Program Flow

This flowchart shows the main processing logic from program start to end.

```mermaid
flowchart TD
    [... mermaid code ...]
```

**Key Insights**: 
- Main processing loop handles records until EOF
- Three validation stages before output
- Error handling redirects to logging procedure

## 2. Call Hierarchy

[... continue for all diagram types ...]
```

**Mermaid Best Practices for OMNISCRIPT**:
- Keep diagrams focused and readable (split large programs into multiple diagrams)
- Use consistent naming matching OMNISCRIPT procedure names
- Add annotations for complex logic (use `:::className` styling)
- Link diagram nodes to detailed documentation when possible
- Use subgraphs to group related procedures or processing sections
- Color-code critical paths, error handling, and I/O operations

**Tools and Resources**:
- **Mermaid Live Editor**: https://mermaid.live (validate syntax)
- **Mermaid Documentation**: https://mermaid.js.org/intro/
- **VS Code Extension**: Mermaid Preview (install for inline rendering)
- **GitHub/GitLab**: Native Mermaid rendering in Markdown files

### 4.7 Update Master Index
**Objective**: Link all comprehensive documentation back to the master index

**Actions**:
1. Update `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_INDEX.md` with links to:
   - Comprehensive program documentation
   - **OmniScript Upgrade Assessment (detected version to 7.5)**
   - Data dictionary
   - Call graph
   - Variable mutations
   - **Mermaid visual diagrams (MANDATORY)**
   - **Error handling analysis**
   - **Performance analysis**
   - **Testing guide**
   - **Integration guide**
   - **Business rules**
   - Cross-reference documentation
   - Validation report
   - Documentation standards applied

2. Add navigation paths for different user types:
   - **New developers**: Start here path (executive summary → architecture → Mermaid diagrams → key procedures)
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
│   │   ├── PROGRAM-A_INDEX.md
│   │   ├── PROGRAM-A_DATA_DICTIONARY.md
│   │   ├── PROGRAM-A_COMPREHENSIVE_DOC.md
│   │   ├── PROGRAM-A_CALL_GRAPH.md
│   │   ├── PROGRAM-A_VARIABLE_MUTATIONS.md
│   │   ├── PROGRAM-A_CROSS_REFERENCE.md
│   │   ├── PROGRAM-A_DIAGRAMS.md
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
- [ ] Each procedure documented individually (with error handling and performance notes)
- [ ] Call graph created showing all call relationships
- [ ] Variable mutation patterns identified and documented
- [ ] **Error handling analysis completed with risk assessment**
- [ ] **Performance analysis completed with bottleneck identification**
- [ ] **Testing guide created with edge cases and scenarios**

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

### Overall Success Indicators

**Immediate Success**:
- Documentation covers all program components (including error handling, performance, testing)
- Automated validation passes with high confidence scores
- Documentation is clear, complete, and self-consistent
- Documentation integrated into development workflow
- **Risk assessment completed for all file operations and buffers**
- **Performance bottlenecks identified and documented**
- **Testing scenarios comprehensive (standard, edge, error cases)**
- **Integration contracts fully specified**
- **Business rules explicitly documented**

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