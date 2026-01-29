# COBOL Documentation Standards Template

This template is **MANDATORY** for establishing consistent COBOL documentation quality across all programs. Following these standards is **REQUIRED**.

## Template Structure

Use this exact template structure when creating COBOL documentation standards:

```markdown
# COBOL Documentation Standards - [Organization/Project Name] - [Date]

## Purpose and Scope

### Purpose
This document defines the documentation standards for COBOL programs to ensure consistency, quality, and maintainability across all documentation efforts.

### Scope
These standards apply to:
- Data dictionary documentation
- Paragraph/section documentation
- Call graph documentation
- Variable mutation analysis
- Comprehensive program documentation
- All AI-generated and human-reviewed documentation

## Documentation Format Standards

### File Format
- **Format**: Markdown (.md)
- **Encoding**: UTF-8
- **Line Endings**: [Unix LF / Windows CRLF - specify]
- **Line Length**: Maximum 120 characters (except for code blocks)

### Naming Conventions
- **Program Analysis**: `[PROGRAM-NAME]_ANALYSIS.md`
- **Data Dictionary**: `[PROGRAM-NAME]_DATA_DICTIONARY.md`
- **Paragraph Docs**: `paragraphs/[PARAGRAPH-NAME].md`
- **Call Graph**: `[PROGRAM-NAME]_CALL_GRAPH.md`
- **Mutation Analysis**: `[PROGRAM-NAME]_VARIABLE_MUTATIONS.md`
- **Comprehensive Doc**: `[PROGRAM-NAME]_COMPREHENSIVE_DOC.md`
- **Cross-Reference**: `[PROGRAM-NAME]_CROSS_REFERENCE.md`
- **Error Handling**: `[PROGRAM-NAME]_ERROR_HANDLING.md`
- **Performance Analysis**: `[PROGRAM-NAME]_PERFORMANCE_ANALYSIS.md`
- **Testing Guide**: `[PROGRAM-NAME]_TESTING_GUIDE.md`
- **Integration Guide**: `[PROGRAM-NAME]_INTEGRATION_GUIDE.md`
- **Business Rules**: `[PROGRAM-NAME]_BUSINESS_RULES.md`
- **Mermaid Diagrams**: `[PROGRAM-NAME]_MERMAID_DIAGRAMS.md`
- **Review Log**: `[PROGRAM-NAME]_REVIEW_LOG.md`

### Document Structure
Each documentation file must include:
1. **Title**: Clear heading with program/component name
2. **Last Updated**: Date and author of last update
3. **Purpose**: Brief statement of document purpose
4. **Content Sections**: Organized with clear headings
5. **Cross-References**: Links to related documentation
6. **Review Status**: Expert review status and approval date

## Data Dictionary Standards

### Entry Structure
For each WORKING-STORAGE variable, document:

```markdown
#### [VARIABLE-NAME]
- **Level**: [01, 05, 10, etc.]
- **Picture**: [PIC clause]
- **Type**: [Alphanumeric/Numeric/Packed Decimal/etc.]
- **Size**: [Bytes]
- **Initial Value**: [VALUE clause or "Not initialized"]
- **Purpose**: [Clear description of what this variable represents]
- **Usage Pattern**: [Where read/modified based on cross-reference]
- **Related Variables**: [Variables in same group or related processing]
- **Special Handling**: [REDEFINES, OCCURS, COMP notes]
```

### Quality Standards
- **Clarity**: Purpose must be understandable to non-COBOL developers
- **Completeness**: All fields in entry structure must be filled
- **Accuracy**: Expert must verify all technical details
- **Business Context**: Include business meaning, not just technical description

### Examples

**Good Example**:
```markdown
#### WS-CUSTOMER-ID
- **Level**: 05
- **Picture**: PIC X(10)
- **Type**: Alphanumeric
- **Size**: 10 bytes
- **Initial Value**: Not initialized
- **Purpose**: Unique identifier for customer records, populated from input file
- **Usage Pattern**: Read in PROCESS-INPUT-RECORD, used in VALIDATE-CUSTOMER, written to output in WRITE-REPORT-LINE
- **Related Variables**: Part of WS-CUSTOMER-RECORD group
- **Special Handling**: Must be left-justified with trailing spaces
```

**Poor Example**:
```markdown
#### WS-CUSTOMER-ID
- Customer ID field
```

## Paragraph Documentation Standards

### Entry Structure
For each paragraph, document:

```markdown
# [PARAGRAPH-NAME]

**Location**: Lines [X-Y]
**Called By**: [List of paragraphs that PERFORM this one]
**Calls**: [List of paragraphs this one PERFORMs]

## Purpose
[1-2 sentence description of what this paragraph does]

## Business Logic
[Detailed explanation of the business rules implemented]

## Processing Steps
1. [First step with explanation]
2. [Second step with explanation]
3. [Continue for all major steps]

## Variables Used
- **Read**: [List variables read/checked]
- **Modified**: [List variables modified]

## File Operations
[Any READ, WRITE, REWRITE, DELETE operations]

## Error Handling
[How errors are detected and handled]
- FILE STATUS codes checked (or note absence)
- Input validation performed
- Error recovery procedures
- Error message generation
- Fail-safe mechanisms

## Performance Considerations
[Performance characteristics of this paragraph]
- Loop complexity and iteration counts
- STRING/UNSTRING operation costs
- File I/O patterns
- Memory usage
- Performance bottlenecks identified

## Security Considerations
[Security aspects of this paragraph]
- Input validation and sanitization
- Buffer overflow protection
- Resource limit enforcement
- Access control requirements

## Testing Scenarios
[Key test cases for this paragraph]
- Standard test case
- Edge cases (empty, maximum, boundary values)
- Error conditions
- Integration test requirements

## Dependencies
[External programs called, COPY books used, special conditions required]

## Notes
[Any special considerations, edge cases, or technical debt]
```

### Quality Standards
- **Business Focus**: Explain WHY, not just WHAT
- **Step-by-Step**: Break complex logic into clear steps
- **Context**: Include enough context to understand standalone
- **Accuracy**: Expert must verify business logic interpretation

### Examples

**Good Example**:
```markdown
# VALIDATE-CUSTOMER-RECORD

**Location**: Lines 450-520
**Called By**: PROCESS-INPUT-RECORD
**Calls**: CHECK-REQUIRED-FIELDS, VALIDATE-CUSTOMER-ID

## Purpose
Validates customer record data meets business rules before processing transaction

## Business Logic
Per business policy BP-2024-15, customer records must have:
- Valid customer ID (10 digits, registered in system)
- Complete name (first and last, no special characters)
- Valid transaction amount (positive, max $99,999.99)

## Processing Steps
1. Call CHECK-REQUIRED-FIELDS to verify all mandatory fields populated
2. Call VALIDATE-CUSTOMER-ID against customer master file
3. Check name fields for special characters using INSPECT
4. Validate transaction amount is positive and within limits
5. Set WS-VALIDATION-STATUS to 'PASS' or 'FAIL'
6. If failed, populate WS-ERROR-MESSAGE with specific error

## Variables Used
- **Read**: WS-CUSTOMER-ID, WS-CUSTOMER-FNAME, WS-CUSTOMER-LNAME, WS-TRANSACTION-AMOUNT
- **Modified**: WS-VALIDATION-STATUS, WS-ERROR-MESSAGE

## Error Handling
Sets WS-VALIDATION-STATUS to 'FAIL' and populates WS-ERROR-MESSAGE with:
- "ERR001: Missing required fields" 
- "ERR002: Invalid customer ID"
- "ERR003: Invalid name format"
- "ERR004: Invalid transaction amount"

## Dependencies
- Requires CUSTOMER-MASTER-FILE to be open
- Uses COPY book VALIDATION-CODES

## Notes
- Special character validation uses ASCII values 32-126 only
- Technical debt: Should be refactored to use table-driven validation
```

## Call Graph Standards

### Visualization Format
- **Tool**: Mermaid diagram or PlantUML
- **Direction**: Top-to-bottom for main flow
- **Node Labels**: Paragraph names
- **Edge Labels**: Conditional PERFORMs noted
- **Color Coding**: Entry points (green), loops (blue), error handlers (red)

### Quality Standards
- **Completeness**: All PERFORM relationships shown
- **Accuracy**: Expert verifies control flow
- **Readability**: No more than 20 nodes per diagram (split large programs)
- **Annotations**: Loop conditions and special flows noted

## Variable Mutation Analysis Standards

### Entry Structure
For each high-mutation variable:

```markdown
## [VARIABLE-NAME]

**Definition**: [Location and PIC clause]
**Modification Count**: [Number of paragraphs that modify it]

### Initialization
- **Paragraph**: [Where initialized]
- **Initial Value**: [Value]
- **Line**: [Line number]

### Mutations (in execution order)
1. **[PARAGRAPH-1]** (Line [X])
   - **Trigger**: [What causes this modification]
   - **Operation**: [SET, MOVE, ADD, COMPUTE, etc.]
   - **New Value**: [What it becomes]
   - **Business Logic**: [Why this change happens]

2. **[PARAGRAPH-2]** (Line [Y])
   [Same structure]

### State Transitions
[Description of how this variable's value changes throughout program execution]

### Concerns
[Any potential issues: race conditions, unexpected mutations, refactoring recommendations]
```

### Quality Standards
- **Execution Order**: Mutations documented in likely execution order
- **Business Context**: Why each mutation occurs
- **Completeness**: All modification points identified
- **Analysis**: Potential issues noted

## Comprehensive Documentation Standards

### Structure
1. **Executive Summary** (1-2 paragraphs)
2. **Business Context** (what problem this solves)
3. **Architecture Overview** (major sections and their purposes)
4. **Data Flow** (inputs → processing → outputs)
5. **Key Processing Logic** (critical algorithms and business rules)
6. **Dependencies** (external programs, files, COPY books)
7. **Error Handling** (how errors are detected and handled)
8. **Maintenance Notes** (technical debt, refactoring suggestions, gotchas)

### Quality Standards
- **Audience**: Written for developers new to the program
- **Length**: 5-10 pages typical for standard programs
- **Detail Level**: High-level with references to detailed docs
- **Navigation**: Clear section headers and table of contents

## Expert Review Standards

### Review Criteria

#### Data Dictionary Review
- [ ] All variable purposes accurately described
- [ ] Data types correctly interpreted
- [ ] Usage patterns verified against code
- [ ] Business context provided for each variable

#### Paragraph Documentation Review
- [ ] Business logic correctly explained
- [ ] Processing steps match actual execution
- [ ] Error handling accurately documented
- [ ] Dependencies correctly identified

#### Call Graph Review
- [ ] PERFORM relationships correctly mapped
- [ ] Conditional execution paths accurate
- [ ] Loop structures properly documented

#### Mutation Analysis Review
- [ ] State transitions correctly described
- [ ] Execution order assumptions valid
- [ ] Business logic interpretations accurate
- [ ] All mutation points identified

### Review Process
1. **Initial Review**: Expert reads AI-generated documentation
2. **Error Logging**: Expert documents specific errors in review log
3. **Correction**: Documentation updated based on feedback
4. **Re-Review**: Expert verifies corrections
5. **Approval**: Expert signs off when satisfied

### Review Documentation
Document in `[PROGRAM-NAME]_REVIEW_LOG.md`:
- Date of review
- Reviewer name and role
- Errors found (categorized)
- Corrections made
- Approval status and date

## AI-Generated Documentation Guidelines

### AI Tool Selection
- **Recommended Tools**: [List specific AI platforms used]
- **Context Window**: Must be sufficient for chunk size
- **COBOL Knowledge**: Verify AI understands COBOL syntax

### Prompt Engineering
- **Context Provision**: Always include static analysis reports
- **Specificity**: Request specific format and detail level
- **Business Focus**: Ask for business logic explanation, not just technical
- **Examples**: Provide examples of good documentation

### Quality Checks
After AI generation, verify:
- [ ] No COBOL syntax misinterpretations
- [ ] Business logic not purely guessed
- [ ] All sections of template populated
- [ ] Cross-references accurate
- [ ] Technical terms used correctly

## Error Handling Documentation Standards

### Entry Structure
For error handling analysis:

```markdown
# [PROGRAM-NAME] Error Handling and Risk Analysis

## FILE STATUS Analysis
- **FILE STATUS clauses present**: [Yes/No]
- **Files with error handling**: [List]
- **Files without error handling**: [List - HIGH RISK]

## Runtime Error Scenarios
| Error Scenario | Detection Mechanism | Recovery Procedure | Risk Level |
|----------------|---------------------|-------------------|------------|
| File not found | [Mechanism or "None"] | [Procedure or "No recovery"] | [High/Med/Low] |
| Access denied | [Mechanism or "None"] | [Procedure or "No recovery"] | [High/Med/Low] |
| Buffer overflow | [Mechanism or "None"] | [Procedure or "No recovery"] | [High/Med/Low] |
| Invalid data | [Mechanism or "None"] | [Procedure or "No recovery"] | [High/Med/Low] |

## Resource Limits
- Maximum file size: [Size or "Not validated"]
- Buffer size limits: [Sizes and overflow risk assessment]
- Array bounds: [OCCURS limits and checking]
- Token/record limits: [Limits and enforcement]

## Input Validation
- File path validation: [Yes/No - describe mechanism]
- Data format validation: [Yes/No - describe mechanism]
- Range checking: [Yes/No - describe mechanism]
- Special character handling: [Yes/No - describe mechanism]

## Risk Assessment Summary
- **High Risk Items**: [List items requiring immediate attention]
- **Medium Risk Items**: [List items for future enhancement]
- **Low Risk Items**: [List acceptable risk areas]

## Recommendations
1. [Priority 1 recommendation]
2. [Priority 2 recommendation]
```

### Quality Standards
- **Honesty**: Document absence of error handling as risk, don't fabricate
- **Completeness**: Cover all file operations, buffer operations, arithmetic
- **Risk Rating**: Use consistent criteria (High=no checking, Med=partial, Low=comprehensive)
- **Actionable**: Recommendations must be specific and implementable

## Performance Analysis Standards

### Entry Structure
For performance analysis:

```markdown
# [PROGRAM-NAME] Performance Analysis

## Operation Cost Analysis
| Operation Type | Frequency | Estimated Cost | Impact |
|----------------|-----------|----------------|--------|
| STRING operations | [Count] | High/Med/Low | [Description] |
| Loop iterations | [Max count] | High/Med/Low | [Description] |
| File I/O operations | [Count] | High/Med/Low | [Description] |
| Nested PERFORMs | [Depth] | High/Med/Low | [Description] |

## Memory Usage Analysis
- **Total WORKING-STORAGE**: [Bytes]
- **Large buffers (>500 bytes)**: [List with sizes]
- **Temporary vs persistent**: [Breakdown]
- **Memory efficiency**: [Assessment]

## Scalability Limits
- **Maximum records/tokens**: [Hard-coded limit]
- **Maximum file size**: [Based on buffer sizes]
- **Processing time estimate**: [Small/Med/Large file estimates]
- **Concurrent execution**: [Safe/Unsafe/Requires coordination]

## Performance Bottlenecks
1. **[Bottleneck description]**
   - Location: [Paragraph/Lines]
   - Impact: High/Med/Low
   - Explanation: [Why it's expensive]
   
## Optimization Recommendations
1. **[Recommendation]**
   - Current approach: [Description]
   - Suggested approach: [Alternative]
   - Expected improvement: [Estimate]
   - Implementation complexity: High/Med/Low
```

### Quality Standards
- **Data-driven**: Base analysis on actual code, not assumptions
- **Relative costs**: Use High/Med/Low consistently
- **Context**: Consider typical file sizes and usage patterns
- **Realistic**: Don't recommend premature optimization

## Testing Guide Standards

### Entry Structure
For testing documentation:

```markdown
# [PROGRAM-NAME] Testing Guide

## Standard Test Cases
| Test Case | Input Description | Expected Output | Test Data Location |
|-----------|-------------------|-----------------|-------------------|
| [Name] | [Description] | [Expected result] | [Path/file] |

## Edge Case Test Scenarios
| Test Case | Scenario | Expected Behavior | Priority |
|-----------|----------|-------------------|----------|
| Empty file | Zero records | [Behavior] | High/Med/Low |
| Maximum size | At buffer limit | [Behavior] | High/Med/Low |
| Boundary value | [Description] | [Behavior] | High/Med/Low |

## Error Condition Tests
| Test Case | Error Condition | Expected Error Handling | Validation |
|-----------|-----------------|------------------------|------------|
| [Name] | [Condition] | [How program should respond] | [How to verify] |

## Integration Test Scenarios
1. **[Scenario name]**
   - Setup: [Required state]
   - Execution: [Steps]
   - Verification: [Expected results]

## Sample Test Data
[Provide or reference sample input files and expected output]
```

### Quality Standards
- **Coverage**: Include standard, edge, and error cases
- **Specificity**: Test cases must be reproducible
- **Prioritization**: Mark critical tests as High priority
- **Completeness**: Cover all major code paths

## Integration Guide Standards

### Entry Structure
For integration documentation:

```markdown
# [PROGRAM-NAME] Integration Guide

## Called Programs
| Program Name | Purpose | Parameters | Return Codes | Error Handling |
|--------------|---------|------------|--------------|----------------|
| [Name] | [Purpose] | [List params] | [Codes] | [How handled] |

## Calling Program Contract (Entry Point)
**Parameters (LINKAGE SECTION)**:
- [Parameter 1]: [Type, Direction (IN/OUT/INOUT), Purpose]
- [Parameter 2]: [Type, Direction, Purpose]

**Pre-conditions**:
- [Required state before calling]

**Post-conditions**:
- [Guaranteed state after execution]

**Usage Example**:
```cobol
[Show how to call this program]
```

## File Dependencies
| File Name | Type | Format | Access Required | Purpose |
|-----------|------|--------|-----------------|---------|
| [Name] | Input/Output | [Description] | Read/Write | [Purpose] |

## Copybook Dependencies
| Copybook Name | Contents | Purpose | Version |
|---------------|----------|---------|---------|
| [Name] | [Data structures] | [Why needed] | [Version info] |

## System Requirements
- **COBOL Compiler**: [Name and version]
- **Operating System**: [OS requirements]
- **Compile Flags**: [Required flags]
- **Runtime Settings**: [Environment variables, etc.]

## Deployment
1. [Step 1]
2. [Step 2]
```

### Quality Standards
- **Complete interface specs**: Document all parameters fully
- **Examples**: Provide usage examples for calling programs
- **Version info**: Include compiler and system version requirements
- **Deployment ready**: Include actual deployment steps

## Business Rules Standards

### Entry Structure
For business rules documentation:

```markdown
# [PROGRAM-NAME] Business Rules

## Explicit Business Rules
| Rule ID | Rule Description | Implementation Location | Validation |
|---------|------------------|------------------------|------------|
| BR-001 | [Rule statement] | [Paragraph, lines] | [How enforced] |

## Implicit Business Rules
| Rule | Inferred From | Business Justification |
|------|---------------|------------------------|
| [Rule] | [Code pattern] | [Why this rule exists] |

## Business Constraints
- **[Constraint name]**: [Description and enforcement]

## Security Requirements
| Requirement | Implementation | Risk Level |
|-------------|----------------|------------|
| Input validation | [How implemented] | High/Med/Low |
| Resource limits | [How enforced] | High/Med/Low |
| Audit logging | [What's logged] | High/Med/Low |

## Compliance and Audit
- **Regulatory requirements**: [List if applicable]
- **Audit trail**: [What's recorded and where]
- **Data retention**: [Rules for keeping data]
```

### Quality Standards
- **Traceability**: Link rules to code locations
- **Business language**: Explain in business terms, not just technical
- **Validation**: Document how each rule is enforced
- **Completeness**: Extract all rules, even implicit ones

## Terminology Standards

### COBOL-Specific Terms
- **Paragraph**: Not "function" or "method"
- **PERFORM**: Not "call" (unless CALL statement)
- **WORKING-STORAGE**: Not "variables section"
- **PIC clause**: Not "type definition"
- **Level number**: Not "indentation level"

### Business Terms
[Organization-specific business terminology]

### Abbreviations
Document all abbreviations used:
- WS: WORKING-STORAGE
- LS: LINKAGE SECTION
- FD: File Description
- [Add organization-specific abbreviations]

## Maintenance and Updates

### When to Update Documentation
- After any code modification
- After adding new paragraphs or variables
- After changing PERFORM relationships
- Before production release

### Update Process
1. Identify changed sections
2. Use AI with original docs + changed code + change reason
3. Expert reviews updates
4. Update cross-references
5. Update master index
6. Update "Last Modified" date

### Version Control
- Commit documentation with code changes
- Link doc commits to code commits
- Tag documentation versions with releases

## Metrics and Quality Gates

### Coverage Metrics
- **Target**: 100% of paragraphs documented
- **Target**: 100% of WORKING-STORAGE documented
- **Target**: All PERFORM relationships mapped

### Quality Metrics
- **Target**: 100% expert approval rate (after corrections)
- **Target**: Documentation updated within 48 hours of code change
- **Target**: Zero documentation-related production issues

### Quality Gates
- [ ] No code promotion without updated documentation
- [ ] Peer review must include documentation review
- [ ] CI/CD checks for documentation completeness

---
**IMPORTANT**: These standards must be followed by all team members and AI tools generating COBOL documentation.

**REVISION**: This document should be reviewed and updated quarterly based on lessons learned.
```

## Critical Requirements

### Content Requirements
- **Format Standards**: Must define file formats, naming conventions, structure
- **Component Standards**: Must provide standards for each documentation type (data dictionary, paragraphs, etc.)
- **Quality Standards**: Must define review criteria and quality metrics
- **Examples**: Must include both good and poor examples
- **Maintenance**: Must define update processes and version control

### Format Requirements
- Use exact markdown structure as shown above
- Include organization name and date in title
- Include all mandatory sections
- Provide concrete examples
- Maintain consistent section headers and formatting

### File Naming Convention
Save documentation standards as:
`${COBOL_DOCS_DIR}/DOCUMENTATION_STANDARDS.md`

## Mandatory Compliance

This template structure is **NON-NEGOTIABLE**. Organizations must:
1. Follow the exact section structure
2. Include all required sections
3. Customize examples for their context
4. Maintain consistent formatting
5. Review and update regularly

**Failure to establish clear standards will result in inconsistent documentation quality.**

## Usage Notes

- This document should be placed in `${COBOL_DOCS_DIR}/standards/`
- Standards should be established early in Phase 3
- All team members and AI tools must follow these standards
- Standards should be living document, updated based on experience
