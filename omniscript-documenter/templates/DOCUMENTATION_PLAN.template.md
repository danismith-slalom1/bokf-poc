# OMNISCRIPT Documentation Plan Template

This template is **MANDATORY** for all AI agents performing OMNISCRIPT documentation planning. Following this template structure is **REQUIRED**. Deviating from this template structure will result in termination of the agent.

## Template Structure

Use this exact template structure when creating OMNISCRIPT documentation plan documents:

```markdown
# OMNISCRIPT Documentation Plan - [Program Name] - [Date]

## Program Summary
- **Program**: [Program name]
- **Complexity**: [Simple/Moderate/Complex]
- **Lines of Code**: [Total LOC]
- **Documentation Approach**: [Minimal/Standard/Comprehensive]

## Phase 2: Iterative Documentation - Detailed Plan

### 2.1 Data Dictionary Generation

**Objective**: Document all program data structures and variables

**Scope**:
- Variable declarations lines: [X-Y]
- Total variables: [Count]
- Estimated AI interactions: [Number]

**Execution Plan**:
1. Extract variable declarations
2. Generate cross-reference context
3. AI prompt for data dictionary generation
4. Expert review of data dictionary entries
5. Corrections and iterations as needed

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_DATA_DICTIONARY.md` [Includes Variable Mutations section]

**Timeline**: [Estimated hours/days]

### 2.2 Procedure Documentation

**Objective**: Document each procedure individually

**Procedures to Document** (in sequence):
1. **[PROCEDURE-NAME-1]** (Lines [X-Y])
   - Purpose: [Brief description]
   - Complexity: [Simple/Moderate/Complex]
   - Dependencies: [Variables used, procedures called]
   - Estimated time: [Hours]

2. **[PROCEDURE-NAME-2]** (Lines [X-Y])
   - Purpose: [Brief description]
   - Complexity: [Simple/Moderate/Complex]
   - Dependencies: [Variables used, procedures called]
   - Estimated time: [Hours]

[Continue for all procedures]

**Chunking Strategy**:
- Group [N] related procedures per AI session
- Provide data dictionary context for each session
- Include call hierarchy for context

**Deliverables**: `${OMNISCRIPT_DOCS_DIR}/procedures/{PROCEDURE-NAME}.md` (one file per procedure or procedure group)

**Timeline**: [Estimated hours/days]

### 2.3 Call Graph Creation

**Objective**: Map all call relationships

**Scope**:
- Call statements: [Count]
- Maximum nesting depth: [Level]
- Loop structures: [Count]

**Execution Plan**:
1. Extract call hierarchy from static analysis
2. Feed to AI with procedure documentation context
3. Generate visual call graph
4. Expert review of control flow accuracy
5. Create Mermaid or PlantUML diagrams

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_CALL_GRAPH.md`

**Timeline**: [Estimated hours/days]

### 2.4 Variable Mutation Analysis

**Objective**: Track global variable state changes

**Scope**:
- High-mutation variables: [Count and names]
- Paragraphs modifying each: [Counts]

**Execution Plan**:
1. Identify variables modified in 3+ locations
2. Extract all procedures modifying each variable
3. AI analysis of mutation patterns
4. Expert review of state transition logic
5. Document recommendations for refactoring if needed

**Deliverable**: Variable Mutations section within `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_DATA_DICTIONARY.md`

**Timeline**: [Estimated hours/days]

## Phase 3: Expert Review - Detailed Plan

### 3.1 Expert Review Sessions

**Review Schedule**:
1. **Data Dictionary Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Variable purposes, data types, usage patterns

2. **Procedure Documentation Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Business logic accuracy, processing steps

3. **Call Graph Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Control flow accuracy, call relationships

4. **Mutation Analysis Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: State transitions, business logic

**Review Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_REVIEW_LOG.md`

### 3.2 Correction Iterations

**Process**:
1. Expert provides corrections
2. Update documentation with corrections
3. Re-submit unclear sections to AI with better context
4. Iterate until expert approves

**Expected Iterations**: [Number based on complexity]

### 3.3 Documentation Standards

**Standards to Apply**:
- Format: [Markdown with specific structure]
- Terminology: [OMNISCRIPT-specific conventions]
- Detail level: [Appropriate for program complexity]
- Cross-referencing: [How to link between documents]

**Standards Document**: `${OMNISCRIPT_DOCS_DIR}/DOCUMENTATION_STANDARDS.md`

## Phase 4: Synthesis - Detailed Plan

### 4.1 Program Overview Documentation

**Objective**: Create consolidated overview merging program index with comprehensive documentation

**Sections to Include**:
1. Executive Summary
2. Program Structure and Index
3. Business Context
4. Architecture Overview
5. Core Flow Diagrams (embedded)
6. Data Flow
7. Key Processing Logic
8. Dependencies
9. Error Handling
10. Maintenance Notes

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_OVERVIEW.md` [Consolidates INDEX + COMPREHENSIVE_DOC]

**Timeline**: [Estimated hours/days]

### 4.2 Cross-Reference Documentation

**Indices to Create**:
- Variable Index (alphabetical)
- Procedure Index (by location)
- File Operations Index
- Business Rule Index
- Error Handling Index

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_CROSS_REFERENCE.md`

**Timeline**: [Estimated hours/days]

### 4.3 Visual Diagrams

**Core Diagrams** (embedded in OVERVIEW.md):
1. **Program Flow Diagram** - High-level processing flow
2. **Call Hierarchy Tree** - Primary call relationships
3. **Data Flow Overview** - Input/output transformation

**Complex Diagrams** (separate DIAGRAMS.md):
1. **Detailed Data Flow** - Variable transformations
2. **File I/O Sequence** - File operations timeline
3. **State Machines** - Variable lifecycle diagrams
4. **Module Dependencies** - Integration visualization
5. [Additional diagrams as needed]

**Tools**: Mermaid syntax for all diagrams

**Deliverables**: 
- Core diagrams in `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_OVERVIEW.md`
- Complex diagrams in `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_DIAGRAMS.md`

**Timeline**: [Estimated hours/days]

### 4.4 Master Index Update

**Index Structure**:
- Navigation paths for different user types
- Links to all documentation artifacts
- Quick reference sections

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_INDEX.md` (updated)

**Timeline**: [Estimated hours]

## Phase 5: Maintenance Process - Detailed Plan

### 5.1 Maintenance Guide Creation

**Guide Contents**:
- When to update documentation
- How to use AI for updates
- Documentation quality standards
- Workflow integration

**Deliverable**: `${OMNISCRIPT_DOCS_DIR}/MAINTENANCE_GUIDE.md`

**Timeline**: [Estimated hours/days]

### 5.2 Team Training

**Training Components**:
- AI tool selection and setup
- Prompt engineering for OMNISCRIPT
- Review process training
- Common pitfalls and limitations

**Training Materials**: `${OMNISCRIPT_DOCS_DIR}/training/`

**Training Date**: [Scheduled date]

### 5.3 Documentation Repository Setup

**Repository Structure**:
```
omniscript-documentation/
├── {PROGRAM-NAME}/
│   ├── [All program docs]
│   └── procedures/
├── standards/
├── training/
└── README.md
```

**Version Control**: [Git integration plan]

### 5.4 Metrics and Quality Gates

**Metrics to Track**:
- Documentation coverage percentage
- Expert review approval rate
- Documentation update lag time
- Documentation-related production issues

**Quality Gates**:
- No code changes without doc updates
- Peer review includes doc review
- CI/CD documentation checks
- Quarterly documentation audits

## Resource Allocation

### Personnel
- **OMNISCRIPT Experts**: [Names/roles] - [Hours allocated]
- **Documentation Specialist**: [Name] - [Hours allocated]
- **AI Operator**: [Name] - [Hours allocated]

### Tools Required
- Static analysis tools: [List specific tools]
- AI tools: [List specific AI platforms]
- Diagram tools: [Mermaid, PlantUML, etc.]
- Version control: [Git repository location]

### Timeline Summary
- **Phase 2 (Iterative Documentation)**: [Duration]
- **Phase 3 (Expert Review)**: [Duration]
- **Phase 4 (Synthesis)**: [Duration]
- **Phase 5 (Maintenance Setup)**: [Duration]
- **Total Estimated Time**: [Total duration]

## Risk Mitigation

### Identified Risks
1. **Expert availability**: [Mitigation strategy]
2. **AI accuracy issues**: [Mitigation strategy]
3. **Context window limitations**: [Mitigation strategy]
4. **Documentation drift**: [Mitigation strategy]

## Success Criteria

For complete success criteria and quality gates, see [WORKFLOW.md](../WORKFLOW.md#success-criteria) in the omniscript-documenter module.

---
**IMPORTANT**: This plan must be reviewed and confirmed by project stakeholders before execution.

**CRITICAL**: This plan follows the workflow defined in WORKFLOW.md. All phases must be executed in sequence.
```

## Critical Requirements

### Content Requirements
- **Program Summary**: Must capture key program characteristics
- **Phase 2 Plan**: Must detail each documentation generation step with scope and timeline
- **Phase 3 Plan**: Must schedule expert review sessions with specific focus areas
- **Phase 4 Plan**: Must plan synthesis, cross-references, and diagrams
- **Phase 5 Plan**: Must establish ongoing maintenance processes
- **Resource Allocation**: Must identify personnel, tools, and timeline
- **Risk Mitigation**: Must identify and address potential issues

### Format Requirements
- Use exact markdown structure as shown above
- Include the program name and date in the title
- Include all mandatory sections
- Include the footer note about workflow context
- Maintain consistent section headers and formatting

### File Naming Convention
Save documentation plan documents as:
`${OMNISCRIPT_DOCS_DIR}/{PROGRAM-NAME}_DOCUMENTATION_PLAN.md`

## Mandatory Compliance

This template structure is **NON-NEGOTIABLE**. Agents must:
1. Follow the exact section structure
2. Include all required sections
3. Maintain consistent formatting
4. Include the contextual footer note
5. Use the specified file naming convention

**Failure to comply with this template will result in agent termination.**

## Usage Notes

- This document should be placed in `${OMNISCRIPT_DOCS_DIR}/`
- The documentation plan is created after Phase 1 analysis is complete
- Stakeholders should review and approve before execution begins
- This template enforces mandatory compliance for consistent planning
