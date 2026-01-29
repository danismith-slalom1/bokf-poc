# COBOL Documentation Plan Template

This template is **MANDATORY** for all AI agents performing COBOL documentation planning. Following this template structure is **REQUIRED**. Deviating from this template structure will result in termination of the agent.

## Template Structure

Use this exact template structure when creating COBOL documentation plan documents:

```markdown
# COBOL Documentation Plan - [Program Name] - [Date]

## Program Summary
- **Program**: [Program name]
- **Complexity**: [Simple/Moderate/Complex]
- **Lines of Code**: [Total LOC]
- **Documentation Approach**: [Minimal/Standard/Comprehensive]

## Phase 2: Iterative Documentation - Detailed Plan

### 2.1 Data Dictionary Generation

**Objective**: Document all WORKING-STORAGE variables

**Scope**:
- WORKING-STORAGE lines: [X-Y]
- Total variables: [Count]
- Estimated AI interactions: [Number]

**Execution Plan**:
1. Extract WORKING-STORAGE SECTION
2. Generate cross-reference context
3. AI prompt for data dictionary generation
4. Expert review of data dictionary entries
5. Corrections and iterations as needed

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_DATA_DICTIONARY.md`

**Timeline**: [Estimated hours/days]

### 2.2 Paragraph Documentation

**Objective**: Document each paragraph/section individually

**Paragraphs to Document** (in sequence):
1. **[PARAGRAPH-NAME-1]** (Lines [X-Y])
   - Purpose: [Brief description]
   - Complexity: [Simple/Moderate/Complex]
   - Dependencies: [Variables used, paragraphs called]
   - Estimated time: [Hours]

2. **[PARAGRAPH-NAME-2]** (Lines [X-Y])
   - Purpose: [Brief description]
   - Complexity: [Simple/Moderate/Complex]
   - Dependencies: [Variables used, paragraphs called]
   - Estimated time: [Hours]

[Continue for all paragraphs]

**Chunking Strategy**:
- Group [N] related paragraphs per AI session
- Provide data dictionary context for each session
- Include PERFORM hierarchy for context

**Deliverables**: `${COBOL_DOCS_DIR}/paragraphs/[PARAGRAPH-NAME].md` (one file per paragraph or paragraph group)

**Timeline**: [Estimated hours/days]

### 2.3 Call Graph Creation

**Objective**: Map all PERFORM relationships

**Scope**:
- PERFORM statements: [Count]
- Maximum nesting depth: [Level]
- Loop structures: [Count]

**Execution Plan**:
1. Extract PERFORM hierarchy from static analysis
2. Feed to AI with paragraph documentation context
3. Generate visual call graph
4. Expert review of control flow accuracy
5. Create Mermaid or PlantUML diagrams

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_CALL_GRAPH.md`

**Timeline**: [Estimated hours/days]

### 2.4 Variable Mutation Analysis

**Objective**: Track global variable state changes

**Scope**:
- High-mutation variables: [Count and names]
- Paragraphs modifying each: [Counts]

**Execution Plan**:
1. Identify variables modified in 3+ locations
2. Extract all paragraphs modifying each variable
3. AI analysis of mutation patterns
4. Expert review of state transition logic
5. Document recommendations for refactoring if needed

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_VARIABLE_MUTATIONS.md`

**Timeline**: [Estimated hours/days]

## Phase 3: Expert Review - Detailed Plan

### 3.1 Expert Review Sessions

**Review Schedule**:
1. **Data Dictionary Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Variable purposes, data types, usage patterns

2. **Paragraph Documentation Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Business logic accuracy, processing steps

3. **Call Graph Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: Control flow accuracy, PERFORM relationships

4. **Mutation Analysis Review** - [Date/Time]
   - Reviewers: [Names]
   - Focus: State transitions, business logic

**Review Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_REVIEW_LOG.md`

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
- Terminology: [COBOL-specific conventions]
- Detail level: [Appropriate for program complexity]
- Cross-referencing: [How to link between documents]

**Standards Document**: `${COBOL_DOCS_DIR}/DOCUMENTATION_STANDARDS.md`

## Phase 4: Synthesis - Detailed Plan

### 4.1 Comprehensive Program Documentation

**Objective**: Synthesize all component docs into master documentation

**Sections to Include**:
1. Executive Summary
2. Business Context
3. Architecture Overview
4. Data Flow
5. Key Processing Logic
6. Dependencies
7. Error Handling
8. Maintenance Notes

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_COMPREHENSIVE_DOC.md`

**Timeline**: [Estimated hours/days]

### 4.2 Cross-Reference Documentation

**Indices to Create**:
- Variable Index (alphabetical)
- Paragraph Index (by location)
- File Operations Index
- Business Rule Index
- Error Handling Index

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_CROSS_REFERENCE.md`

**Timeline**: [Estimated hours/days]

### 4.3 Visual Diagrams

**Diagrams to Create**:
1. **Program Flow Diagram** - High-level processing flow
2. **Data Flow Diagram** - Data transformations
3. **PERFORM Hierarchy Tree** - Call graph visualization
4. **File I/O Flow** - File operations in sequence
5. [Additional diagrams as needed]

**Tools**: Mermaid, PlantUML, or hand-drawn with photographs

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_DIAGRAMS.md`

**Timeline**: [Estimated hours/days]

### 4.4 Master Index Update

**Index Structure**:
- Navigation paths for different user types
- Links to all documentation artifacts
- Quick reference sections

**Deliverable**: `${COBOL_DOCS_DIR}/[PROGRAM-NAME]_INDEX.md` (updated)

**Timeline**: [Estimated hours]

## Phase 5: Maintenance Process - Detailed Plan

### 5.1 Maintenance Guide Creation

**Guide Contents**:
- When to update documentation
- How to use AI for updates
- Documentation quality standards
- Workflow integration

**Deliverable**: `${COBOL_DOCS_DIR}/MAINTENANCE_GUIDE.md`

**Timeline**: [Estimated hours/days]

### 5.2 Team Training

**Training Components**:
- AI tool selection and setup
- Prompt engineering for COBOL
- Review process training
- Common pitfalls and limitations

**Training Materials**: `${COBOL_DOCS_DIR}/training/`

**Training Date**: [Scheduled date]

### 5.3 Documentation Repository Setup

**Repository Structure**:
```
cobol-documentation/
├── [PROGRAM-NAME]/
│   ├── [All program docs]
│   └── paragraphs/
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
- **COBOL Experts**: [Names/roles] - [Hours allocated]
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

### Phase Completion Criteria
- [ ] All WORKING-STORAGE variables documented
- [ ] All paragraphs individually documented
- [ ] Call graph created and verified
- [ ] Variable mutations analyzed
- [ ] Expert reviews completed with approval
- [ ] Comprehensive documentation synthesized
- [ ] Cross-references created
- [ ] Visual diagrams generated
- [ ] Maintenance process established

### Quality Criteria
- [ ] COBOL experts approve all documentation
- [ ] New developers can understand program from docs
- [ ] Documentation integrated into workflow
- [ ] Metrics tracking in place

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
`${COBOL_DOCS_DIR}/[PROGRAM-NAME]_DOCUMENTATION_PLAN.md`

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
- The documentation plan is created after Phase 1 analysis is complete
- Stakeholders should review and approve before execution begins
- This template enforces mandatory compliance for consistent planning
