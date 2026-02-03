This is a configuration file to aid with the automation of the OMNISCRIPT documentation process. Below you will find configuration settings and guidance for documenting OMNISCRIPT programs using AI assistance.

**Configuration Settings**:

## Static Analysis Tools

- **Primary Tool**: [Specify OMNISCRIPT interpreter or static analysis tool]
  - Cross-reference generation: [Command/flag to generate cross-reference]
  - Call graph generation: [Command/flag to generate call graph]
  - Output format: [Format of analysis reports]

- **Alternative Tools**: [List backup or supplementary tools]

## OMNISCRIPT Environment

- **OMNISCRIPT Version**: [Interpreter version or dialect]
- **Interpreter**: [Standard interpreter or custom implementation]
- **Platform**: [Unix, Linux, Windows, etc.]
- **Specific Features**: [Any implementation-specific considerations]

## Documentation Priorities

- **Program Selection Criteria**: [How to prioritize which programs to document first]
  - Business criticality (high/medium/low)
  - Complexity (simple/moderate/complex)
  - Maintenance frequency (frequent/occasional/rare)
  - Current documentation state (none/minimal/outdated)
  - Error handling maturity (none/partial/comprehensive)
  - Performance criticality (high-volume/standard/low-volume)

- **Documentation Depth by Program Type**:
  - **Mission-critical**: Comprehensive documentation (all phases + all supplemental docs)
    - **REQUIRED FILES**: Overview, Data dictionary (with mutations), Call graph, Diagrams, Error handling, Integration guide, Business rules, Cross reference, Validation report, Procedures (all)
    - **OPTIONAL**: Performance analysis
  - **Standard business logic**: Standard documentation (overview + data dictionary + procedures + call graph + essentials)
    - **REQUIRED FILES**: Overview, Data dictionary (with mutations), Call graph, Diagrams, Error handling, Integration guide, Business rules, Cross reference, Validation report, Procedures (key)
    - **OPTIONAL**: Performance analysis, Testing guide (basic)
  - **Simple utilities**: Minimal documentation (overview + key sections)
    - **REQUIRED FILES**: Overview (with embedded flow diagram), Data dictionary, Procedures (key)
    - **OPTIONAL**: Error handling assessment (if file I/O present)

**DEFAULT DOCUMENTATION LEVEL**: If not specified, use **Standard business logic** level for all programs.

## Output Directory Structure

**Standard documentation location**: All OMNISCRIPT documentation must follow this directory structure:

```
omniscript-documentation/
└── {REPO-NAME}/
    └── {PROGRAM-NAME}/
        ├── {PROGRAM}_OVERVIEW.md                  [REQUIRED: Merged INDEX + COMPREHENSIVE_DOC]
        ├── {PROGRAM}_DATA_DICTIONARY.md           [REQUIRED: Includes Variable Mutations section]
        ├── {PROGRAM}_CALL_GRAPH.md                [REQUIRED]
        ├── {PROGRAM}_DIAGRAMS.md                  [REQUIRED: Complex visualizations + Mermaid diagrams]
        ├── {PROGRAM}_ERROR_HANDLING.md            [REQUIRED: See Phase 3.1 in WORKFLOW.md]
        ├── {PROGRAM}_INTEGRATION_GUIDE.md         [REQUIRED: See Phase 4.3 in WORKFLOW.md]
        ├── {PROGRAM}_BUSINESS_RULES.md            [REQUIRED: See Phase 4.4 in WORKFLOW.md]
        ├── {PROGRAM}_CROSS_REFERENCE.md           [REQUIRED: See Phase 4.2 in WORKFLOW.md]
        ├── {PROGRAM}_VALIDATION_REPORT.md         [REQUIRED: See Phase 3.1 in WORKFLOW.md]
        └── procedures/
            ├── PROCEDURE-1.md
            ├── PROCEDURE-2.md
            └── ...
```

**Path Naming Conventions**:
- Repository name: Extract from repository or project folder name
- Program name: Extract from OmniScript file name without extension (e.g., `PAYROLL.os` → `PAYROLL`)
- Use uppercase for program names in directory paths
- Use underscores to separate program name from document type

**For complete file naming conventions and documentation standards**, see [templates/DOCUMENTATION_STANDARDS.template.md](./templates/DOCUMENTATION_STANDARDS.template.md#naming-conventions).

**Example paths**:
- Overview: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_OVERVIEW.md`
- Data dictionary: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_DATA_DICTIONARY.md`
- Call graph: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_CALL_GRAPH.md`
- Diagrams: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_DIAGRAMS.md`
- Procedure: `omniscript-documentation/my-repo/PAYROLL/procedures/CALCULATE-TAX.md`

## Documentation Enhancement Settings

### Error Handling Documentation (MANDATORY)
- **Always document**: **YES** (REQUIRED for all programs)
- **Risk assessment required**: **YES** (REQUIRED)
- **Error status analysis depth**: Comprehensive
- **Recovery procedure documentation**: YES
- **Output file**: `{PROGRAM}_ERROR_HANDLING.md`
- **See**: WORKFLOW.md Phase 3.1 for validation process

### Comprehensive Code Quality Assessment
- **Template**: [CODE_QUALITY_ASSESSMENT.template.md](./templates/CODE_QUALITY_ASSESSMENT.template.md)
- **When to use**: After Phase 5 completion, or standalone for existing code
- **Scope**: Security, operational risk, best practices, quality gates (Sections A-F)
- **Output file**: `{PROGRAM}_QUALITY_ASSESSMENT.md`
- **See**: WORKFLOW.md "Comprehensive Code Quality Assessment" section

### Integration Documentation (MANDATORY)
- **Document all external calls**: **YES** (REQUIRED)
- **Entry point contract**: **Always** (REQUIRED)
- **Deployment guide**: Full (for mission-critical), Basic (for standard)
- **System requirements**: Detailed
- **Output file**: `{PROGRAM}_INTEGRATION_GUIDE.md`
- **See**: WORKFLOW.md Phase 4.3 for requirements

### Business Rules Extraction (MANDATORY)
- **Extract explicit rules**: **YES** (REQUIRED)
- **Infer implicit rules**: **YES** (REQUIRED)
- **Security requirements**: **Document** (REQUIRED)
- **Compliance documentation**: YES (if applicable)
- **Output file**: `{PROGRAM}_BUSINESS_RULES.md`
- **See**: WORKFLOW.md Phase 4.4 for extraction process

### Cross-Reference Documentation (MANDATORY)
- **Variable index**: **YES** (REQUIRED)
- **Procedure index**: **YES** (REQUIRED)
- **File operations index**: **YES** (REQUIRED)
- **Business rule index**: **YES** (REQUIRED)
- **Error handling index**: **YES** (REQUIRED)
- **Output file**: `{PROGRAM}_CROSS_REFERENCE.md`
- **See**: WORKFLOW.md Phase 4.2 for generation

### Validation Report (MANDATORY)
- **Self-validation**: **YES** (REQUIRED)
- **Confidence scores**: **YES** (REQUIRED)
- **Assumption documentation**: **YES** (REQUIRED)
- **Alternative interpretations**: YES (where applicable)
- **Output file**: `{PROGRAM}_VALIDATION_REPORT.md`
- **See**: WORKFLOW.md Phase 3.1 for validation process

### Data Flow Diagrams (MANDATORY)
- **Generate for all programs**: **YES** (REQUIRED)
- **Variable lifecycle diagrams**: All major variables (REQUIRED)
- **Transformation examples**: **YES** (REQUIRED)
- **Mermaid diagrams**: **YES** (MANDATORY - see MERMAID_GUIDE.md)
- **Output file**: `{PROGRAM}_DIAGRAMS.md`

### Performance Analysis (OPTIONAL)
- **Document for critical programs**: YES (for mission-critical only)
- **String operation analysis**: YES (if applicable)
- **Memory usage analysis**: YES (if applicable)
- **Optimization recommendations**: YES (if applicable)
- **Output file**: `{PROGRAM}_PERFORMANCE_ANALYSIS.md` (optional)

### Testing Guide Creation (OPTIONAL)
- **Always generate**: NO (optional for standard programs)
- **Edge case identification**: Automatic (when generated)
- **Sample test data**: Include (when generated)
- **Integration test scenarios**: YES (when generated)
- **Output file**: `{PROGRAM}_TESTING_GUIDE.md` (optional)

## AI Tool Configuration

- **Preferred AI Platform**: [Claude, GPT-4, Copilot, etc.]
- **Context Window Size**: [Token limit for the AI tool]
- **Model Version**: [Specific model version if applicable]

## Chunking Strategy Defaults

- **Small Programs** (<500 lines): Document as single unit
- **Medium Programs** (500-2000 lines): Chunk by major sections
- **Large Programs** (>2000 lines): Chunk by major modules and procedures

- **Maximum Chunk Size**: [Lines of code per AI interaction]
- **Overlap Between Chunks**: [Lines to overlap for context]

## Expert Review Configuration

- **Primary OMNISCRIPT Experts**: [Names and availability]
- **Review Schedule**: [How often reviews occur]
- **Review Priority Areas**: [Which aspects need most scrutiny]
  1. Business logic interpretations
  2. Variable mutation patterns
  3. Call graph accuracy
  4. Error handling documentation

## Documentation Repository

- **Repository Location**: [Path to documentation repository]
- **Version Control**: [Git repository URL if applicable]
- **Branch Strategy**: [How documentation branches relate to code branches]
- **Naming Conventions**: [Organization-specific naming rules]

## Output Formats

- **Primary Format**: Markdown (.md)
- **Diagram Format**: [Mermaid, PlantUML, hand-drawn, etc.]
- **Cross-Reference Format**: [Table, list, graph, etc.]

## Quality Gates

<!-- EDGAR_CHANGE: UPDATED - Enhanced quality gates to reference CODE_QUALITY_ASSESSMENT template -->

**Quality Assessment Template**: Use `templates/CODE_QUALITY_ASSESSMENT.template.md` for standardized quality gate evaluation.

- **Coverage Requirements**:
  - Minimum % of procedures documented: [100% for critical, variable for others]
  - Minimum % of data structures documented: [100% recommended]
  - Call graph completeness: [All procedure call relationships mapped]
  - **Quality assessment completion**: [Required for mission-critical, recommended for all production code]

- **Quality Gate Criteria** (from CODE_QUALITY_ASSESSMENT template):
  - **PASS Criteria**: No CRITICAL risks, HIGH risks mitigated, error handling present, input validation present
  - **PASS WITH WARNINGS**: Some HIGH/MEDIUM risks with documented mitigation plans
  - **FAIL**: Any CRITICAL security/operational risk, hardcoded credentials, injection vulnerabilities, missing error handling

- **Review Requirements**:
  - Expert approval required: [Yes/No by program type]
  - Peer review required: [Yes/No by program type]
  - **Security team review**: [Required for programs with CRITICAL/HIGH security findings]
  - **Operations team review**: [Required for programs with CRITICAL/HIGH operational risks]
  - Documentation update lag: [Maximum days between code change and doc update]

- **Pre-Production Requirements**:
  - Quality assessment must be performed: [Yes/No by program type]
  - Quality gate status must be PASS or PASS WITH WARNINGS: [Yes/No]
  - All CRITICAL findings remediated: [Yes/No]
  - Remediation roadmap approved: [Yes/No]

## Maintenance Process

- **Update Triggers**:
  - Any code modification: [Yes/No]
  - Major logic changes only: [Yes/No]
  - Before production release: [Yes/No]

- **Update Workflow**:
  1. [Step 1: Identify changed sections]
  2. [Step 2: Use AI for updates]
  3. [Step 3: Expert review]
  4. [Step 4: Commit with code changes]

## Organization-Specific Settings

- **Business Terminology**: [Link to business glossary if exists]
- **Abbreviation Standards**: [Organization-specific abbreviations]
- **Naming Conventions**: [Specific naming patterns used in organization]
- **Compliance Requirements**: [Any regulatory or audit documentation needs]

## Error Handling

- **When AI Misinterprets OMNISCRIPT**: [Process for handling AI errors]
- **When Context Window Exceeded**: [Fallback chunking strategy]
- **When Static Analysis Unavailable**: [Manual alternatives]
- **When Expert Unavailable**: [Peer review or escalation process]

---

**Note**: This configuration should be reviewed and customized for each OMNISCRIPT documentation project. Not all settings will apply to every situation.
