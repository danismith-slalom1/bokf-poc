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
    - Includes: Overview, Data dictionary (with mutations), procedures, call graphs, diagrams
    - **Plus**: Error handling analysis, performance analysis, testing guide, integration guide, business rules
  - **Standard business logic**: Standard documentation (overview + data dictionary + procedures + call graph + essentials)
    - Includes: Overview, Data dictionary (with mutations), key procedures, call graph, core diagrams
    - **Plus**: Error handling analysis, testing guide (basic)
  - **Simple utilities**: Minimal documentation (overview + key sections)
    - Includes: Overview (with embedded flow diagram), key procedures
    - **Plus**: Error handling assessment (if file I/O present)

## Output Directory Structure

**Standard documentation location**: All OMNISCRIPT documentation must follow this directory structure:

```
omniscript-documentation/
└── {REPO-NAME}/
    └── {PROGRAM-NAME}/
        ├── {PROGRAM}_OVERVIEW.md                  [Merged: INDEX + COMPREHENSIVE_DOC]
        ├── {PROGRAM}_DATA_DICTIONARY.md           [Includes: Variable Mutations section]
        ├── {PROGRAM}_CALL_GRAPH.md
        ├── {PROGRAM}_DIAGRAMS.md                  [Complex visualizations only]
        ├── {PROGRAM}_ERROR_HANDLING.md
        ├── {PROGRAM}_INTEGRATION_GUIDE.md
        ├── {PROGRAM}_BUSINESS_RULES.md
        ├── {PROGRAM}_CROSS_REFERENCE.md
        ├── {PROGRAM}_VALIDATION_REPORT.md
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

### Error Handling Documentation (HIGH PRIORITY)
- **Always document**: Yes/No [RECOMMENDED: Yes - critical for risk management]
- **Risk assessment required**: Yes/No [RECOMMENDED: Yes]
- **Error status analysis depth**: Basic/Comprehensive
- **Recovery procedure documentation**: Yes/No

### Performance Analysis (MEDIUM PRIORITY)
- **Document for critical programs**: Yes/No [RECOMMENDED: Yes for mission-critical]
- **String operation analysis**: Yes/No [RECOMMENDED: Yes - can be expensive]
- **Memory usage analysis**: Yes/No
- **Optimization recommendations**: Yes/No

### Testing Guide Creation (HIGH PRIORITY)
- **Always generate**: Yes/No [RECOMMENDED: Yes]
- **Edge case identification**: Automatic/Manual review
- **Sample test data**: Include/Reference only
- **Integration test scenarios**: Yes/No

### Integration Documentation (MEDIUM PRIORITY)
- **Document all external calls**: Yes/No [RECOMMENDED: Yes]
- **Entry point contract**: Always/Critical only
- **Deployment guide**: Full/Basic/None
- **System requirements**: Detailed/Summary

### Business Rules Extraction (MEDIUM PRIORITY)
- **Extract explicit rules**: Yes/No [RECOMMENDED: Yes]
- **Infer implicit rules**: Yes/No [RECOMMENDED: Yes for critical programs]
- **Security requirements**: Document/Skip
- **Compliance documentation**: Yes/No/If applicable

### Data Flow Diagrams (LOW PRIORITY - but valuable)
- **Generate for all programs**: Yes/No
- **Variable lifecycle diagrams**: Critical variables only/All major variables
- **Transformation examples**: Yes/No

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

- **Coverage Requirements**:
  - Minimum % of procedures documented: [100% for critical, variable for others]
  - Minimum % of data structures documented: [100% recommended]
  - Call graph completeness: [All procedure call relationships mapped]

- **Review Requirements**:
  - Expert approval required: [Yes/No by program type]
  - Peer review required: [Yes/No by program type]
  - Documentation update lag: [Maximum days between code change and doc update]

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
