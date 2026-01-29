This is a configuration file to aid with the automation of the COBOL documentation process. Below you will find configuration settings and guidance for documenting COBOL programs using AI assistance.

**Configuration Settings**:

## Static Analysis Tools

- **Primary Tool**: [Specify COBOL compiler or static analysis tool]
  - Cross-reference generation: [Command/flag to generate cross-reference]
  - Call graph generation: [Command/flag to generate call graph]
  - Output format: [Format of analysis reports]

- **Alternative Tools**: [List backup or supplementary tools]

## COBOL Dialect and Environment

- **COBOL Version**: [COBOL-74, COBOL-85, Enterprise COBOL, Micro Focus, etc.]
- **Compiler**: [IBM, Micro Focus, GnuCOBOL, etc.]
- **Platform**: [z/OS, Unix, Windows, etc.]
- **Specific Dialects**: [Any dialect-specific considerations]

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
    - Includes: Data dictionary, paragraphs, call graphs, mutations, comprehensive doc, Mermaid diagrams
    - **Plus**: Error handling analysis, performance analysis, testing guide, integration guide, business rules
  - **Standard business logic**: Standard documentation (data dictionary + paragraphs + call graph + essentials)
    - Includes: Data dictionary, key paragraphs, call graph, Mermaid diagrams
    - **Plus**: Error handling analysis, testing guide (basic)
  - **Simple utilities**: Minimal documentation (overview + key sections)
    - Includes: Overview, key paragraphs, basic Mermaid diagram
    - **Plus**: Error handling assessment (if file I/O present)

## Documentation Enhancement Settings

### Error Handling Documentation (HIGH PRIORITY)
- **Always document**: Yes/No [RECOMMENDED: Yes - critical for risk management]
- **Risk assessment required**: Yes/No [RECOMMENDED: Yes]
- **FILE STATUS analysis depth**: Basic/Comprehensive
- **Recovery procedure documentation**: Yes/No

### Performance Analysis (MEDIUM PRIORITY)
- **Document for critical programs**: Yes/No [RECOMMENDED: Yes for mission-critical]
- **STRING operation analysis**: Yes/No [RECOMMENDED: Yes - expensive in COBOL]
- **Memory usage analysis**: Yes/No
- **Optimization recommendations**: Yes/No

### Testing Guide Creation (HIGH PRIORITY)
- **Always generate**: Yes/No [RECOMMENDED: Yes]
- **Edge case identification**: Automatic/Manual review
- **Sample test data**: Include/Reference only
- **Integration test scenarios**: Yes/No

### Integration Documentation (MEDIUM PRIORITY)
- **Document all CALL statements**: Yes/No [RECOMMENDED: Yes]
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
- **Large Programs** (>2000 lines): Chunk by division and section

- **Maximum Chunk Size**: [Lines of code per AI interaction]
- **Overlap Between Chunks**: [Lines to overlap for context]

## Expert Review Configuration

- **Primary COBOL Experts**: [Names and availability]
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
  - Minimum % of paragraphs documented: [100% for critical, variable for others]
  - Minimum % of WORKING-STORAGE documented: [100% recommended]
  - Call graph completeness: [All PERFORM relationships mapped]

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

- **When AI Misinterprets COBOL**: [Process for handling AI errors]
- **When Context Window Exceeded**: [Fallback chunking strategy]
- **When Static Analysis Unavailable**: [Manual alternatives]
- **When Expert Unavailable**: [Peer review or escalation process]

---

**Note**: This configuration should be reviewed and customized for each COBOL documentation project. Not all settings will apply to every situation.
