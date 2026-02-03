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
    - Includes: Data dictionary, procedures, call graphs, mutations, comprehensive doc, Mermaid diagrams
    - **Plus**: Error handling analysis, performance analysis, testing guide, integration guide, business rules
  - **Standard business logic**: Standard documentation (data dictionary + procedures + call graph + essentials)
    - Includes: Data dictionary, key procedures, call graph, Mermaid diagrams
    - **Plus**: Error handling analysis, testing guide (basic)
  - **Simple utilities**: Minimal documentation (overview + key sections)
    - Includes: Overview, key procedures, basic Mermaid diagram
    - **Plus**: Error handling assessment (if file I/O present)

## Documentation Enhancement Settings

<!-- EDGAR_CHANGE: UPDATED - Consolidated settings to reference CODE_QUALITY_ASSESSMENT template -->

### Comprehensive Code Quality Assessment (HIGH PRIORITY)
**Template**: `templates/CODE_QUALITY_ASSESSMENT.template.md`

- **Always perform for mission-critical programs**: Yes/No [RECOMMENDED: Yes]
- **Perform for standard programs**: Yes/No [RECOMMENDED: Yes for production-bound code]
- **Assessment scope by program type**:
  - Mission-critical: Comprehensive (All sections A-F)
  - Standard business logic: Standard (Sections A, C, D, F + selective B, E)
  - Utility programs: Targeted (Sections A, D, F minimum)
  
#### Quality Assessment Components:
**Section A: Error Handling Documentation** (HIGH PRIORITY)
- **Always document**: Yes/No [RECOMMENDED: Yes - critical for risk management]
- **Risk assessment required**: Yes/No [RECOMMENDED: Yes]
- **Error status analysis depth**: Basic/Comprehensive
- **Recovery procedure documentation**: Yes/No
- **Runtime error scenario analysis**: Yes/No [RECOMMENDED: Yes]
- **Resource limit documentation**: Yes/No [RECOMMENDED: Yes]
- **Input validation assessment**: Yes/No [RECOMMENDED: Yes]

**Section B: Best Practices Assessment** (MEDIUM PRIORITY)
- **OmniScript API pattern analysis**: Yes/No [RECOMMENDED: Yes]
- **Deprecated API detection**: Yes/No [RECOMMENDED: Yes]
- **COBOL-specific checks**: Yes/No/If applicable
- **Integration pattern evaluation**: Yes/No [RECOMMENDED: Yes]
- **Performance pattern analysis**: Yes/No [RECOMMENDED: Yes for critical programs]

**Section C: Security Assessment** (HIGH PRIORITY)
- **Security vulnerability scan**: Always/Critical only/Never [RECOMMENDED: Always]
- **Hardcoded credential detection**: Yes/No [RECOMMENDED: Yes - MANDATORY for production]
- **Injection vulnerability check**: Yes/No [RECOMMENDED: Yes]
- **Access control review**: Yes/No [RECOMMENDED: Yes for critical programs]
- **Compliance verification**: Yes/No/If applicable

**Section D: Operational Risk Assessment** (HIGH PRIORITY)
- **Risk categorization**: Yes/No [RECOMMENDED: Yes]
- **Data corruption scenario analysis**: Yes/No [RECOMMENDED: Yes]
- **Performance degradation assessment**: Yes/No [RECOMMENDED: Yes for critical programs]
- **Resource exhaustion check**: Yes/No [RECOMMENDED: Yes]

**Section E: Quality Scoring** (MEDIUM PRIORITY)
- **Overall quality metrics**: Yes/No [RECOMMENDED: Yes]
- **Per-procedure assessment**: Yes/No/Critical procedures only
- **Trend analysis**: Yes/No [RECOMMENDED: Yes if historical data exists]

**Section F: Quality Gate Checks** (HIGH PRIORITY)
- **Automated gate evaluation**: Yes/No [RECOMMENDED: Yes for all production code]
- **Deployment readiness determination**: Yes/No [RECOMMENDED: Yes]
- **Remediation roadmap generation**: Yes/No [RECOMMENDED: Yes]

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
