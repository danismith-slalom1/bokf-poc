# OmniScript Upgrade Assessment Template

This template is **MANDATORY** for all AI agents performing OmniScript version upgrade readiness assessments. Following this template structure is **REQUIRED** when documenting the upgrade path from detected OmniScript version to target version 7.5.

**Expected Version Context**: Files are expected to be OmniScript 6.05. Version detection will verify this and identify any discrepancies.

**IMPORTANT**: This template supports flexible version detection. If the detected version differs from the expected 6.05, document the actual version and adjust the analysis accordingly.

## Template Structure

Use this exact template structure when creating OmniScript upgrade assessment documents:

```markdown
# [PROGRAM-NAME] OmniScript Upgrade Assessment

**Assessment Date**: [Date]  
**Expected Version**: 6.05  
**Detected Version**: [Actual detected version or "Unknown - See Detection Analysis"]  
**Target Version**: 7.5  
**Assessed By**: [AI Agent with expert review]

## Executive Summary

### Upgrade Readiness Overview
- **Version Detection Status**: [Successfully Detected/Unable to Detect]
- **Expected vs Detected**: [Match/Discrepancy - Details]
- **Version Gap**: [e.g., 6.05 → 7.5, or actual detected version → 7.5]
- **Overall Risk Level**: [Low/Medium/High/Critical]
- **Migration Complexity**: [Trivial/Minor/Moderate/Major/Severe]
- **Estimated Migration Effort**: [Duration with breakdown]
- **Recommended Timeline**: [Suggested timeframe for upgrade]
- **Go/No-Go Recommendation**: [Proceed/Defer with justification]

### Key Findings
1. [Most critical finding]
2. [Second most critical finding]
3. [Third most critical finding]

### Quick Stats
- **Total Lines of Code**: [Count]
- **Lines Requiring Changes**: [Count] ([Percentage]%)
- **Procedures Requiring Updates**: [Count]
- **Breaking Changes Identified**: [Count]
- **API Migrations Required**: [Count]
- **Deprecated Features in Use**: [Count]

## Current Version Analysis

### Version Detection
- **Detection Method**: [How version was identified or why it couldn't be determined]
- **Version Confidence**: [High/Medium/Low/Unable to Detect]
- **Detected Version**: [Specific version or "Unknown"]
- **Version Evidence**: [Specific indicators - headers, syntax patterns, API usage, or lack thereof]

#### Version Detection Details
**File Header Analysis**:
- Version declarations found: [Yes/No - Details]
- Copyright/metadata with version info: [Yes/No - Details]

**Syntax Pattern Analysis**:
- Version-specific syntax identified: [List patterns if found]
- Syntax suggests version range: [e.g., "5.x or 6.x based on X feature"]

**API Usage Analysis**:
- Version-specific APIs detected: [List if found]
- APIs suggest minimum version: [Version or Unknown]

**Conservative Assumption**:
- If version unknown: [State assumption, e.g., "Assuming 5.x or earlier for conservative analysis"]

### Version-Specific Feature Usage
List all version-specific features, APIs, and syntax patterns used in this program:

| Feature/API | Usage Location(s) | First Available In | Status in Target Version |
|------------|-------------------|-------------------|-------------------------|
| [Feature] | Lines [X-Y], Proc [NAME] | [Version] | [Deprecated/Removed/Changed/Unchanged] |

### Code Quality Assessment (Current Version)
- **Compliance with Current Version Best Practices**: [Excellent/Good/Fair/Poor]
- **Use of Deprecated Features**: [List if any, or "Cannot assess - version unknown"]
- **Performance Patterns**: [Assessment of current performance approach]
- **Security Posture**: [Assessment of current security measures]
- **Technical Debt**: [Known issues or workarounds]

### Known Issues in Current Version
- [List any known bugs, limitations, or workarounds in detected version]
- [If version unknown: "Version-specific issues cannot be identified without version detection"]

## Target Version Upgrade Impact Analysis

**Note**: If current version is unknown, this analysis assumes conservative baseline and identifies all potentially problematic patterns that may require updates.

### Breaking Changes Analysis

#### Version Gap Assessment
- **From Version**: [Detected version or "Unknown (assumed older)"]
- **To Version**: [Target version]
- **Version Span**: [Number of major versions if known]
- **Analysis Approach**: [Version-specific if known, or conservative/comprehensive if unknown]

#### Critical Breaking Changes
Breaking changes that will cause immediate failures if not addressed:

| Breaking Change | Impact Location | Description | Required Action | Effort |
|----------------|-----------------|-------------|-----------------|--------|
| [Change description] | Lines [X-Y], Proc [NAME] | [What breaks] | [How to fix] | [Hours/Days] |

**Note**: If current version is unknown, this list includes all potential breaking changes from older versions to target version.

#### Behavioral Changes
Changes that alter program behavior without causing errors:

| Behavioral Change | Impact Location | Old Behavior | New Behavior | Risk Level |
|------------------|-----------------|--------------|--------------|------------|
| [Change] | [Location] | [Current behavior] | [Target behavior] | [High/Med/Low] |

### API Migration Requirements

**Version Context**: [If current version detected, note specific API changes; if unknown, note comprehensive API review needed]

#### Removed APIs
APIs that no longer exist in target version:

| Removed API | Usage Locations | Target Version Replacement | Migration Complexity |
|------------|-----------------|---------------------------|---------------------|
| [API name] | [Lines/Procs] | [New API] | [Simple/Moderate/Complex] |

#### Changed API Signatures
APIs with modified parameters or return values:

| API | Old Signature | New Signature | Usage Locations | Changes Required |
|-----|---------------|---------------|-----------------|------------------|
| [API] | [Current sig] | [Target sig] | [Lines/Procs] | [Description] |

#### Deprecated APIs Still Functional
APIs marked deprecated but still working in target version:

| Deprecated API | Usage Locations | Target Version Recommended Replacement | Timeline for Removal |
|---------------|-----------------|---------------------------------------|---------------------|
| [API] | [Lines/Procs] | [New API] | [Version when removed] |

**Version Detection Impact**: [If version unknown, note that all potentially deprecated APIs should be reviewed]

### Data Structure Compatibility

#### Data Type Changes
| 6.05 Type | 7.5 Type | Impact | Migration Required |
|-----------|----------|--------|-------------------|
| [Type] | [Type] | [Description] | [Yes/No - Details] |

#### Serialization/Format Changes
- **File Format Compatibility**: [Assessment of file I/O compatibility]
- **Database Schema Impact**: [Any database structure changes needed]
- **Inter-Program Data Exchange**: [Impact on data passed to/from other programs]

### Compatibility Risk Assessment

#### High Risk Items (Critical - Must Address)
1. **[Risk Description]**
   - **Location**: [Files, lines, procedures]
   - **Issue**: [What will break]
   - **Impact**: [Business/technical impact]
   - **Resolution**: [How to fix]
   - **Effort**: [Estimated time]

#### Medium Risk Items (Important - Should Address)
1. **[Risk Description]**
   - **Location**: [Files, lines, procedures]
   - **Issue**: [Potential problem]
   - **Impact**: [Business/technical impact]
   - **Resolution**: [How to fix]
   - **Effort**: [Estimated time]

#### Low Risk Items (Optional - Consider Addressing)
1. **[Risk Description]**
   - **Location**: [Files, lines, procedures]
   - **Issue**: [Minor concern]
   - **Impact**: [Business/technical impact]
   - **Resolution**: [How to improve]
   - **Effort**: [Estimated time]

#### Opportunities (Enhancements Enabled by Target Version)
1. **[Opportunity Description]**
   - **Target Version Feature**: [New capability]
   - **Current Implementation**: [How it's done in current version]
   - **Potential Improvement**: [How target version could improve it]
   - **Benefits**: [Performance, maintainability, etc.]
   - **Effort**: [Estimated time]

### Performance Implications

#### Expected Performance Improvements
- **[Improvement Area]**: [Description and expected benefit in target version]

#### Potential Performance Regressions
- **[Regression Area]**: [Description and mitigation strategy]

#### New Optimization Opportunities
- **[Optimization]**: [How target version enables better performance]

#### Memory Usage Impact
- **Expected Memory Changes**: [Increase/Decrease/Neutral with details]

## Migration Effort Breakdown

### Code Modification Requirements

#### By Priority Level
| Priority | Item Count | Total Effort | Description |
|----------|-----------|--------------|-------------|
| Critical (P1) | [Count] | [Hours/Days] | [Must-fix items] |
| High (P2) | [Count] | [Hours/Days] | [Should-fix items] |
| Medium (P3) | [Count] | [Hours/Days] | [Nice-to-fix items] |
| Low (P4) | [Count] | [Hours/Days] | [Optional improvements] |
| **Total** | **[Count]** | **[Hours/Days]** | |

#### By Code Section
| Section/Module | Lines to Change | Complexity | Effort | Risk |
|---------------|-----------------|------------|--------|------|
| [Module name] | [Count] | [Low/Med/High] | [Hours/Days] | [Low/Med/High] |

#### By Change Type
| Change Type | Occurrences | Effort per Change | Total Effort |
|------------|-------------|-------------------|--------------|
| API Migration | [Count] | [Hours] | [Hours] |
| Syntax Update | [Count] | [Hours] | [Hours] |
| Behavioral Adjustment | [Count] | [Hours] | [Hours] |
| Refactoring | [Count] | [Hours] | [Hours] |
| **Total** | **[Count]** | | **[Hours]** |

### Testing Requirements

#### Test Creation/Update Needs
| Test Type | Tests to Create | Tests to Update | Effort |
|-----------|----------------|-----------------|--------|
| Unit Tests | [Count] | [Count] | [Hours/Days] |
| Integration Tests | [Count] | [Count] | [Hours/Days] |
| Regression Tests | [Count] | [Count] | [Hours/Days] |
| Performance Tests | [Count] | [Count] | [Hours/Days] |
| Security Tests | [Count] | [Count] | [Hours/Days] |
| **Total** | **[Count]** | **[Count]** | **[Hours/Days]** |

#### Test Coverage Requirements
- **Current Test Coverage**: [Percentage]
- **Required Test Coverage Post-Upgrade**: [Percentage]
- **Additional Tests Needed**: [Count and description]

### Documentation Updates
| Document Type | Updates Required | Effort |
|--------------|------------------|--------|
| Technical Documentation | [Description] | [Hours] |
| User Documentation | [Description] | [Hours] |
| API Documentation | [Description] | [Hours] |
| Deployment Guides | [Description] | [Hours] |
| **Total Documentation Effort** | | **[Hours]** |

### Total Migration Effort Summary
- **Code Changes**: [Hours/Days]
- **Testing**: [Hours/Days]
- **Documentation**: [Hours/Days]
- **Review & QA**: [Hours/Days]
- **Contingency (20%)**: [Hours/Days]
- **Total Estimated Effort**: [Hours/Days/Weeks]

## Dependency Chain Analysis

### Internal Dependencies
Programs/modules in this system that must be upgraded concurrently:

| Dependent Program | Relationship | Upgrade Coordination Required | Notes |
|------------------|--------------|------------------------------|-------|
| [Program name] | [Calls/Called by/Shares data] | [Yes/No] | [Details] |

### External Dependencies
Third-party libraries or external systems affected:

| Dependency | Current Version | Required Version for Target | Upgrade Available | Impact |
|-----------|----------------|-----------------------------|-------------------|--------|
| [Library/System] | [Version or Unknown] | [Version] | [Yes/No] | [Description] |

**Note**: If current version is unknown, dependency version requirements should be verified during migration.

### Shared Resources
Shared modules or libraries requiring version compatibility:

| Shared Resource | Current Version | Compatibility with Target | Action Required |
|----------------|----------------|--------------------------|------------------|
| [Resource name] | [Version or Unknown] | [Compatible/Incompatible/Unknown] | [Details] |

### Integration Points
External systems with version-specific integration requirements:

| Integration Point | Protocol/API | Impact of Upgrade | Changes Required |
|------------------|--------------|-------------------|------------------|
| [System name] | [Details] | [Description] | [Details] |

## Upgrade Roadmap

### Pre-Migration Phase

#### Preparation Checklist
- [ ] Complete static analysis of current codebase
- [ ] Review OmniScript 7.5 migration guide thoroughly
- [ ] Inventory all dependencies and versions
- [ ] Backup production code and data
- [ ] Set up parallel 7.5 test environment
- [ ] Establish rollback procedures
- [ ] Identify and notify stakeholders
- [ ] Create detailed test plan
- [ ] Schedule upgrade maintenance window
- [ ] Document current system performance baselines

#### Environment Setup
1. **Test Environment**: [Requirements and setup steps]
2. **Version Control**: [Branching strategy]
3. **Backup Strategy**: [Backup procedures]
4. **Rollback Plan**: [Rollback procedures]

#### Stakeholder Communication
- **Affected Teams**: [List teams to notify]
- **Communication Plan**: [Timeline and content]
- **Training Requirements**: [If any]

### Migration Phases

#### Phase 1: Preparation (Estimated: [Duration])
**Objective**: Prepare environment and codebase for migration

**Tasks**:
1. [Task 1] - [Duration] - [Owner]
2. [Task 2] - [Duration] - [Owner]

**Deliverables**:
- [Deliverable 1]
- [Deliverable 2]

**Success Criteria**:
- [Criterion 1]
- [Criterion 2]

#### Phase 2: Non-Breaking Updates (Estimated: [Duration])
**Objective**: Address deprecation warnings and prepare code

**Tasks**:
1. Update deprecated API calls to recommended alternatives
2. Modernize code patterns where possible
3. Run initial compatibility tests

**Deliverables**:
- Updated code with deprecated features replaced
- Preliminary test results

**Success Criteria**:
- Zero deprecation warnings
- All preliminary tests pass

#### Phase 3: Breaking Changes (Estimated: [Duration])
**Objective**: Update code for breaking changes

**Tasks**:
1. [Specific breaking change 1] - [Location] - [Duration]
2. [Specific breaking change 2] - [Location] - [Duration]
3. [If version unknown: Comprehensive code review for compatibility] - [Duration]

**Deliverables**:
- Code migrated to target version syntax and APIs
- Unit tests updated and passing

**Success Criteria**:
- Code compiles/runs on target version
- Unit tests pass
- [If version unknown: All compatibility issues addressed]

#### Phase 4: Comprehensive Testing (Estimated: [Duration])
**Objective**: Validate all functionality in target version environment

**Tasks**:
1. Execute regression test suite
2. Perform integration testing
3. Conduct performance testing
4. Execute security testing
5. User acceptance testing (if applicable)

**Deliverables**:
- Test results report
- Performance comparison (current vs target version)
- Issue tracking and resolution log

**Success Criteria**:
- [X]% regression tests pass
- Performance meets or exceeds current baseline
- Security tests pass
- All P1/P2 issues resolved

#### Phase 5: Optimization (Estimated: [Duration])
**Objective**: Leverage target version features for improvements

**Tasks**:
1. Implement performance optimizations
2. Adopt new target version best practices
3. Refactor code using new target version features

**Deliverables**:
- Optimized codebase
- Performance improvement report

**Success Criteria**:
- Measurable performance improvements
- Code quality metrics improved

#### Phase 6: Deployment (Estimated: [Duration])
**Objective**: Deploy to production with monitoring

**Tasks**:
1. Final pre-deployment checklist
2. Production deployment
3. Post-deployment monitoring
4. Documentation updates

**Deliverables**:
- Production system on target version
- Deployment report
- Updated documentation

**Success Criteria**:
- Successful production deployment
- No critical issues in first [X] hours
- Rollback plan ready if needed

### Code Modification Priorities

#### Priority 1 (Critical) - Week 1
Items that will cause immediate failures:
1. **[Item 1]** - [Location] - [Effort]
   - **Issue**: [Description]
   - **Fix**: [Solution]

#### Priority 2 (High) - Week 2
Deprecated features with near-term removal:
1. **[Item 1]** - [Location] - [Effort]
   - **Issue**: [Description]
   - **Fix**: [Solution]

#### Priority 3 (Medium) - Week 3
Performance and security improvements:
1. **[Item 1]** - [Location] - [Effort]
   - **Improvement**: [Description]
   - **Implementation**: [Solution]

#### Priority 4 (Low) - Week 4+
Optional enhancements and modernization:
1. **[Item 1]** - [Location] - [Effort]
   - **Enhancement**: [Description]
   - **Implementation**: [Solution]

### Testing Strategy

#### Unit Testing
- **Scope**: [What will be unit tested]
- **Approach**: [Testing methodology]
- **Success Criteria**: [Pass rate and coverage targets]

#### Integration Testing
- **Scope**: [What integration points will be tested]
- **Approach**: [Testing methodology]
- **Success Criteria**: [Pass rate and coverage targets]

#### Regression Testing
- **Scope**: [What regression tests will be run]
- **Approach**: [Testing methodology]
- **Success Criteria**: [Pass rate targets]

#### Performance Testing
- **Baseline Metrics**: [Current version performance or "To be established"]
- **Target Metrics**: [Desired target version performance]
- **Test Scenarios**: [List of performance tests]
- **Success Criteria**: [Performance targets]

#### Security Testing
- **Security Tests**: [List of security validations]
- **Compliance Requirements**: [If applicable]
- **Success Criteria**: [Security pass criteria]

### Fallback and Contingency Planning

#### Rollback Plan
**Rollback Triggers**:
- [Trigger 1 - description and threshold]
- [Trigger 2 - description and threshold]

**Rollback Procedures**:
1. [Step 1]
2. [Step 2]

**Rollback Time**: [Estimated duration to revert]

#### Version Coexistence Strategy
If parallel operation of current and target versions is needed:
- **Coexistence Duration**: [Timeframe]
- **Data Synchronization**: [How to keep versions in sync]
- **Traffic Routing**: [How to direct traffic]
- **Decision Criteria**: [When to fully cut over]

#### Gradual Migration Approach
For complex systems requiring phased migration:
- **Phase 1**: [What migrates first]
- **Phase 2**: [What migrates second]
- **Completion**: [Final migration phase]

#### Parallel Running
If running both versions simultaneously:
- **Duration**: [How long to run parallel]
- **Comparison Strategy**: [How to compare results]
- **Cutover Criteria**: [When to switch to target version only]

## Benefits of Upgrading

### Performance Improvements
1. **[Improvement 1]**: [Description and expected gain]
2. **[Improvement 2]**: [Description and expected gain]

### New Features and Capabilities
1. **[Feature 1]**: [Description and benefit]
2. **[Feature 2]**: [Description and benefit]

### Security Enhancements
1. **[Enhancement 1]**: [Description and security benefit]
2. **[Enhancement 2]**: [Description and security benefit]

### Maintainability Improvements
1. **[Improvement 1]**: [Description and maintenance benefit]
2. **[Improvement 2]**: [Description and maintenance benefit]

### Cost Savings
- **[Cost Area]**: [Description and estimated savings]

### Business Value
- **[Business Benefit]**: [Description and impact]

## Risks of Not Upgrading

### Support and Maintenance
- **Current Version End-of-Life Date**: [Date if known, or "Unknown - requires vendor verification"]
- **Support Implications**: [Description of support limitations]
- **Security Patch Availability**: [Timeline and concerns]

### Security Vulnerabilities
1. **[Vulnerability 1]**: [Description and risk]
2. **[Vulnerability 2]**: [Description and risk]

**Note**: If current version is unknown, security assessment requires version identification.

### Performance Limitations
- **[Limitation 1]**: [Description of performance constraint]
- **[Limitation 2]**: [Description of performance constraint]

### Feature Gaps
- **[Missing Feature]**: [Business impact of not having it]

### Technical Debt Accumulation
- **Current Technical Debt**: [Description]
- **Projected Growth**: [How debt will increase over time]
- **Future Migration Complexity**: [How difficulty increases with delay]

### Competitive Disadvantage
- **Market Impact**: [How staying on old version affects competitiveness]
- **Innovation Constraints**: [Limitations on new capabilities]

## Recommendations

### Immediate Actions (Before Migration)
1. **[Action 1]** - [Rationale] - [Owner] - [Deadline]
2. **[Action 2]** - [Rationale] - [Owner] - [Deadline]

### Pre-Upgrade Actions (Preparation Phase)
1. **[Action 1]** - [Rationale] - [Owner] - [Deadline]
2. **[Action 2]** - [Rationale] - [Owner] - [Deadline]

### Post-Upgrade Actions (After Migration)
1. **[Action 1]** - [Rationale] - [Owner] - [Deadline]
2. **[Action 2]** - [Rationale] - [Owner] - [Deadline]

### Long-Term Improvements
1. **[Improvement 1]** - [Description and timeline]
2. **[Improvement 2]** - [Description and timeline]

## Decision Factors

### Go/No-Go Criteria

#### Proceed with Upgrade If:
- [ ] Migration effort is within acceptable range ([X] days/weeks)
- [ ] No critical blockers identified
- [ ] Test environment successfully configured
- [ ] Stakeholder approval obtained
- [ ] Adequate resources available
- [ ] Business value justifies effort

#### Defer Upgrade If:
- [ ] Critical blockers with no known solution
- [ ] Dependencies not ready for 7.5
- [ ] Inadequate testing resources
- [ ] Business-critical deadline conflicts
- [ ] Unacceptable risk level
- [ ] Cost exceeds benefits

### Risk vs. Benefit Analysis
**Overall Assessment**: [Go/No-Go/Conditional with detailed justification]

## Appendices

### Appendix A: Detailed Change Inventory
[Complete list of every change required with line-by-line details]

### Appendix B: Testing Checklist
[Comprehensive test case list]

### Appendix C: API Migration Reference
[Complete mapping of old APIs to new APIs with examples]

### Appendix D: Code Examples
[Before/after code samples for common migration scenarios]

### Appendix E: Related Documentation
- OmniScript 6.05 Documentation: [Link]
- OmniScript 7.5 Documentation: [Link]
- Migration Guide: [Link]
- Release Notes: [Link]

---
**IMPORTANT**: This upgrade assessment must be reviewed and validated by OmniScript experts and business stakeholders before proceeding with migration.

**NEXT STEPS**: Upon approval, proceed to Phase 2 (Iterative Documentation Generation) with upgrade considerations integrated throughout the documentation process.
```

## Critical Requirements

### Content Requirements
- **Version Identification**: Must accurately detect and document current version
- **Breaking Changes**: Must identify all breaking changes affecting the code
- **API Migrations**: Must map all API changes with replacement guidance
- **Risk Assessment**: Must categorize all risks by severity level
- **Effort Estimation**: Must provide realistic time estimates
- **Testing Strategy**: Must define comprehensive testing approach
- **Rollback Planning**: Must include contingency and rollback procedures

### Content Guidance
When creating the upgrade assessment document, ensure you:
- **Provide specific line numbers** for all identified issues
- **Include code examples** showing before/after for complex changes
- **Quantify effort estimates** realistically based on program complexity
- **Prioritize changes** based on business impact and technical necessity
- **Document dependencies** that affect upgrade coordination
- **Request expert validation** of assessment accuracy

### Format Requirements
- Use exact markdown structure as shown above
- Include the program name, date, and version information in the title
- Include all mandatory sections
- Maintain consistent section headers and formatting
- Use tables for structured data comparisons
- Include checklists for actionable items

### File Naming Convention
Save OmniScript upgrade assessment documents as:
`${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_UPGRADE_ASSESSMENT.md`

Where `${OMNISCRIPT_DOCS_DIR}` refers to `omniscript-documentation/{REPO-NAME}/[PROGRAM-NAME]/`

## Mandatory Compliance

This template structure is **NON-NEGOTIABLE**. Agents must:
1. Follow the exact section structure
2. Include all required sections
3. Maintain consistent formatting
4. Include risk assessments and effort estimates
5. Provide actionable recommendations
6. Use the specified file naming convention

**Failure to comply with this template will result in agent termination.**

## Usage Notes

- This document should be placed in `${OMNISCRIPT_DOCS_DIR}/` where `${OMNISCRIPT_DOCS_DIR}` = `omniscript-documentation/{REPO-NAME}/[PROGRAM-NAME]/`
- The upgrade assessment is created in Phase 1.5 (after program analysis, before iterative documentation)
- OmniScript experts and business stakeholders must explicitly approve the assessment before proceeding with migration
- This template enforces mandatory compliance for consistent upgrade planning
- **Example path**: `omniscript-documentation/my-repo/PAYROLL/PAYROLL_UPGRADE_ASSESSMENT.md`
- Link to this document from the master index and comprehensive documentation
- Reference specific sections from other documentation as needed (e.g., link breaking changes from error handling docs)
