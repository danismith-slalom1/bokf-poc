# Code Quality Assessment Template

This template is **MANDATORY** for all AI agents performing comprehensive code quality, security, and risk assessment of OMNISCRIPT programs. Following this template structure is **REQUIRED** for production-ready quality assurance documentation.

**Purpose**: This template provides a standardized framework for assessing code quality, identifying security vulnerabilities, evaluating operational risks, and determining upgrade/deployment readiness through automated quality gate checks.

**When to Use**: 
- During Phase 2 documentation (reference from Phase 2.3)
- As a standalone assessment for existing documented code
- Before production deployments
- For compliance and audit requirements
- During modernization planning

## Template Structure

Use this exact template structure when creating code quality assessment documents:

```markdown
# Code Quality Assessment - [Program Name] - [Date]

**Assessment Date**: [Date]
**Program**: [Program name and version]
**Assessed By**: [AI Agent with expert review]
**Assessment Type**: [Comprehensive/Targeted/Pre-Production/Audit]
**Overall Quality Grade**: [A/B/C/D/F with justification]

---

## Executive Summary

### Quality Overview
- **Overall Risk Level**: [🔴 Critical / 🟠 High / 🟡 Medium / 🟢 Low]
- **Security Posture**: [Critical Issues Count / High Issues Count / Assessment]
- **Operational Readiness**: [Production Ready / Needs Remediation / Not Ready]
- **Quality Gate Status**: [✅ PASSED / ⚠️ PASSED WITH WARNINGS / ❌ FAILED]
- **Recommended Actions**: [Immediate / Before Production / Next Cycle]

### Critical Findings Summary
1. **[Finding Type]**: [Brief description] - Priority: [IMMEDIATE/HIGH/MEDIUM]
2. **[Finding Type]**: [Brief description] - Priority: [IMMEDIATE/HIGH/MEDIUM]
3. **[Finding Type]**: [Brief description] - Priority: [IMMEDIATE/HIGH/MEDIUM]

### Quick Metrics
- **Total Issues Identified**: [Count]
  - 🔴 Critical: [Count]
  - 🟠 High: [Count]
  - 🟡 Medium: [Count]
  - 🟢 Low: [Count]
  - ⚪ Informational: [Count]
- **Estimated Remediation Effort**: [Hours/Days]
- **Lines of Code Affected**: [Count] ([Percentage]%)

---

## Section A: Error Handling Analysis

**Objective**: Comprehensive analysis of error handling mechanisms, recovery procedures, and risk assessment for all operations.

### A.1 Error Handling & Runtime Risk Analysis

**Objective**: Comprehensive analysis of error handling mechanisms, runtime scenarios, and risk assessment.

#### Error Handling Coverage

| Operation/Scenario | Location | Error Mechanism | Coverage | Risk Level | Recommendation |
|-------------------|----------|-----------------|----------|------------|----------------|
| File Open/Not Found | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| File Access Denied | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Database Query/Timeout | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| External Call Failure | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Invalid Data Format | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Type Mismatch | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Division by Zero | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Array Out of Bounds | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |
| Null/Empty Reference | Lines [X-Y] | [Try-Catch/Status/None] | [Complete/Partial/Missing] | [Level] | [Action] |

#### Status Codes & Recovery Procedures

| Status Code | Meaning | Handler Location | Recovery Action | Adequacy |
|------------|---------|------------------|-----------------|----------|
| [Code] | [Description] | Lines [X-Y] | [What happens] | [Adequate/Partial/None] |

#### Critical Gaps (Operations Without Error Checking)

| Operation | Location | Potential Failure | Impact | Priority |
|-----------|----------|-------------------|--------|----------|
| [Operation] | Lines [X-Y] | [Failure type] | [Impact] | [IMMEDIATE/HIGH] |

### A.2 Resource Limits & Input Validation

**Objective**: Document system limits, boundary protections, and input validation mechanisms.

#### Buffer and Memory Limits

| Resource | Maximum Size | Overflow Protection | Validation Method | Risk Level |
|----------|-------------|---------------------|-------------------|------------|
| String buffers | [Size] | [Yes/No - Method] | [Method or "None"] | [Level] |
| Arrays | [Size] | [Yes/No - Method] | [Method or "None"] | [Level] |
| File buffers | [Size] | [Yes/No - Method] | [Method or "None"] | [Level] |
| Memory allocation | [Size] | [Yes/No - Method] | [Method or "None"] | [Level] |

#### Operational Limits & Enforcement

| Limit Type | Maximum Value | Enforcement | Exceeded Handling | Documentation |
|-----------|---------------|-------------|------------------|---------------|
| Record count | [Count] | [Yes/No] | [What happens] | [Where documented] |
| File size | [Size] | [Yes/No] | [What happens] | [Where documented] |
| Transaction size | [Size] | [Yes/No] | [What happens] | [Where documented] |

#### Input Validation & Sanitization

**File Path Validation**:
- **Validation Present**: [Yes/No/Partial] | **Sanitization**: [Method or "None"]
- **Path Traversal Protection**: [Yes/No] | **Injection Protection**: [Yes/No]

**Data Format Validation**:

| Input Type | Validation Method | Range/Boundary Check | Risk if Missing |
|-----------|------------------|---------------------|-----------------|
| Numeric fields | [Method or "None"] | [Yes/No - Range] | [Risk level + description] |
| Date fields | [Method or "None"] | [Yes/No - Range] | [Risk level + description] |
| String fields | [Method or "None"] | [Yes/No - Length] | [Risk level + description] |
| File formats | [Method or "None"] | [Yes/No] | [Risk level + description] |

---

## Section B: OmniScript Best Practices Assessment

**Objective**: Evaluate adherence to language-specific best practices and identify anti-patterns.

### OmniScript-Specific Quality Checks

#### API Usage Patterns
| API/Function | Usage Location | Pattern Quality | Issue | Recommendation |
|-------------|----------------|-----------------|-------|----------------|
| [API name] | Lines [X-Y] | [Optimal/Suboptimal/Incorrect] | [Issue description] | [Improvement] |

**Deprecated API Usage**:
- **Count**: [Number] deprecated APIs in use
- **Risk**: [Low/Medium/High] based on removal timeline

| Deprecated API | Location | Replacement API | Migration Priority | Effort |
|---------------|----------|-----------------|-------------------|--------|
| [API] | Lines [X-Y] | [New API] | [IMMEDIATE/HIGH/MEDIUM] | [Estimate] |

#### Integration Patterns
**External Program Calling Conventions**:
- **Convention Compliance**: [Yes/No/Partial]
- **Issues Identified**: [Count]

| Call Location | Called Program | Pattern Used | Issue | Best Practice |
|--------------|----------------|--------------|-------|---------------|
| Lines [X-Y] | [Program] | [Pattern] | [Issue or "None"] | [Recommendation] |

**Parameter Passing**:
- **Consistency**: [Consistent/Inconsistent]
- **Type Safety**: [Strong/Weak/None]
- **Validation**: [Present/Missing]

**Return Code Handling**:
- **Coverage**: [Percentage]% of external calls check return codes
- **Standardization**: [Standardized/Varies]

#### Performance Patterns

| Pattern Type | Location | Current State | Complexity/Impact | Optimization Opportunity |
|--------------|----------|---------------|-------------------|-------------------------|
| Loop Structure | Lines [X-Y] | [FOR/WHILE/etc] | [O(n)/O(n²)/etc] | [Suggestion] |
| String Concatenation | Lines [X-Y] | [In loop/Outside] | [High/Medium/Low] | [Suggestion] |
| String Buffer Usage | Lines [X-Y] | [Optimal/Suboptimal] | [Impact] | [Suggestion] |
| File I/O Buffering | Lines [X-Y] | [Present/Missing] | [Impact] | [Suggestion] |
| File Read/Write Pattern | Lines [X-Y] | [Efficient/Inefficient] | [Impact] | [Suggestion] |
| Transaction Management | Lines [X-Y] | [Present/Missing] | [Impact] | [Suggestion] |

**Performance Summary**:
- **Critical Performance Issues**: [Count]
- **Estimated Performance Impact**: [High/Medium/Low]
- **Optimization Priority**: [IMMEDIATE/HIGH/MEDIUM/LOW]

### Test Coverage Assessment

**Objective**: Evaluate the completeness and quality of test coverage for the OmniScript program.

#### Test Inventory
| Test Type | Coverage | Location | Adequacy | Gaps |
|-----------|----------|----------|----------|------|
| Unit Tests | [%] | [Location] | [Adequate/Inadequate] | [Description] |
| Integration Tests | [%] | [Location] | [Adequate/Inadequate] | [Description] |
| Error Scenario Tests | [%] | [Location] | [Adequate/Inadequate] | [Description] |
| Boundary Tests | [%] | [Location] | [Adequate/Inadequate] | [Description] |

#### Critical Path Coverage
- **Critical Operations Tested**: [%]
- **Error Paths Tested**: [%]
- **Edge Cases Covered**: [List or count]

#### Test Quality Assessment
| Aspect | Status | Risk Level | Recommendation |
|--------|--------|------------|----------------|
| Test Existence | [Present/Missing] | [Level] | [Action] |
| Test Completeness | [Complete/Partial] | [Level] | [Action] |
| Test Maintainability | [Good/Poor] | [Level] | [Action] |
| Mock/Stub Quality | [Adequate/Inadequate] | [Level] | [Action] |

#### OmniScript Configuration Management
- **Configuration Externalization**: [Yes/No/Partial]
- **Environment-Specific Settings**: [Properly managed/Hardcoded]
- **Secret Management**: [Secure/Insecure]

| Configuration Item | Location | Method | Security Risk | Recommendation |
|-------------------|----------|--------|---------------|----------------|
| [Item] | Lines [X-Y] | [Hardcoded/External] | [Level] | [Action] |

#### Data Element Usage
- **Data Element Validation**: [Comprehensive/Partial/Missing]
- **Type Safety**: [Strong/Weak]
- **Null Handling**: [Consistent/Inconsistent]

| Data Element | Location | Validation Present | Issue | Recommendation |
|--------------|----------|-------------------|-------|----------------|
| [Element] | Lines [X-Y] | [Yes/No/Partial] | [Issue or "None"] | [Action] |

#### Integration Patterns - OmniScript Specific
- **Remote Procedure Calls**: [Best practice/Needs improvement]
- **Data Transformation**: [Efficient/Inefficient]
- **Error Propagation**: [Proper/Improper]

| Integration Point | Location | Pattern Quality | Issue | Best Practice |
|------------------|----------|-----------------|-------|---------------|
| [Call/Integration] | Lines [X-Y] | [Optimal/Suboptimal] | [Issue or "None"] | [Recommendation] |
---

## Section C: Security and Safety Assessment

**Objective**: Identify security vulnerabilities, safety risks, and compliance gaps.

### C.1 Security Vulnerabilities & Risks

**Objective**: Identify and categorize all security vulnerabilities by severity.

**Security Findings Summary**:
- 🔴 **Critical**: [Count] - Immediate remediation required
- 🟠 **High**: [Count] - Address before production
- 🟡 **Medium**: [Count] - Address in next cycle
- **Block Deployment**: [YES / NO] - [Justification]

#### All Security Findings

| Severity | Vulnerability Type | Location | Description | Exploit Scenario/Impact | Remediation | Effort | Priority |
|----------|-------------------|----------|-------------|------------------------|-------------|--------|----------|
| 🔴 | Hardcoded Credentials | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🔴 | SQL Injection | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🔴 | Command Injection | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🔴 | Path Traversal | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🔴 | Auth Bypass | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🔴 | Hardcoded API Keys/Secrets | Lines [X-Y] | [Details] | [How exploited] | [Fix] | [Estimate] | IMMEDIATE |
| 🟠 | Missing Input Validation | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Error Info Disclosure | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Insecure File Handling | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Race Conditions | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Access Control Gaps | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Missing RPC Auth/Timeout | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟠 | Data Element Access Control | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | HIGH |
| 🟡 | Weak Input Sanitization | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | MEDIUM |
| 🟡 | Insufficient Logging | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | MEDIUM |
| 🟡 | Deprecated Crypto | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | MEDIUM |
| 🟡 | Deprecated OmniScript APIs | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | MEDIUM |
| 🟡 | Data Transformation Risks | Lines [X-Y] | [Details] | [Impact] | [Fix] | [Estimate] | MEDIUM |

### C.2 Security Posture Summary

#### Security Controls Present
- ✅ **Authentication**: [Present/Missing/Inadequate]
- ✅ **Authorization**: [Present/Missing/Inadequate]
- ✅ **Input Validation**: [Comprehensive/Partial/Missing]
- ✅ **Output Encoding**: [Present/Missing]
- ✅ **Error Handling**: [Secure/Exposes internals]
- ✅ **Audit Logging**: [Comprehensive/Basic/Missing]
- ✅ **Data Encryption**: [Present/Missing/N/A]
- ✅ **Session Management**: [Secure/Insecure/N/A]

#### OmniScript-Specific Controls
- ✅ **RPC Security**: [Secure/Needs Review/Insecure] - Auth, authz, timeout handling on external calls
- ✅ **Configuration Management**: [Externalized/Partially Hardcoded/Hardcoded] - Secrets in vault vs hardcoded
- ✅ **Data Element Security**: [Present/Partial/Missing] - Access control, sensitive data handling, validation

#### Compliance Considerations
- **Data Privacy**: [GDPR/CCPA/Other compliance requirements]
- **Audit Trail**: [Complete/Incomplete/Missing]
- **Data Retention**: [Compliant/Non-compliant]
- **Access Logging**: [Present/Missing]
---

## Section D: Operational and Error Handling Risk Assessment

**Objective**: Assess operational risks, error handling maturity, and impacts on system stability, data integrity, and business operations.

### D.1 Error Handling Maturity

- **Maturity Level**: [None / Minimal / Partial / Comprehensive / Exemplary]
- **Coverage**: [Estimate]% of critical operations covered
- **Consistency**: [Inconsistent / Partially Consistent / Fully Consistent]

### D.2 Operational Risks & Findings

**Objective**: Assess all operational risks that could impact system stability, data integrity, or business operations.

**Operational Risk Summary**:
- 🔴 **Critical**: [Count] - Risk of system failure or data loss
- 🟠 **High**: [Count] - Significant operational concerns
- 🟡 **Medium**: [Count] - Should be improved
- 🟢 **Low/Info**: [Count] - Minor improvements and observations
- **Production Readiness**: [BLOCKED / CONDITIONAL / CLEARED]

#### All Operational Findings

| Severity | Risk Type | Location | Scenario/Description | Impact | Mitigation | Owner | Timeline |
|----------|-----------|----------|---------------------|--------|------------|-------|----------|
| 🔴 | Data Corruption | Lines [X-Y] | [Scenario] | [Business impact] | [Action] | [Team] | IMMEDIATE |
| 🔴 | System Crash | Lines [X-Y] | [Scenario] | [Business impact] | [Action] | [Team] | IMMEDIATE |
| 🔴 | Unrecoverable Error | Lines [X-Y] | [Scenario] | [Business impact] | [Action] | [Team] | IMMEDIATE |
| 🔴 | Silent Data Loss | Lines [X-Y] | [Scenario] | [Business impact] | [Action] | [Team] | IMMEDIATE |
| 🟠 | Performance Degradation | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Team] | [Timeline] |
| 🟠 | Resource Exhaustion | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Team] | [Timeline] |
| 🟠 | Error Recovery Gap | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Team] | [Timeline] |
| 🟠 | Data Inconsistency | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Team] | [Timeline] |
| 🟠 | Integration Failure | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Team] | [Timeline] |
| 🟡 | Suboptimal Performance | Lines [X-Y] | [Details] | [Impact] | [Action] | [Team] | Next Cycle |
| 🟡 | Minor Resource Leak | Lines [X-Y] | [Details] | [Impact] | [Action] | [Team] | Next Cycle |
| 🟡 | Inconsistent Handling | Lines [X-Y] | [Details] | [Impact] | [Action] | [Team] | Next Cycle |
| 🟡 | Maintainability Issue | Lines [X-Y] | [Details] | [Impact] | [Action] | [Team] | Next Cycle |
| 🟢 | Code Style Inconsistency | Lines [X-Y] | [Details] | [Impact] | [Suggestion] | [Team] | Backlog |
| 🟢 | Minor Inefficiency | Lines [X-Y] | [Details] | [Impact] | [Suggestion] | [Team] | Backlog |
| 🟢 | Documentation Gap | Lines [X-Y] | [Details] | [Impact] | [Suggestion] | [Team] | Backlog |
| ⚪ | Best Practice Opportunity | Lines [X-Y] | [Details] | [Impact] | [Suggestion] | [Team] | Future |

### D.3 Risk Mitigation Status

**Critical Risks**: [Count with mitigation status]
**High Risks**: [Count with mitigation status]
**Overall Risk Trend**: [Improving/Stable/Declining]

---

## Section E: Code Quality Scoring

**Objective**: Provide quantitative and qualitative assessment of code quality across multiple dimensions.

### E.1 Overall Quality Metrics

| Metric | Score | Grade | Industry Standard | Assessment |
|--------|-------|-------|------------------|------------|
| **Overall Code Quality** | [0-100] | [A-F] | 80+ | [Above/At/Below] |
| Security Posture | [0-100] | [A-F] | 85+ | [Above/At/Below] |
| Error Handling | [0-100] | [A-F] | 80+ | [Above/At/Below] |
| Performance | [0-100] | [A-F] | 75+ | [Above/At/Below] |
| Maintainability | [0-100] | [A-F] | 70+ | [Above/At/Below] |
| Best Practices | [0-100] | [A-F] | 75+ | [Above/At/Below] |
| Documentation | [0-100] | [A-F] | 80+ | [Above/At/Below] |

### E.2 Key Findings by Area

**Note**: Reference detailed findings from Sections A-D above. Provide summary only for highest-priority items.

| Area | Risk Level | Top Issue | Location | Priority Action |
|------|-----------|-----------|----------|----------------|
| Error Handling | [🔴/🟠/🟡/🟢] | [Brief description] | Lines [X-Y] | [Action needed] |
| Security | [🔴/🟠/🟡/🟢] | [Brief description] | Lines [X-Y] | [Action needed] |
| Performance | [🔴/🟠/🟡/🟢] | [Brief description] | Lines [X-Y] | [Action needed] |
| Best Practices | [🔴/🟠/🟡/🟢] | [Brief description] | Lines [X-Y] | [Action needed] |

---

## Section F: Automated Quality Gate Checks

**Objective**: Provide clear pass/fail criteria for deployment decisions and compliance verification.

### F.1 Quality Gate Criteria

#### ✅ PASS Criteria (All must be met)
- [ ] No CRITICAL security risks present
- [ ] No CRITICAL operational risks present
- [ ] All HIGH security risks have documented mitigation plans with owners
- [ ] All HIGH operational risks have documented mitigation plans with owners
- [ ] Error handling present for all file I/O operations
- [ ] Input validation present for all external data sources
- [ ] No hardcoded credentials or secrets
- [ ] No SQL/Command injection vulnerabilities
- [ ] Audit logging present for critical operations
- [ ] Recovery procedures documented for all error scenarios

#### ⚠️ PASS WITH WARNINGS Criteria
- [ ] Some HIGH risks present but with approved mitigation plans
- [ ] MEDIUM risks present but documented
- [ ] Performance issues identified but not blocking
- [ ] Technical debt documented with remediation roadmap

#### ❌ FAIL Criteria (Any one causes failure)
- [ ] Any CRITICAL security risk present
- [ ] Any CRITICAL operational risk without immediate mitigation
- [ ] Hardcoded credentials detected
- [ ] SQL/Command injection vulnerabilities without mitigation
- [ ] Missing error handling on file operations
- [ ] No input validation on user-provided data
- [ ] Data corruption risk identified
- [ ] System crash scenarios without handling

#### OmniScript-Specific Quality Gates
- [ ] No deprecated OmniScript APIs in use (or migration plan documented)
- [ ] All external integrations have timeout handling
- [ ] Configuration externalized (no hardcoded environment-specific values)
- [ ] Data element validation present for all user inputs
- [ ] Integration error handling follows OmniScript patterns
- [ ] Remote procedure calls have proper authentication/authorization

### F.2 Quality Gate Determination

**Quality Gate Status**: [✅ PASSED / ⚠️ PASSED WITH WARNINGS / ❌ FAILED]

**Justification**: 
[Detailed explanation of why the quality gate passed, passed with warnings, or failed, referencing specific criteria]

**Pass Conditions Met**: [Count] of [Total]
**Fail Conditions Triggered**: [Count]

**Blocking Issues** (if failed):
1. [Issue description with reference to section]
2. [Issue description with reference to section]

**Warning Conditions** (if passed with warnings):
1. [Issue description with reference to section]
2. [Issue description with reference to section]

### F.3 Deployment Recommendation

**Recommendation**: [APPROVE / CONDITIONAL APPROVAL / REJECT]

**Conditions for Approval** (if conditional):
1. [Specific condition that must be met]
2. [Specific condition that must be met]

**Required Remediation** (if rejected):
1. [Specific remediation required with priority and effort estimate]
2. [Specific remediation required with priority and effort estimate]

**Timeline for Re-assessment**: [Date or "After remediation completion"]

### F.4 Compliance Checklist

#### Regulatory Compliance
- [ ] Data privacy requirements met (GDPR/CCPA/etc.)
- [ ] Audit logging sufficient for compliance
- [ ] Data retention policies implemented
- [ ] Access controls documented and enforced

#### Internal Standards Compliance
- [ ] Coding standards followed
- [ ] Security standards met
- [ ] Documentation standards met
- [ ] Testing requirements satisfied

#### Production Readiness
- [ ] Error handling comprehensive
- [ ] Performance acceptable under load
- [ ] Security vulnerabilities addressed
- [ ] Operational risks mitigated
- [ ] Rollback procedures documented

---

## Remediation Roadmap

**Total Issues**: [Count] | **Total Effort**: [Hours/Days] | **Timeline**: [Duration]

### Prioritized Remediation Plan

| Phase | Priority | Issue | Location | Effort | Owner | Blocks Production | Status |
|-------|----------|-------|----------|--------|-------|------------------|--------|
| 1 - Immediate | 🔴 CRITICAL | [Issue] | [Location] | [Est] | [Team] | YES | [Status] |
| 1 - Immediate | 🔴 CRITICAL | [Issue] | [Location] | [Est] | [Team] | YES | [Status] |
| 2 - Pre-Production | 🟠 HIGH | [Issue] | [Location] | [Est] | [Team] | YES | [Status] |
| 2 - Pre-Production | 🟠 HIGH | [Issue] | [Location] | [Est] | [Team] | YES | [Status] |
| 3 - Post-Production | 🟡 MEDIUM | [Issue] | [Location] | [Est] | [Team] | NO | [Status] |
| 3 - Post-Production | 🟡 MEDIUM | [Issue] | [Location] | [Est] | [Team] | NO | [Status] |
| 4 - Tech Debt | 🟢 LOW | [Issue] | [Location] | [Est] | [Team] | NO | [Status] |

**Phase Timelines**:
- **Phase 1 (Immediate)**: [Duration] - BLOCKING
- **Phase 2 (Pre-Production)**: [Duration] - REQUIRED FOR GO-LIVE
- **Phase 3 (Post-Production)**: [Duration] - Next sprint/cycle
- **Phase 4 (Tech Debt)**: [Duration] - Continuous improvement

---

## AI Prompt Template

```
Perform comprehensive code quality assessment for OMNISCRIPT program following CODE_QUALITY_ASSESSMENT template:

Program Source: [Insert/reference]
Focus: [Comprehensive/Security/Performance/Error Handling]

Generate assessment covering:

A. ERROR HANDLING: Mechanisms, runtime scenarios, resource limits, input validation, risk assessment
B. BEST PRACTICES: OmniScript APIs, integration patterns, performance, configuration, data elements, test coverage
C. SECURITY: Critical/High/Medium risks (credentials, injection, validation gaps, file handling), security controls
D. OPERATIONAL RISK: Critical/High/Medium/Low risks (data corruption, crashes, performance, error recovery) + error handling maturity
E. QUALITY SCORING: Overall metrics (0-100 scores), key findings summary by area
F. QUALITY GATES: Evaluate pass/fail/warnings criteria, deployment recommendation, remediation roadmap

For OmniScript-specific items:
- Deprecated API usage and migration paths
- Integration timeout/auth handling
- Configuration externalization
- Data element validation patterns

Provide specific line numbers, severity levels (🔴/🟠/🟡/🟢/⚪), and actionable recommendations.
```

## Usage and Maintenance

**When to Generate**: During Phase 2.3 (error handling reference), after Phase 4 (comprehensive), pre-production (mandatory gate), periodic reviews

**Integration**: Link from Phase 2.3, Phase 4 master docs, and maintenance guides

**Output Location**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`

**Update Triggers**: Significant code changes, pre-deployment, security discoveries, compliance reviews

**Success Criteria**: All sections (A-F) documented, critical findings have owners/timelines, quality gate determined, remediation roadmap created

```

**Create quality assessment document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`
