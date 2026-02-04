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

### A.1 Error Status Analysis

#### Error Handling Mechanisms Inventory
Document all error handling approaches used in the program:

| Mechanism Type | Location | Coverage | Adequacy | Risk Level |
|---------------|----------|----------|----------|------------|
| Try-Catch Blocks | Lines [X-Y], Proc [NAME] | [Complete/Partial/Missing] | [Adequate/Inadequate] | [Level] |
| Status Code Checks | Lines [X-Y], Proc [NAME] | [Complete/Partial/Missing] | [Adequate/Inadequate] | [Level] |
| Error Flags/Variables | Lines [X-Y], Proc [NAME] | [Complete/Partial/Missing] | [Adequate/Inadequate] | [Level] |
| Return Code Validation | Lines [X-Y], Proc [NAME] | [Complete/Partial/Missing] | [Adequate/Inadequate] | [Level] |

#### Status Codes Documentation
List all error status codes and their handling:

| Status Code | Meaning | Handler Location | Recovery Procedure | Completeness |
|------------|---------|------------------|-------------------|--------------|
| [Code] | [Description] | Lines [X-Y] | [What happens] | [Adequate/Partial/None] |

#### Operations Without Error Checking
**🔴 CRITICAL GAPS**: Operations that lack error handling:

| Operation Type | Location | Potential Failure | Impact | Priority |
|---------------|----------|-------------------|--------|----------|
| File Open | Lines [X-Y] | File not found, access denied | [Impact description] | [IMMEDIATE/HIGH] |
| Database Query | Lines [X-Y] | Connection failure, timeout | [Impact description] | [IMMEDIATE/HIGH] |
| External Call | Lines [X-Y] | Program unavailable, parameter error | [Impact description] | [HIGH/MEDIUM] |

### A.2 Runtime Error Scenarios

Analyze potential runtime error scenarios and their handling:

#### File Operations
| Scenario | Likelihood | Current Handling | Impact if Unhandled | Recommendation |
|----------|------------|------------------|---------------------|----------------|
| File not found | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Access denied | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Disk full | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Corrupt file | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |

#### Data Processing
| Scenario | Likelihood | Current Handling | Impact if Unhandled | Recommendation |
|----------|------------|------------------|---------------------|----------------|
| Invalid data format | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Type mismatch | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Buffer overflow | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Division by zero | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| String overflow | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Array out of bounds | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |
| Null/empty reference | [High/Medium/Low] | [Description or "None"] | [Impact] | [Action needed] |

### A.3 Resource Limits Documentation

#### Buffer and Memory Limits
| Resource | Maximum Size | Overflow Protection | Overflow Impact | Risk Level |
|----------|-------------|---------------------|-----------------|------------|
| String buffers | [Size] | [Yes/No - Method] | [What happens] | [Level] |
| Arrays | [Size] | [Yes/No - Method] | [What happens] | [Level] |
| File buffers | [Size] | [Yes/No - Method] | [What happens] | [Level] |
| Memory allocation | [Size] | [Yes/No - Method] | [What happens] | [Level] |

#### Operational Limits
| Limit Type | Maximum Value | Enforcement | Exceeded Handling | Documentation |
|-----------|---------------|-------------|------------------|---------------|
| Record count | [Count] | [Yes/No] | [What happens] | [Where documented] |
| Token count | [Count] | [Yes/No] | [What happens] | [Where documented] |
| File size | [Size] | [Yes/No] | [What happens] | [Where documented] |
| Transaction size | [Size] | [Yes/No] | [What happens] | [Where documented] |

### A.4 Input Validation

#### File Path Validation
- **Validation Present**: [Yes/No/Partial]
- **Sanitization Method**: [Description or "None"]
- **Path Traversal Protection**: [Yes/No]
- **Injection Protection**: [Yes/No]
- **Risk Assessment**: [Level with justification]

#### Data Format Validation
| Input Type | Validation Method | Completeness | Risk if Missing |
|-----------|------------------|--------------|-----------------|
| Numeric fields | [Method or "None"] | [Complete/Partial/None] | [Risk level + description] |
| Date fields | [Method or "None"] | [Complete/Partial/None] | [Risk level + description] |
| String fields | [Method or "None"] | [Complete/Partial/None] | [Risk level + description] |
| File formats | [Method or "None"] | [Complete/Partial/None] | [Risk level + description] |

#### Range and Boundary Checking
| Field/Variable | Expected Range | Range Check Present | Boundary Handling |
|---------------|----------------|---------------------|-------------------|
| [Field name] | [Min-Max] | [Yes/No] | [How handled] |

### A.5 Error Handling Risk Assessment

#### Overall Error Handling Maturity
- **Maturity Level**: [None / Minimal / Partial / Comprehensive / Exemplary]
- **Coverage Percentage**: [Estimate]% of critical operations covered
- **Consistency**: [Inconsistent / Partially Consistent / Fully Consistent]

#### Risk Categories
**🔴 HIGH RISK - Immediate Action Required**:
1. [Description of high risk item with location]
2. [Description of high risk item with location]

**🟠 MEDIUM RISK - Address Before Production**:
1. [Description of medium risk item with location]
2. [Description of medium risk item with location]

**🟢 LOW RISK - Monitor and Improve**:
1. [Description of low risk item with location]

#### Recommended Improvements
| Priority | Improvement | Location | Effort Estimate | Impact |
|----------|-------------|----------|-----------------|--------|
| IMMEDIATE | [Description] | Lines [X-Y] | [Hours/Days] | [Prevents critical failure] |
| HIGH | [Description] | Lines [X-Y] | [Hours/Days] | [Prevents data corruption] |
| MEDIUM | [Description] | Lines [X-Y] | [Hours/Days] | [Improves reliability] |

---

## Section B: OmniScript/COBOL Best Practices Assessment

**Objective**: Evaluate adherence to language-specific best practices and identify anti-patterns.

### B.1 OmniScript-Specific Quality Checks

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

**Loop Structures**:
| Loop Location | Type | Complexity | Optimization Status | Recommendation |
|--------------|------|------------|---------------------|----------------|
| Lines [X-Y] | [FOR/WHILE/etc] | [O(n)/O(n²)/etc] | [Optimal/Suboptimal] | [Suggestion] |

**String Operations**:
- **Total String Operations**: [Count]
- **Concatenation in Loops**: [Count] (potential performance issue)
- **String Buffer Usage**: [Optimal/Suboptimal]

| Operation | Location | Frequency | Impact | Optimization Opportunity |
|-----------|----------|-----------|--------|-------------------------|
| [Operation] | Lines [X-Y] | [Count/loop] | [High/Medium/Low] | [Suggestion] |

**File I/O Efficiency**:
- **Buffering**: [Optimal/Suboptimal/None]
- **Read/Write Patterns**: [Efficient/Inefficient]
- **Transaction Management**: [Present/Missing]

### B.2 COBOL-Specific Quality Checks

**Note**: Apply only if program contains COBOL code or interacts with COBOL programs.

#### GOTO Usage Analysis
- **Total GOTO Statements**: [Count]
- **Assessment**: [Acceptable / Excessive / Anti-pattern]

| GOTO Location | Target | Justification | Refactoring Opportunity |
|--------------|--------|---------------|------------------------|
| Lines [X-Y] | [Label] | [Reason or "None"] | [Yes/No - Suggestion] |

#### PERFORM Best Practices
- **PERFORM Usage**: [Structured/Mixed/Unstructured]
- **Paragraph Organization**: [Well-organized/Needs improvement]

| Issue Type | Location | Current Pattern | Best Practice | Priority |
|-----------|----------|-----------------|---------------|----------|
| [Issue] | Lines [X-Y] | [Description] | [Recommendation] | [HIGH/MEDIUM/LOW] |

#### Data Division Organization
- **Organization Quality**: [Excellent/Good/Fair/Poor]
- **Logical Grouping**: [Present/Inconsistent/Missing]
- **Naming Conventions**: [Consistent/Inconsistent]

#### File Handling
- **OPEN/CLOSE Sequences**: [Proper/Improper]
- **Error Checking**: [Comprehensive/Partial/Missing]

| File Operation | Location | Issue | Risk | Recommendation |
|---------------|----------|-------|------|----------------|
| [Operation] | Lines [X-Y] | [Issue or "None"] | [Level] | [Action] |

#### WORKING-STORAGE Efficiency
- **Memory Footprint**: [Optimal/Acceptable/Excessive]
- **Redef Usage**: [Appropriate/Questionable]
- **01-Level Organization**: [Well-structured/Needs improvement]

---

## Section C: Security and Safety Assessment

**Objective**: Identify security vulnerabilities, safety risks, and compliance gaps.

### C.1 Critical Security Risks (🔴 CRITICAL)

**Immediate remediation required** - These issues pose severe security threats:

| Vulnerability | Location | Description | Exploit Scenario | Remediation | Effort |
|--------------|----------|-------------|------------------|-------------|--------|
| Hardcoded Credentials | Lines [X-Y] | [Details] | [How it could be exploited] | [Required fix] | [Estimate] |
| SQL Injection | Lines [X-Y] | [Details] | [How it could be exploited] | [Required fix] | [Estimate] |
| Command Injection | Lines [X-Y] | [Details] | [How it could be exploited] | [Required fix] | [Estimate] |
| Path Traversal | Lines [X-Y] | [Details] | [How it could be exploited] | [Required fix] | [Estimate] |
| Auth Bypass | Lines [X-Y] | [Details] | [How it could be exploited] | [Required fix] | [Estimate] |

**Critical Security Findings Count**: [Count]
**Block Deployment**: [YES / NO] - [Justification]

### C.2 High Security Risks (🟠 HIGH)

**Address before production** - Significant security concerns:

| Risk Type | Location | Description | Potential Impact | Mitigation | Priority |
|-----------|----------|-------------|------------------|------------|----------|
| Missing Input Validation | Lines [X-Y] | [Details] | [Impact] | [Fix] | [HIGH/MEDIUM] |
| Error Info Disclosure | Lines [X-Y] | [Details] | [Impact] | [Fix] | [HIGH/MEDIUM] |
| Insecure File Handling | Lines [X-Y] | [Details] | [Impact] | [Fix] | [HIGH/MEDIUM] |
| Race Conditions | Lines [X-Y] | [Details] | [Impact] | [Fix] | [HIGH/MEDIUM] |
| Access Control Gaps | Lines [X-Y] | [Details] | [Impact] | [Fix] | [HIGH/MEDIUM] |

**High Security Findings Count**: [Count]

### C.3 Medium Security Risks (🟡 MEDIUM)

**Address in next cycle** - Moderate security concerns:

| Risk Type | Location | Description | Mitigation Strategy |
|-----------|----------|-------------|---------------------|
| Weak Input Sanitization | Lines [X-Y] | [Details] | [Recommendation] |
| Insufficient Logging | Lines [X-Y] | [Details] | [Recommendation] |
| Deprecated Crypto | Lines [X-Y] | [Details] | [Recommendation] |

### C.4 Security Posture Summary

#### Security Controls Present
- ✅ **Authentication**: [Present/Missing/Inadequate]
- ✅ **Authorization**: [Present/Missing/Inadequate]
- ✅ **Input Validation**: [Comprehensive/Partial/Missing]
- ✅ **Output Encoding**: [Present/Missing]
- ✅ **Error Handling**: [Secure/Exposes internals]
- ✅ **Audit Logging**: [Comprehensive/Basic/Missing]
- ✅ **Data Encryption**: [Present/Missing/N/A]
- ✅ **Session Management**: [Secure/Insecure/N/A]

#### Compliance Considerations
- **Data Privacy**: [GDPR/CCPA/Other compliance requirements]
- **Audit Trail**: [Complete/Incomplete/Missing]
- **Data Retention**: [Compliant/Non-compliant]
- **Access Logging**: [Present/Missing]

---

## Section D: Operational Risk Assessment

**Objective**: Assess operational risks that could impact system stability, data integrity, or business operations.

### D.1 Critical Operational Risks (🔴 CRITICAL)

**Immediate action required** - Risk of system failure or data loss:

| Risk | Location | Scenario | Impact | Likelihood | Mitigation | Owner |
|------|----------|----------|--------|------------|------------|-------|
| Data Corruption | Lines [X-Y] | [Scenario] | [Business impact] | [High/Medium] | [Required action] | [Team] |
| System Crash | Lines [X-Y] | [Scenario] | [Business impact] | [High/Medium] | [Required action] | [Team] |
| Unrecoverable Error | Lines [X-Y] | [Scenario] | [Business impact] | [High/Medium] | [Required action] | [Team] |
| Silent Data Loss | Lines [X-Y] | [Scenario] | [Business impact] | [High/Medium] | [Required action] | [Team] |

**Critical Risk Count**: [Count]
**Production Readiness**: [BLOCKED / CONDITIONAL / CLEARED]

### D.2 High Operational Risks (🟠 HIGH)

**Address before production** - Significant operational concerns:

| Risk | Location | Description | Impact | Mitigation Plan | Timeline |
|------|----------|-------------|--------|-----------------|----------|
| Performance Degradation | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Timeline] |
| Resource Exhaustion | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Timeline] |
| Error Recovery Gap | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Timeline] |
| Data Inconsistency | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Timeline] |
| Integration Failure | Lines [X-Y] | [Details] | [Impact] | [Plan] | [Timeline] |

### D.3 Medium Operational Risks (🟡 MEDIUM)

**Address in next cycle** - Should be improved but not blocking:

| Risk | Location | Description | Impact | Recommendation |
|------|----------|-------------|--------|----------------|
| Suboptimal Performance | Lines [X-Y] | [Details] | [Impact] | [Action] |
| Minor Resource Leak | Lines [X-Y] | [Details] | [Impact] | [Action] |
| Inconsistent Handling | Lines [X-Y] | [Details] | [Impact] | [Action] |
| Maintainability Issue | Lines [X-Y] | [Details] | [Impact] | [Action] |

### D.4 Low Operational Risks (🟢 LOW)

**Nice to have** - Minor improvements:

| Risk | Description | Recommendation |
|------|-------------|----------------|
| Code Style Inconsistency | [Details] | [Suggestion] |
| Minor Inefficiency | [Details] | [Suggestion] |
| Documentation Gap | [Details] | [Suggestion] |

### D.5 Informational (⚪ INFO)

**No action required** - Observations and opportunities:

- [Best practice suggestion]
- [Optimization opportunity]
- [Refactoring recommendation]

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

### E.2 Quality Assessment by Section/Procedure

For each major section or procedure, provide detailed assessment:

#### Quality Assessment: [Procedure/Section Name]

**Overall Risk Level**: [🔴 CRITICAL / 🟠 HIGH / 🟡 MEDIUM / 🟢 LOW]

**Risk Breakdown**:
- Security: [🔴/🟠/🟡/🟢] ([Count] issues)
- Operational: [🔴/🟠/🟡/🟢] ([Count] issues)
- Performance: [🔴/🟠/🟡/🟢] ([Count] issues)
- Maintainability: [🔴/🟠/🟡/🟢] ([Count] issues)

**Critical Findings**:

##### [Risk Level]: [Finding Title]
- **Location**: Line(s) [X-Y], Procedure [NAME]
- **Issue**: [Detailed description]
- **Impact**: [Business and technical impact]
- **Recommendation**: [Specific remediation steps]
- **Effort**: [Hours/Days estimate]
- **Priority**: [IMMEDIATE/HIGH/MEDIUM/LOW]

[Repeat for each finding]

**Best Practice Violations**:

##### OmniScript-Specific:
- ❌ [Violation description] (Location: Lines [X-Y])
  - **Recommendation**: [Specific action]
  - **Effort**: [Estimate]

##### COBOL-Specific (if applicable):
- ❌ [Violation description] (Location: Lines [X-Y])
  - **Recommendation**: [Specific action]
  - **Effort**: [Estimate]

**Performance Impact**:
- **Current Complexity**: [O(n), O(n²), etc.]
- **Potential Improvement**: [O(n), etc.]
- **Estimated Performance Gain**: [Percentage or description]
- **Optimization Effort**: [Estimate]

**Security Posture**:
- **Authentication**: [✅ Present / ❌ Missing / ⚠️ Inadequate]
- **Input Validation**: [✅ Comprehensive / ⚠️ Partial / ❌ Missing]
- **Error Disclosure**: [✅ Secure / ❌ Exposes internals]
- **Audit Logging**: [✅ Present / ⚠️ Partial / ❌ Missing]

**Recommended Actions** (Prioritized):
1. **IMMEDIATE**: [Action with location and brief justification]
2. **Before Production**: [Action with location and brief justification]
3. **Next Sprint/Cycle**: [Action with location and brief justification]
4. **Technical Debt**: [Action with location and brief justification]

[Repeat assessment for each major procedure/section]

### E.3 Trend Analysis

**Historical Comparison** (if previous assessments available):
- Quality Score Trend: [Improving/Stable/Declining]
- Critical Issues Trend: [Decreasing/Stable/Increasing]
- Technical Debt: [Reduced/Stable/Growing]

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

**Total Issues**: [Count]
**Total Estimated Effort**: [Hours/Days]
**Recommended Timeline**: [Duration]

### Phase 1: Immediate Actions (Do Now)
**Duration**: [Estimate]
**Blocking Production**: YES

| Priority | Issue | Location | Effort | Owner | Status |
|----------|-------|----------|--------|-------|--------|
| 🔴 CRITICAL | [Issue] | [Location] | [Estimate] | [Team/Person] | [Not Started/In Progress/Complete] |

### Phase 2: Pre-Production (Before Go-Live)
**Duration**: [Estimate]
**Required for Production**: YES

| Priority | Issue | Location | Effort | Owner | Status |
|----------|-------|----------|--------|-------|--------|
| 🟠 HIGH | [Issue] | [Location] | [Estimate] | [Team/Person] | [Status] |

### Phase 3: Post-Production (Next Sprint/Cycle)
**Duration**: [Estimate]
**Required for Production**: NO

| Priority | Issue | Location | Effort | Owner | Status |
|----------|-------|----------|--------|-------|--------|
| 🟡 MEDIUM | [Issue] | [Location] | [Estimate] | [Team/Person] | [Status] |

### Phase 4: Technical Debt Reduction
**Duration**: [Estimate]
**Type**: Continuous Improvement

| Priority | Issue | Location | Effort | Owner | Status |
|----------|-------|----------|--------|-------|--------|
| 🟢 LOW | [Issue] | [Location] | [Estimate] | [Team/Person] | [Status] |

---

## AI Prompt Templates

### Comprehensive Quality Assessment Prompt

```
Perform a comprehensive code quality assessment for this OMNISCRIPT program following the CODE_QUALITY_ASSESSMENT template:

Program Source: [Insert or reference]
Static Analysis Reports: [Insert or reference]
Previous Documentation: [Insert references to data dictionary, procedure docs, etc.]

Generate assessment covering:

A. ERROR HANDLING ANALYSIS
   - Identify all error handling mechanisms
   - Document runtime error scenarios
   - Assess resource limits and buffer risks
   - Evaluate input validation
   - Provide risk assessment (High/Medium/Low)

B. BEST PRACTICES ASSESSMENT
   - OmniScript API usage patterns
   - Integration patterns (external calls, parameters, return codes)
   - Performance patterns (loops, string ops, file I/O)
   - COBOL patterns (if applicable): GOTO usage, PERFORM, file handling

C. SECURITY ASSESSMENT
   - Critical risks: hardcoded credentials, injection vulnerabilities
   - High risks: missing validation, info disclosure, file handling
   - Medium risks: sanitization, logging, deprecated functions
   - Security posture summary

D. OPERATIONAL RISK ASSESSMENT
   - Critical: data corruption, crashes, unrecoverable errors
   - High: performance issues, resource exhaustion, error recovery
   - Medium: suboptimal patterns, minor leaks, maintainability
   - Low/Informational: style, minor inefficiencies, opportunities

E. QUALITY SCORING
   - Overall metrics (0-100 scores with grades)
   - Per-procedure assessments with risk breakdowns
   - Best practice violations with specific recommendations
   - Performance impact analysis
   - Security posture evaluation

F. QUALITY GATE CHECKS
   - Evaluate against pass/fail criteria
   - Determine deployment readiness
   - Provide conditional approval requirements
   - Generate remediation roadmap with timeline

Provide specific line numbers, concrete examples, and actionable recommendations.
Categorize all findings by severity (🔴 CRITICAL / 🟠 HIGH / 🟡 MEDIUM / 🟢 LOW / ⚪ INFO).
```

### Targeted Security Assessment Prompt

```
Perform focused security assessment for [Procedure/Module Name]:

Source Code: [Insert]
Focus Areas: [e.g., "File handling", "External calls", "Input processing"]

Analyze for:
1. Critical security risks (hardcoded credentials, injection, path traversal, auth bypass)
2. Input validation gaps
3. Error information disclosure
4. Insecure file/resource handling
5. Access control weaknesses

For each finding provide:
- Severity (🔴/🟠/🟡)
- Location (line numbers)
- Exploit scenario
- Business impact
- Specific remediation steps
- Effort estimate
```

### Quality Gate Evaluation Prompt

```
Evaluate code quality gate status using the quality assessment:

Quality Assessment Document: [Reference]

Evaluate against criteria:

PASS CRITERIA (all must be met):
- No CRITICAL security/operational risks
- HIGH risks have mitigation plans
- Error handling for file I/O
- Input validation for external data
- No credentials/injection vulnerabilities

FAIL CRITERIA (any triggers failure):
- CRITICAL risks present
- Hardcoded credentials
- Injection vulnerabilities
- Missing file error handling
- No input validation
- Data corruption risk

Provide:
- Quality gate status (✅ PASS / ⚠️ PASS WITH WARNINGS / ❌ FAIL)
- Justification with specific references
- Deployment recommendation
- Remediation roadmap if failed/warnings
```

---

## Integration with Documentation Workflow

**When to Generate Quality Assessment**:
- **During Phase 2.3**: Reference error handling analysis from this template
- **After Phase 4**: Perform comprehensive assessment once all documentation complete
- **Pre-Production**: Mandatory quality gate check before deployment
- **Periodic Review**: Regular assessments for maintained code

**Integration Points**:
- **Phase 2.3 Error Handling**: Brief reference, links to Section A of this template
- **Phase 4 Master Documentation**: Links to quality assessment in comprehensive doc
- **Master Index**: Include link to quality assessment document
- **Maintenance Guide**: Reference quality gates for ongoing changes

**Output Location**:
Create quality assessment document: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`

---

## Maintenance and Updates

**Update Triggers**:
- After any significant code modification
- Before production deployments
- After security vulnerability discoveries
- Periodic compliance reviews (quarterly/annually)
- When quality gate criteria change

**Version Control**:
- Track assessment versions with dates
- Link assessments to code versions/commits
- Maintain historical trend data
- Document remediation completion

---

## Success Criteria

✅ **Quality Assessment Complete When**:
- [ ] All sections (A-F) thoroughly documented
- [ ] Every critical finding has owner and timeline
- [ ] Quality gate status determined with justification
- [ ] Remediation roadmap created with effort estimates
- [ ] Assessment reviewed by security team (for critical systems)
- [ ] Assessment reviewed by operations team
- [ ] Expert review and approval obtained
- [ ] Assessment linked from master index and comprehensive documentation

```

**Create quality assessment document**: `${OMNISCRIPT_DOCS_DIR}/[PROGRAM-NAME]_QUALITY_ASSESSMENT.md`
