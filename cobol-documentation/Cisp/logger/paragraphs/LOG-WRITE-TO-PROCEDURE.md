# LOG-WRITE-TO-PROCEDURE Paragraph Documentation

**Program**: LOGGER  
**Paragraph**: LOG-WRITE-TO-PROCEDURE  
**Lines**: 48-52  
**Last Updated**: January 20, 2026

---

## Purpose

LOG-WRITE-TO-PROCEDURE appends log entries to the log file by incrementing the record counter and copying the function name and message from the calling program's LINKAGE SECTION parameters into the file record structure before writing.

---

## Source Code

```cobol
LOG-WRITE-TO-PROCEDURE.
    ADD 1 TO LOG-RECORD-ID.
    MOVE LS-LOG-RECORD-FUNCTION-NAME TO LOG-RECORD-FUNCTION-NAME.
    MOVE LS-LOG-RECORD-MESSAGE TO LOG-RECORD-MESSAGE.
    WRITE LOG-RECORD.
```

---

## Input Requirements

### Expected State
- LOG-FILE must already be open (via prior "OPEN" operation)
- Calling program has issued "ADD" operation via MAIN-PROCEDURE
- LS-LOG-RECORD populated with valid function name and message

### Parameters (via LINKAGE SECTION)
1. **LS-LOG-RECORD-FUNCTION-NAME** (PIC X(40))
   - Source function or program name
   - Must be provided by calling program
   - No validation performed
   
2. **LS-LOG-RECORD-MESSAGE** (PIC X(100))
   - Log message text
   - Must be provided by calling program
   - No sanitization performed

### Prerequisites
- LOG-INIT-PROCEDURE must have been called first
- LOG-FILE currently in open state
- LOG-RECORD-ID contains valid counter value from previous write

---

## Processing Logic

### Step-by-Step Execution

1. **Increment Record Counter**
   ```cobol
   ADD 1 TO LOG-RECORD-ID.
   ```
   - Increments sequential record ID by 1
   - LOG-RECORD-ID serves as sequence number for all log entries
   - No overflow checking (max: 9,999,999,999)
   - **Critical**: Counter incremented BEFORE write, so next write has correct value

2. **Copy Function Name from Caller**
   ```cobol
   MOVE LS-LOG-RECORD-FUNCTION-NAME TO LOG-RECORD-FUNCTION-NAME.
   ```
   - Copies 40 bytes from LINKAGE SECTION to FILE SECTION
   - **No validation**: Function name accepted as-is
   - **Padding**: Shorter names space-padded on right
   - **Truncation**: Names >40 bytes silently truncated

3. **Copy Message from Caller**
   ```cobol
   MOVE LS-LOG-RECORD-MESSAGE TO LOG-RECORD-MESSAGE.
   ```
   - Copies 100 bytes from LINKAGE SECTION to FILE SECTION
   - **No validation**: Message accepted without sanitization
   - **Padding**: Shorter messages space-padded on right
   - **Truncation**: Messages >100 bytes silently truncated
   - **Special Characters**: Newlines, tabs, etc. written as-is

4. **Write Log Record**
   ```cobol
   WRITE LOG-RECORD.
   ```
   - Appends 150-byte record to log file
   - Record format: `[10-digit ID][40-char function][100-char message]`
   - **No error checking**: No FILE STATUS to detect write failures
   - **Sequential**: Records written in order received

---

## Output and Side Effects

### Variables Modified
1. **LOG-RECORD-ID** ← `LOG-RECORD-ID + 1` (incremented)
2. **LOG-RECORD-FUNCTION-NAME** ← `LS-LOG-RECORD-FUNCTION-NAME` (copied from caller)
3. **LOG-RECORD-MESSAGE** ← `LS-LOG-RECORD-MESSAGE` (copied from caller)

### Files Updated
- **LOG-FILE** (`..logs\log.data`):
  - New 150-byte record appended
  - Record contains: incremented ID, function name from caller, message from caller
  - File size increases by 150 bytes per call

### System Side Effects
- Disk I/O operation (may be buffered)
- File position advanced to next record slot
- Disk space consumed

---

## Error Handling

### Potential Error Conditions

1. **Log File Not Open**
   - **Trigger**: "ADD" operation before "OPEN" or after "CLOSE"
   - **Result**: WRITE to closed file
   - **Detection**: None (no FILE STATUS)
   - **Recovery**: None
   - **Impact**: Runtime error or silent failure (depends on COBOL runtime)
   - **Root Cause**: No state tracking to verify file open

2. **Disk Full**
   - **Trigger**: Insufficient disk space for write operation
   - **Result**: WRITE statement fails
   - **Detection**: None (no FILE STATUS)
   - **Recovery**: None
   - **Impact**: Log entry lost, possible file corruption

3. **Permission Denied**
   - **Trigger**: File permissions changed between OPEN and WRITE
   - **Result**: WRITE statement fails
   - **Detection**: None (no FILE STATUS)
   - **Recovery**: None
   - **Impact**: Log entry lost

4. **Record ID Overflow**
   - **Trigger**: 10 billion+ writes (LOG-RECORD-ID > 9,999,999,999)
   - **Result**: Numeric overflow
   - **Detection**: None (no bounds checking)
   - **Recovery**: None
   - **Impact**: Undefined behavior (likely wraps to 0 or causes error)
   - **Likelihood**: Extremely low for typical usage

5. **Invalid Linkage Data**
   - **Trigger**: Calling program passes invalid or uninitialized LS-LOG-RECORD
   - **Result**: Garbage data written to log file
   - **Detection**: None (no validation)
   - **Recovery**: None
   - **Impact**: Corrupted log entries with invalid data

6. **Message Truncation**
   - **Trigger**: LS-LOG-RECORD-MESSAGE > 100 bytes
   - **Result**: Silently truncated to 100 bytes
   - **Detection**: None
   - **Recovery**: None
   - **Impact**: Information loss in log message

### Missing Error Handling
- **No FILE STATUS checking** on WRITE operation
- **No validation** of input parameters (function name, message)
- **No file open state verification**
- **No return code** to inform caller of success/failure
- **No overflow protection** for LOG-RECORD-ID
- **No sanitization** of special characters in message

---

## Performance Characteristics

### Computational Complexity
- **O(1)**: Fixed number of operations (1 ADD + 2 MOVE + 1 WRITE)
- No loops or conditional logic
- Extremely lightweight

### I/O Operations
1. **WRITE LOG-RECORD**: Appends 150-byte record
   - Single I/O operation per call
   - May be buffered by runtime (not immediately flushed to disk)
   - Sequential writes generally fast

### Resource Usage
- **Disk Space**: 150 bytes per log entry
- **Memory**: File I/O buffer (maintained by runtime)
- **CPU**: Minimal (simple data moves)

### Performance Considerations
- **High-Frequency Logging**: Suitable for frequent log writes
- **Buffer Flushing**: Log entries may not be immediately visible until buffer flushes or file closes
- **Sequential I/O**: Optimal for write performance (no seeks)
- **Fixed Record Size**: Predictable disk usage (150 bytes/record)

### Scalability
- **Record Limit**: Theoretical max 9,999,999,999 records (LOG-RECORD-ID constraint)
- **File Size Growth**: 150 bytes per record
  - 1,000 records = 150 KB
  - 1,000,000 records = 150 MB
  - 10,000,000 records = 1.5 GB
- **No Log Rotation**: Unbounded file growth (see recommendations)

---

## Dependencies

### Calls To (PERFORMs)
- None (leaf-level procedure)

### Called By
- **MAIN-PROCEDURE** - When LS-LOG-OPERATION-FLAG = "ADD"

### Data Dependencies
- **Reads**: 
  - LOG-RECORD-ID (FILE SECTION - for increment)
  - LS-LOG-RECORD-FUNCTION-NAME (LINKAGE SECTION)
  - LS-LOG-RECORD-MESSAGE (LINKAGE SECTION)
- **Writes**: 
  - LOG-RECORD-ID (incremented)
  - LOG-RECORD-FUNCTION-NAME (populated from LINKAGE)
  - LOG-RECORD-MESSAGE (populated from LINKAGE)

### File Dependencies
- **LOG-FILE**: Must be open (OUTPUT or EXTEND mode)
- **File State**: Assumes prior successful OPEN operation

---

## Edge Cases and Boundary Conditions

### Edge Case: First Write After Init
- **Scenario**: First "ADD" operation after "OPEN"
- **Behavior**: LOG-RECORD-ID incremented from 1 to 2
- **Result**: Second record in file has ID=2 (first has ID=1 from LOG-INIT)
- **Note**: Sequential numbering maintained

### Edge Case: Multiple Consecutive Writes
- **Scenario**: Hundreds or thousands of "ADD" operations
- **Behavior**: LOG-RECORD-ID increments sequentially for each
- **Result**: Continuous sequence 2, 3, 4, ..., N
- **Performance**: Efficient (sequential I/O)

### Edge Case: Empty Function Name
- **Scenario**: LS-LOG-RECORD-FUNCTION-NAME contains only spaces
- **Behavior**: 40 spaces written to LOG-RECORD-FUNCTION-NAME
- **Result**: Blank function name in log (valid but uninformative)
- **Detection**: None

### Edge Case: Empty Message
- **Scenario**: LS-LOG-RECORD-MESSAGE contains only spaces
- **Behavior**: 100 spaces written to LOG-RECORD-MESSAGE
- **Result**: Blank message in log (valid but uninformative)
- **Detection**: None

### Edge Case: Special Characters in Message
- **Scenario**: LS-LOG-RECORD-MESSAGE contains newlines, tabs, control characters
- **Behavior**: Characters written as-is to log file
- **Result**: May break log parsing or display (e.g., embedded newlines)
- **Risk**: Log format corruption for line-based parsers

### Edge Case: Write Without Open
- **Scenario**: "ADD" operation without prior "OPEN"
- **Behavior**: WRITE to unopened file
- **Result**: Runtime error (behavior varies by COBOL implementation)
- **Detection**: None (no state tracking)

### Edge Case: Record ID Near Maximum
- **Scenario**: LOG-RECORD-ID approaches 9,999,999,999
- **Behavior**: ADD 1 continues incrementing
- **Result**: Eventually exceeds PIC 9(10) capacity
- **Impact**: Overflow error or wrap-around to 0
- **Likelihood**: Extremely rare (billions of records)

---

## Security Considerations

### Input Validation
- **No Validation**: Function name and message accepted without checks
- **Risk**: Malicious or malformed data written to log file
- **Impact**: Log injection attacks possible

### Log Injection Attack
- **Vulnerability**: Special characters in message not sanitized
- **Attack Vector**: Crafted messages with newlines, delimiters, control characters
- **Example**: `MESSAGE = "User login" + NEWLINE + "9999999999Admin                                     SECURITY BREACH!"`
- **Result**: Fake log entries, log parsing corruption
- **Mitigation**: Sanitize input, escape special characters

### Data Integrity
- **No Checksum**: Log records not protected against tampering
- **No Encryption**: Sensitive data written in plaintext
- **Risk**: Log files can be modified without detection

### Buffer Overflow
- **Not Applicable**: Fixed-size MOVE operations (40 and 100 bytes)
- **Truncation**: Long inputs truncated, not overflowed
- **Risk Level**: Low (COBOL handles fixed-length fields safely)

### Resource Exhaustion
- **No Limits**: Unbounded number of log writes possible
- **Risk**: Disk space exhaustion from excessive logging
- **Mitigation**: Implement log rotation, size limits, or quotas

---

## Business Logic

### Business Purpose
Provides centralized log entry recording for COBOL applications. Calling programs pass function name and message, LOGGER handles file I/O and record sequencing.

### Business Rules
1. **Sequential Numbering**: Each log entry receives sequential ID
2. **Caller Identification**: Function name identifies log entry source
3. **Free-Form Messages**: 100-byte message field for arbitrary text
4. **No Filtering**: All "ADD" operations result in log entries (no severity levels)
5. **Append-Only**: Log entries only added, never modified or deleted

### Integration Points
- **Calling Programs**: Any COBOL program with LINKAGE SECTION interface
- **Log Analysis Tools**: Sequential file format suitable for line-by-line parsing
- **Audit Systems**: Sequential IDs enable gap detection for missing records

---

## Recommendations for Improvement

### High Priority
1. **Add FILE STATUS Checking**: Detect write failures
   ```cobol
   WRITE LOG-RECORD.
   IF WS-FILE-STATUS NOT = '00'
       DISPLAY 'Error writing log record: ' WS-FILE-STATUS
       PERFORM LOG-WRITE-ERROR-HANDLER
   END-IF.
   ```

2. **Validate File Open State**: Check before write
   ```cobol
   IF NOT WS-LOG-FILE-OPEN
       DISPLAY 'Log file not open'
       GOBACK
   END-IF.
   ```

3. **Sanitize Input**: Escape or remove special characters
   ```cobol
   PERFORM SANITIZE-MESSAGE USING LS-LOG-RECORD-MESSAGE.
   ```

4. **Add Overflow Check**: Detect LOG-RECORD-ID approaching maximum
   ```cobol
   IF LOG-RECORD-ID > 9999999000
       DISPLAY 'Warning: Log record ID near maximum'
       PERFORM LOG-ROTATION-PROCEDURE
   END-IF.
   ```

### Medium Priority
5. **Add Timestamp**: Record date/time of log entry
6. **Add Severity Level**: Support INFO, WARN, ERROR categories
7. **Return Status Code**: Inform caller of write success/failure
8. **Implement Log Rotation**: Create new file after size/record threshold
9. **Add Record Validation**: Verify function name and message length

### Low Priority
10. **Add Record Checksum**: Detect log tampering
11. **Support Structured Logging**: JSON or XML format option
12. **Add Log Compression**: Compress old log files automatically
13. **Performance Counters**: Track writes per second, error rate

---

## Related Documentation

- **[logger_DATA_DICTIONARY.md](../logger_DATA_DICTIONARY.md)** - LOG-RECORD and LINKAGE SECTION details
- **[MAIN-PROCEDURE.md](MAIN-PROCEDURE.md)** - Dispatcher that calls this procedure
- **[LOG-INIT-PROCEDURE.md](LOG-INIT-PROCEDURE.md)** - Required prior initialization
- **[LOG-CLOSE-PROCEDURE.md](LOG-CLOSE-PROCEDURE.md)** - Matching close operation
- **[../logger_VARIABLE_MUTATIONS.md](../logger_VARIABLE_MUTATIONS.md)** - LOG-RECORD-ID mutation analysis
- **[../logger_ERROR_HANDLING.md](../logger_ERROR_HANDLING.md)** - Comprehensive error analysis
- **[../logger_CALL_GRAPH.md](../logger_CALL_GRAPH.md)** - Control flow diagram

---

## Expert Review Status

**⚠️ AI-GENERATED DOCUMENTATION**: This paragraph documentation was generated by AI and requires expert review.

**Review Checklist**:
- [ ] Verify WRITE behavior for unopened files in GnuCOBOL
- [ ] Confirm LOG-RECORD-ID overflow behavior
- [ ] Validate special character handling in messages
- [ ] Review security implications of log injection vulnerability
- [ ] Assess whether input validation is required
- [ ] Confirm buffer flushing behavior for LINE SEQUENTIAL files
- [ ] Verify recommendations align with organizational standards

**Expert Notes**: _[To be filled during review]_
