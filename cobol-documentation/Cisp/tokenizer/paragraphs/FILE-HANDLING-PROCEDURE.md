# FILE-HANDLING-PROCEDURE

## Overview
**Program**: TOKENIZER  
**Location**: Lines 120-149  
**Purpose**: Read entire LISP source file into memory, excluding comment lines

---

## Business Purpose
Responsible for all file I/O operations. Opens the LISP source file, reads it line-by-line, concatenates non-comment lines into a single working buffer, and closes the file. Provides audit logging for file processing completion.

---

## Input Requirements
- **LS-LISP-FILE-NAME**: Valid file path to LISP source file (from caller)
- **LISP-FILE**: File must exist and be readable
- **WS-LISP-EOF**: Should be uninitialized or set to non-"Y" value

---

## Processing Steps

### 1. File Initialization
```cobol
MOVE LS-LISP-FILE-NAME TO WS-LISP-NAME
OPEN INPUT LISP-FILE
```
- Copies linkage parameter to working storage for dynamic file assignment
- Opens file in INPUT mode (read-only)

### 2. Read First Record
```cobol
READ LISP-FILE
    AT END MOVE "Y" TO WS-LISP-EOF
    NOT AT END
        MOVE IN-LISP-RECORD TO WS-CALC-LENGTH-STR
        PERFORM CALC-LISP-LENGTH
        IF NOT WS-IS-COMMENT-YES THEN
            MOVE IN-LISP-RECORD TO WS-IN-LISP-RECORD
            MOVE WS-LISP-LENGTH TO WS-TEMP-NUM
        END-IF
END-READ
```
- **AT END branch**: Sets EOF flag if file is empty
- **NOT AT END branch**:
  - Calculates actual record length
  - Checks for comment line (starts with ";")
  - If not comment: Initializes WS-IN-LISP-RECORD buffer
  - Stores initial length in WS-TEMP-NUM

### 3. Read Remaining Records (Loop)
```cobol
PERFORM UNTIL WS-LISP-EOF="Y"
    READ LISP-FILE
        AT END MOVE "Y" TO WS-LISP-EOF
        NOT AT END PERFORM APPEND-LISP-PROCEDURE
    END-READ
END-PERFORM
```
- Continues reading until end-of-file
- Each record: calls APPEND-LISP-PROCEDURE for concatenation
- EOF flag terminates loop

### 4. File Cleanup
```cobol
CLOSE LISP-FILE
```
- Closes file handle
- Releases file resources

### 5. Audit Logging
```cobol
MOVE "ADD" TO WS-LOG-OPERATION-FLAG
MOVE "TOKENIZER:FILE-HANDLING-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME
MOVE "COMPLETED reading LISP-FILE" TO WS-LOG-RECORD-MESSAGE
CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD
```
- Logs successful completion to LOGGER program
- Records function name and completion message

---

## Output and Side Effects

### Modified Variables
- **WS-LISP-NAME**: Set to input file name
- **WS-IN-LISP-RECORD**: Contains complete LISP file content (excluding comments)
- **WS-LISP-EOF**: Set to "Y" when file reading complete
- **WS-TEMP-NUM**: Contains total character count of accumulated content
- **WS-LISP-LENGTH**: Set by CALC-LISP-LENGTH for last record
- **WS-IS-COMMENT**: Set by CALC-LISP-LENGTH based on line content

### External Calls
- **CALL 'LOGGER'**: Logs file handling completion

### File Operations
- **OPEN INPUT**: Opens LISP-FILE
- **READ** (multiple): Reads all records from file
- **CLOSE**: Closes LISP-FILE

---

## Dependencies

### Called Paragraphs
- **CALC-LISP-LENGTH**: Calculate record length and detect comments
- **APPEND-LISP-PROCEDURE**: Concatenate additional records to buffer

### Data Dependencies
- **LS-LISP-FILE-NAME**: Input parameter from caller
- **LISP-FILE**: File descriptor (FILE SECTION)
- **IN-LISP-RECORD**: Input buffer (FD record)

---

## Error Handling

### Implicit COBOL Error Handling
- **File not found**: COBOL runtime error (program abends)
- **Permission denied**: COBOL runtime error (program abends)
- **Empty file**: Handled gracefully (sets EOF immediately)

### Missing Explicit Error Handling
- No STATUS clause on SELECT statement
- No explicit check for OPEN failure
- No validation of file path before OPEN
- Could benefit from FILE STATUS checking

---

## Business Rules

### Comment Detection
- Lines starting with ";" are LISP comments
- Comment lines are completely excluded from WS-IN-LISP-RECORD
- Comment detection happens in CALC-LISP-LENGTH procedure

### Multi-line Concatenation
- First non-comment line: Direct copy to WS-IN-LISP-RECORD
- Subsequent lines: Concatenated via APPEND-LISP-PROCEDURE
- No explicit line separators added (LISP doesn't require them)

### Length Tracking
- WS-TEMP-NUM accumulates total character count
- WS-LISP-LENGTH contains length of current record
- Used for precise string concatenation positioning

---

## Performance Considerations
- **I/O Bound**: Performance limited by file system read speed
- **Line-by-line**: Inefficient for very large files (no buffering)
- **String Concatenation**: Multiple STRING operations (expensive in COBOL)
- **Optimization Opportunity**: Could read entire file into array, then process

---

## Example Execution

### Input File Content:
```lisp
; This is a comment
(defun foo (x)
  (+ x 1))
```

### Processing Flow:
1. **First READ**: `; This is a comment`
   - CALC-LISP-LENGTH detects ";" → WS-IS-COMMENT-YES set
   - Skipped (not moved to WS-IN-LISP-RECORD)

2. **Second READ**: `(defun foo (x)`
   - Not comment → WS-IN-LISP-RECORD = `(defun foo (x)`
   - WS-TEMP-NUM = 14

3. **Third READ**: `  (+ x 1))`
   - APPEND-LISP-PROCEDURE concatenates
   - WS-IN-LISP-RECORD = `(defun foo (x)(+ x 1))`
   - WS-TEMP-NUM = 23

4. **Fourth READ**: AT END → WS-LISP-EOF = "Y"

### Final State:
- WS-IN-LISP-RECORD = `(defun foo (x)(+ x 1))`
- WS-LISP-EOF = "Y"
- File closed
- LOGGER called

---

## Maintenance Notes
- Consider adding FILE STATUS checking for production robustness
- Large LISP files may exceed 200-byte WS-IN-LISP-RECORD limit
- Comment detection only handles single-line comments (no block comments)
- No whitespace normalization (leading spaces preserved from original lines)

---

*This documentation is AI-generated and should be reviewed by COBOL experts for accuracy.*
