# MAIN-PROCEDURE

## Overview
**Program**: TOKENIZER  
**Location**: Lines 77-83  
**Purpose**: Entry point and orchestrator for LISP file tokenization workflow

---

## Business Purpose
Main control procedure that coordinates the complete tokenization process for LISP source files. It sequences file reading, tokenization, and symbol length calculation operations.

---

## Input Requirements
- **LS-LISP-FILE-NAME**: Must contain valid file path to LISP source file
- **LS-LISP-SYMBOLS**: Pre-allocated symbol table structure (100-element array)

---

## Processing Steps

1. **File Handling Phase**
   ```cobol
   PERFORM FILE-HANDLING-PROCEDURE
   ```
   - Opens and reads entire LISP file
   - Concatenates multi-line input into single string
   - Filters out comment lines (starting with ";")
   - Result stored in WS-IN-LISP-RECORD

2. **Tokenization Phase**
   ```cobol
   PERFORM TOKENIZE-LISP-PROCEDURE
   ```
   - Formats LISP string (adds spaces around parentheses)
   - Splits formatted string into individual tokens
   - Stores tokens in LS-SYMBOL array
   - Updates LS-SYMBOL-TABLE-SIZE with count

3. **Length Calculation Phase**
   ```cobol
   PERFORM CAL-LENGTH-ALL-SYMBOLS
   ```
   - Calculates actual length of each token
   - Stores lengths in LS-SYMBOL-LEN array
   - Processes all 100 array slots

4. **Debug Output (Optional)**
   ```cobol
   D    PERFORM PRINT-SYMBOL-TABLE
   ```
   - Debug directive (D) - only runs in debug mode
   - Displays tokenized symbols with lengths

5. **Program Termination**
   ```cobol
   GOBACK
   ```
   - Returns control to calling program (CISP)
   - Passes populated LS-LISP-SYMBOLS structure back

---

## Output and Side Effects

### Modified Linkage Section Variables
- **LS-SYMBOL(1...100)**: Populated with tokenized LISP elements
- **LS-SYMBOL-LEN(1...100)**: Populated with token lengths (0-50)
- **LS-SYMBOL-TABLE-SIZE**: Set to actual number of tokens found

### Modified Working Storage Variables
- **WS-IN-LISP-RECORD**: Contains complete formatted LISP string
- **WS-LISP-LENGTH**: Contains calculated string length
- **WS-TEMP-NUM**: Contains accumulated file content length

### External Calls
- **LOGGER program**: Called 3 times (by sub-procedures) for audit logging

---

## Dependencies

### Called Paragraphs
1. FILE-HANDLING-PROCEDURE
2. TOKENIZE-LISP-PROCEDURE
3. CAL-LENGTH-ALL-SYMBOLS
4. PRINT-SYMBOL-TABLE (debug only)

### Data Dependencies
- Requires WS-LISP-EOF initialized
- Requires LS-LISP-FILE-NAME populated by caller
- Requires LISP-FILE accessible from file system

---

## Error Handling
- **No explicit error handling** in this procedure
- Relies on sub-procedures to handle errors
- File access errors will cause runtime COBOL error (not trapped)
- Array overflow silently truncates at 100 tokens

---

## Business Rules
1. Only non-comment lines are tokenized (comments start with ";")
2. Maximum 100 tokens can be processed
3. Sequential processing - no parallel operations
4. Debug output disabled by default (requires D compiler directive)

---

## Notes
- Simple sequential orchestration pattern
- Clean separation of concerns (file I/O, formatting, tokenization, length calc)
- GOBACK used instead of STOP RUN (proper subroutine behavior)
- Debug displays commented with D directive for conditional compilation

---

*This documentation is AI-generated and should be reviewed by COBOL experts for accuracy.*
