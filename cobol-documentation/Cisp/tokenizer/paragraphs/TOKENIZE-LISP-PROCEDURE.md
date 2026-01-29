# TOKENIZE-LISP-PROCEDURE

## Overview
**Program**: TOKENIZER  
**Location**: Lines 150-169  
**Purpose**: Format and tokenize LISP string into individual symbols

---

## Business Purpose
Core tokenization engine that transforms the raw LISP string into a structured symbol table. First formats the string by adding spaces around parentheses, then splits it into individual tokens using COBOL's UNSTRING operation.

---

## Input Requirements
- **WS-IN-LISP-RECORD**: Complete LISP content (from FILE-HANDLING-PROCEDURE)
- **LS-SYMBOL**: Empty array ready to receive tokens
- **LS-SYMBOL-TABLE-SIZE**: Should be initialized to 0

---

## Processing Steps

### 1. Format LISP String
```cobol
PERFORM FORMAT-LISP-PROCEDURE
```
- Adds spaces around all parentheses: `(foo)` → `( foo )`
- Ensures consistent tokenization by space delimiter
- Modifies WS-IN-LISP-RECORD in-place

### 2. Initialize Tokenization State
```cobol
MOVE 1 TO STRING-PTR
MOVE 0 TO LS-SYMBOL-TABLE-SIZE
SET WS-FLAG-YES TO FALSE
```
- **STRING-PTR**: UNSTRING pointer starts at position 1
- **LS-SYMBOL-TABLE-SIZE**: Reset symbol counter to 0
- **WS-FLAG**: Loop control flag (false = continue)

### 3. Tokenize Loop
```cobol
PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT = 100 OR WS-FLAG
    UNSTRING WS-IN-LISP-RECORD DELIMITED BY ALL ' ' 
        INTO LS-SYMBOL(WS-COUNT) 
        WITH POINTER STRING-PTR
    IF LS-SYMBOL(WS-COUNT) = SPACES THEN
        SET WS-FLAG-YES TO TRUE
    ELSE
        ADD 1 TO LS-SYMBOL-TABLE-SIZE
    END-IF
END-PERFORM
```
- **Loop control**: Stops at 100 tokens OR empty token found
- **UNSTRING operation**:
  - Delimiter: `ALL ' '` (any sequence of spaces)
  - Automatically advances STRING-PTR after each extraction
  - Extracts next token into LS-SYMBOL(WS-COUNT)
- **Empty token detection**:
  - When no more tokens: UNSTRING fills with SPACES
  - Sets WS-FLAG-YES to terminate loop early
- **Symbol counting**:
  - Only non-empty tokens increment LS-SYMBOL-TABLE-SIZE
  - Final count reflects actual number of tokens

### 4. Audit Logging
```cobol
MOVE "ADD" TO WS-LOG-OPERATION-FLAG
MOVE "TOKENIZER:TOKENIZE-LISP-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME
MOVE "COMPLETED tokenizing lisp" TO WS-LOG-RECORD-MESSAGE
CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD
```
- Logs successful tokenization completion

---

## Output and Side Effects

### Modified Variables
- **LS-SYMBOL(1...n)**: Populated with individual tokens
- **LS-SYMBOL-TABLE-SIZE**: Set to actual token count (1 to 100)
- **STRING-PTR**: Final position after last UNSTRING
- **WS-FLAG**: Set to TRUE when tokenization complete
- **WS-COUNT**: Final loop counter value

### External Calls
- **CALL 'LOGGER'**: Logs tokenization completion

---

## Dependencies

### Called Paragraphs
- **FORMAT-LISP-PROCEDURE**: Prepares string for tokenization

### Data Dependencies
- **WS-IN-LISP-RECORD**: Must contain valid LISP content
- **LS-SYMBOL array**: Must have 100-element capacity
- Assumes formatting procedure has been successful

---

## Error Handling
- **No explicit error checking**
- **Array bounds**: Hard limit of 100 tokens (excess silently ignored)
- **Empty string**: Results in LS-SYMBOL-TABLE-SIZE = 0
- **No overflow notification**: Calling program must check if 100 tokens reached

---

## Business Rules

### Tokenization Logic
1. **Space-delimited**: Uses space as universal delimiter
2. **Multiple spaces collapsed**: `ALL ' '` treats consecutive spaces as single delimiter
3. **Parentheses as tokens**: `(` and `)` become individual tokens (due to formatting)
4. **Atoms preserved**: Numbers, symbols, strings remain intact

### Token Types Produced
- **Parentheses**: `(` and `)`
- **Operators**: `+`, `-`, `*`, `/`, etc.
- **Keywords**: `defun`, `let`, `lambda`, etc.
- **Atoms**: Variable names, function names
- **Literals**: Numbers (integers, floats)
- **Strings**: Quoted strings (as single token if no internal spaces)

### Array Management
- **Fixed size**: Always processes exactly 100 slots
- **Actual size**: LS-SYMBOL-TABLE-SIZE indicates valid tokens
- **Remaining slots**: Filled with SPACES (not counted)

---

## Performance Considerations
- **UNSTRING efficiency**: Single-pass string parsing (fast)
- **Loop iterations**: Maximum 100 (fixed)
- **STRING operation in FORMAT-LISP-PROCEDURE**: Most expensive operation
- **Early termination**: WS-FLAG allows stopping before 100 iterations

---

## Example Execution

### Input (after formatting):
```
WS-IN-LISP-RECORD = "( defun factorial ( n ) ( if ( = n 0 ) 1 ( * n ( factorial ( - n 1 ) ) ) ) )"
```

### Tokenization Process:

| Iteration | Token Extracted | STRING-PTR | LS-SYMBOL-TABLE-SIZE | Notes |
|-----------|----------------|------------|----------------------|-------|
| 1 | `(` | 3 | 1 | Opening paren |
| 2 | `defun` | 9 | 2 | Function keyword |
| 3 | `factorial` | 19 | 3 | Function name |
| 4 | `(` | 21 | 4 | Parameter list start |
| 5 | `n` | 23 | 5 | Parameter name |
| 6 | `)` | 25 | 6 | Parameter list end |
| ... | ... | ... | ... | ... |
| 29 | `)` | 90 | 29 | Final closing paren |
| 30 | SPACES | 90 | 29 | No more tokens, loop exits |

### Final Output:
```
LS-SYMBOL(1) = "("
LS-SYMBOL(2) = "defun"
LS-SYMBOL(3) = "factorial"
LS-SYMBOL(4) = "("
LS-SYMBOL(5) = "n"
...
LS-SYMBOL(29) = ")"
LS-SYMBOL(30..100) = SPACES
LS-SYMBOL-TABLE-SIZE = 29
```

---

## Debug Output
```cobol
D     DISPLAY "After FORMAT-LISP-PROCEDURE"
D     DISPLAY "TOKENIZE-LISP-PROCEDURE:" WS-IN-LISP-RECORD
```
- Debug displays (D directive) show formatted string before tokenization
- Useful for verifying FORMAT-LISP-PROCEDURE output

---

## Maintenance Notes

### Limitations
1. **100-token limit**: Complex LISP expressions may exceed capacity
2. **No error reporting**: Silent truncation if >100 tokens
3. **String handling**: Quoted strings with spaces will be split (incorrect)
4. **No nested string support**: Doesn't handle escaped quotes

### Potential Enhancements
1. Add overflow detection and error reporting
2. Implement dynamic array sizing
3. Handle quoted strings as single tokens
4. Support escaped characters within strings
5. Report tokenization statistics (token count, types, etc.)

### Known Issues
- **LS-SYMBOL-LENGTH parameter**: Not set by this procedure (should be = LS-SYMBOL-TABLE-SIZE)
- **String literals**: Incorrectly split if contain spaces (e.g., `"hello world"` → 2 tokens)

---

*This documentation is AI-generated and should be reviewed by COBOL experts for accuracy.*
