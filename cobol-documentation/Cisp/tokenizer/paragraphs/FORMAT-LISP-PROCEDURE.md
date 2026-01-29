# FORMAT-LISP-PROCEDURE

## Overview
**Program**: TOKENIZER  
**Location**: Lines 180-213  
**Purpose**: Add spaces around parentheses to enable clean space-delimited tokenization

---

## Business Purpose
Preprocesses LISP string by inserting spaces around all parentheses. This formatting step ensures that parentheses become separate tokens when the string is split by spaces, allowing UNSTRING to correctly tokenize LISP s-expressions.

---

## Input Requirements
- **WS-IN-LISP-RECORD**: Raw concatenated LISP content
- String should contain valid LISP syntax with parentheses

---

## Processing Steps

### 1. Calculate Initial Length
```cobol
MOVE WS-IN-LISP-RECORD TO WS-CALC-LENGTH-STR
PERFORM CALC-LISP-LENGTH
```
- Copies input to calculation buffer
- Determines actual string length (WS-LISP-LENGTH)
- Establishes loop boundary

### 2. Handle Special Case - Leading Parenthesis
```cobol
MOVE 1 TO WS-FORMAT-STR-INDEX
IF WS-IN-LISP-RECORD(1:1)="(" AND NOT WS-IN-LISP-RECORD(2:1) EQUAL " " THEN
    MOVE WS-IN-LISP-RECORD TO WS-PAREN-TEMP-STR
    STRING "( " DELIMITED BY SIZE
        WS-PAREN-TEMP-STR(2:WS-LISP-LENGTH) DELIMITED BY SIZE 
        INTO WS-IN-LISP-RECORD
    ADD 3 TO WS-FORMAT-STR-INDEX
    ADD 1 TO WS-LISP-LENGTH
END-IF
```
- **Condition**: First character is `(` with no following space
- **Action**: Rebuilds string with space after opening paren
  - `(foo` → `( foo`
- **Index adjustment**: Starts main loop at position 3 (skips processed chars)
- **Length adjustment**: Increments by 1 for added space

### 3. Main Formatting Loop
```cobol
PERFORM VARYING WS-FORMAT-STR-INDEX FROM WS-FORMAT-STR-INDEX BY 1 
    UNTIL WS-FORMAT-STR-INDEX > WS-LISP-LENGTH
    SET WS-PAREN-LEFT-YES TO FALSE
    SET WS-PAREN-RIGHT-YES TO FALSE
    MOVE WS-IN-LISP-RECORD TO WS-PAREN-TEMP-STR
    
    EVALUATE WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)
        WHEN "("
            PERFORM FORMAT-PAREN-SPACE-PROCEDURE
        WHEN ")"
            PERFORM FORMAT-PAREN-SPACE-PROCEDURE
    END-EVALUATE
END-PERFORM
```
- **Loop boundary**: Process entire string (dynamically extends as spaces added)
- **Flag reset**: Clear left/right paren flags each iteration
- **Buffer backup**: Copy current state to temp buffer before modification
- **Character evaluation**:
  - **"(" detected**: Call spacing procedure
  - **")" detected**: Call spacing procedure
  - **Other chars**: No action, continue
- **Note**: LENGTH increases during loop as spaces inserted

### 4. Audit Logging
```cobol
MOVE "ADD" TO WS-LOG-OPERATION-FLAG
MOVE "TOKENIZER:FORMAT-LISP-PROCEDURE" TO WS-LOG-RECORD-FUNCTION-NAME
MOVE "COMPLETED formatting lisp string for parsing" TO WS-LOG-RECORD-MESSAGE
CALL 'LOGGER' USING WS-LOG-OPERATION-FLAG, WS-LOG-RECORD
```
- Logs successful formatting completion

---

## Output and Side Effects

### Modified Variables
- **WS-IN-LISP-RECORD**: Transformed with spaces around all parentheses
- **WS-LISP-LENGTH**: Updated to reflect new string length (after space insertion)
- **WS-FORMAT-STR-INDEX**: Final loop counter position
- **WS-PAREN-TEMP-STR**: Contains various intermediate string states
- **WS-PAREN-LEFT**: Flag state after last paren processed
- **WS-PAREN-RIGHT**: Flag state after last paren processed

### External Calls
- **CALL 'LOGGER'**: Logs formatting completion

---

## Dependencies

### Called Paragraphs
- **CALC-LISP-LENGTH**: Calculate string length
- **FORMAT-PAREN-SPACE-PROCEDURE**: Determine and add appropriate spacing
  - **FORMAT-CHECK-PAREN-PROCEDURE**: Check surrounding characters
  - **FORMAT-ADD-BOTH-SPACES**: Add space on both sides
  - **FORMAT-ADD-RIGHT-SPACE**: Add space on right side only
  - **FORMAT-ADD-LEFT-SPACE**: Add space on left side only

### Data Dependencies
- **WS-IN-LISP-RECORD**: Input LISP string
- **WS-LISP-LENGTH**: Length boundary for loop
- **WS-PAREN-TEMP-STR**: 2000-byte buffer (must be larger than WS-IN-LISP-RECORD)

---

## Error Handling
- **Buffer overflow risk**: If formatted string exceeds 200 bytes (WS-IN-LISP-RECORD size)
  - 2000-byte WS-PAREN-TEMP-STR provides safety margin
  - No explicit overflow detection
- **No validation**: Assumes syntactically correct LISP input

---

## Business Rules

### Spacing Logic
1. **Check both sides** of each parenthesis
2. **Add space(s)** where non-space character found:
   - Left side needs space: `x(` → `x (`
   - Right side needs space: `)x` → `) x`
   - Both sides need space: `x(y` → `x ( y`
   - Neither side needs space: ` ( ` → no change

### Special Cases
1. **Leading parenthesis**: Handled before main loop (optimization)
2. **Consecutive parentheses**: `((` → `( (`, `))` → `) )`
3. **Already spaced**: No redundant spaces added

---

## Algorithm Details

### Dynamic Length Tracking
- **Challenge**: String length increases as spaces added
- **Solution**: Loop boundary (WS-LISP-LENGTH) updated by sub-procedures
- **Effect**: Loop continues through newly inserted characters

### Example State Changes

**Input**: `(foo(bar))`

| Index | Character | Action | Result | New Length |
|-------|-----------|--------|--------|------------|
| Start | - | - | `(foo(bar))` | 11 |
| 1 | `(` | Special case | `( foo(bar))` | 12 |
| 3-6 | `f`,`o`,`o` | No action | `( foo(bar))` | 12 |
| 7 | `(` | Add both | `( foo ( bar))` | 13 |
| 12 | `)` | Add left | `( foo ( bar ))` | 14 |
| 14 | `)` | Add left | `( foo ( bar ) )` | 15 |
| End | - | - | `( foo ( bar ) )` | 15 |

**Final Output**: `( foo ( bar ) )`

---

## Performance Considerations

### Time Complexity
- **Best case**: O(n) - no parentheses to format
- **Typical case**: O(n × k) where k = average parentheses per position
- **Worst case**: O(n²) - every character is parenthesis (length doubles, loop extends)

### Space Complexity
- **2000-byte temporary buffer**: Significant memory overhead
- **Multiple STRING operations**: Expensive COBOL operations

### Optimization Opportunities
1. **Two-pass approach**: Calculate required size first, format once
2. **Pre-allocate buffer**: Based on parenthesis count
3. **String builder pattern**: Avoid repeated STRING operations

---

## Debug Output
```cobol
D    DISPLAY "FORMAT-LISP-PROCEDURE:" WS-IN-LISP-RECORD
D    DISPLAY WS-IN-LISP-RECORD(WS-FORMAT-STR-INDEX:1)
D    " left:" WS-PAREN-RIGHT " right:" WS-PAREN-LEFT
```
- **First display**: Shows input before formatting
- **Loop displays**: Shows each character and spacing flags
- **Useful for**: Debugging spacing logic issues

---

## Maintenance Notes

### Known Limitations
1. **Buffer size**: 200-byte limit may be insufficient for complex expressions
2. **No validation**: Doesn't verify balanced parentheses
3. **String literals**: May incorrectly add spaces inside quoted strings
4. **Character sensitivity**: Only handles `(` and `)`, not brackets or braces

### Potential Enhancements
1. Add overflow detection and error reporting
2. Handle quoted strings (don't format inside strings)
3. Support other LISP delimiters (brackets, braces)
4. Validate parenthesis balance
5. Optimize with single-pass algorithm

### Testing Recommendations
- Test with nested expressions: `((a b) (c d))`
- Test with mixed spacing: `(a( b )c)`
- Test edge cases: Empty string, single paren, deeply nested
- Test buffer limits: Expressions near 200 bytes

---

*This documentation is AI-generated and should be reviewed by COBOL experts for accuracy.*
