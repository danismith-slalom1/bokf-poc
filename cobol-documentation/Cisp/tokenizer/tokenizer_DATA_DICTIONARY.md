# TOKENIZER Program - Data Dictionary

## Program Information
- **Program Name**: TOKENIZER
- **Author**: lauryn brown
- **Purpose**: Tokenize LISP input file
- **Repository**: Cisp
- **Generated**: 2026-01-20

---

## WORKING-STORAGE SECTION Variables

### File Handling Variables

#### WS-LISP-NAME
- **Level**: 01
- **Picture**: X(100)
- **Purpose**: Stores the dynamically assigned LISP file name passed from calling program
- **Initial Value**: None
- **Usage**: 
  - SET in: FILE-HANDLING-PROCEDURE (from LS-LISP-FILE-NAME)
  - READ by: SELECT statement (DYNAMIC assignment)
- **Business Logic**: Receives the LISP source file name to be tokenized

#### WS-IN-LISP-RECORD
- **Level**: 01
- **Picture**: X(200)
- **Purpose**: Working storage buffer for accumulating LISP file contents (multi-line concatenation)
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: FILE-HANDLING-PROCEDURE, APPEND-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ by: TOKENIZE-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE, CALC-LISP-LENGTH
- **Business Logic**: Accumulates all non-comment lines from the LISP file into a single string for parsing

#### WS-LISP-EOF
- **Level**: 01
- **Picture**: X
- **Purpose**: End-of-file indicator flag for LISP file reading
- **Initial Value**: None (implicitly spaces)
- **Usage**:
  - SET to "Y" in: FILE-HANDLING-PROCEDURE (AT END clause)
  - READ in: FILE-HANDLING-PROCEDURE (loop condition)
- **Business Logic**: Controls the file reading loop; set to "Y" when EOF is reached

### Length and Sizing Variables

#### WS-MAX-LISP-LENGTH
- **Level**: 78 (Constant)
- **Value**: 200
- **Purpose**: Maximum length constant for LISP record size
- **Usage**: READ in: CALC-LISP-LENGTH (loop boundary)
- **Business Logic**: Defines the maximum buffer size for processing LISP records

#### WS-LISP-LENGTH
- **Level**: 01
- **Picture**: 9(10)
- **Purpose**: Calculated actual length of the LISP content (excluding trailing spaces)
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: CALC-LISP-LENGTH, APPEND-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE, FORMAT-ADD-LEFT-SPACE, FORMAT-ADD-RIGHT-SPACE, FORMAT-ADD-BOTH-SPACES
  - READ by: Multiple formatting and parsing procedures
- **Business Logic**: Tracks the true length of LISP content as it's processed and spaces are added

#### WS-CALC-LENGTH-STR
- **Level**: 01
- **Picture**: X(200)
- **Purpose**: Temporary buffer for length calculation operations
- **Initial Value**: None
- **Usage**:
  - SET in: APPEND-LISP-PROCEDURE, FILE-HANDLING-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ in: CALC-LISP-LENGTH
- **Business Logic**: Working copy of input string used during length calculations to avoid modifying original data

### Comment Detection Variables

#### WS-IS-COMMENT
- **Level**: 01
- **Picture**: X
- **Purpose**: Flag indicating if current line is a LISP comment
- **Initial Value**: None
- **Condition Names**:
  - WS-IS-COMMENT-YES: VALUE "Y", FALSE 'N'
- **Usage**:
  - SET in: CALC-LISP-LENGTH (when ";" detected)
  - READ in: APPEND-LISP-PROCEDURE, FILE-HANDLING-PROCEDURE
- **Business Logic**: LISP comments start with ";"; this flag ensures comment lines are excluded from tokenization

### Formatting Control Variables (WS-FORMAT-LISP Group)

#### WS-NUM-LENGTH-ADD
- **Level**: 02
- **Picture**: 9(10)
- **Purpose**: Counter for consecutive spaces during length calculation
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: CALC-LISP-LENGTH
  - READ in: CALC-LISP-LENGTH
- **Business Logic**: Accumulates space counts to properly calculate string length with embedded spaces

#### WS-PAREN-RIGHT
- **Level**: 02
- **Picture**: X
- **Purpose**: Flag indicating whether space is needed on right side of parenthesis
- **Initial Value**: None
- **Condition Names**:
  - WS-PAREN-RIGHT-YES: VALUE "Y", FALSE "N"
- **Usage**:
  - SET in: FORMAT-CHECK-PAREN-PROCEDURE
  - READ in: FORMAT-PAREN-SPACE-PROCEDURE
- **Business Logic**: Used to determine if right space should be added around parentheses for proper tokenization

#### WS-PAREN-LEFT
- **Level**: 02
- **Picture**: X
- **Purpose**: Flag indicating whether space is needed on left side of parenthesis
- **Initial Value**: None
- **Condition Names**:
  - WS-PAREN-LEFT-YES: VALUE "Y", FALSE "N"
- **Usage**:
  - SET in: FORMAT-CHECK-PAREN-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ in: FORMAT-PAREN-SPACE-PROCEDURE
- **Business Logic**: Used to determine if left space should be added around parentheses for proper tokenization

#### WS-PAREN-TEMP-STR
- **Level**: 02
- **Picture**: X(2000)
- **Purpose**: Temporary buffer for string manipulation during parenthesis spacing
- **Initial Value**: None
- **Usage**:
  - SET in: FORMAT-LISP-PROCEDURE, FORMAT-PAREN-SPACE-PROCEDURE, FORMAT-ADD-LEFT-SPACE, FORMAT-ADD-RIGHT-SPACE, FORMAT-ADD-BOTH-SPACES
  - READ in: All FORMAT-ADD-* procedures
- **Business Logic**: Holds intermediate string states while adding spaces around parentheses

#### WS-PAREN-TEMP-NUM
- **Level**: 02
- **Picture**: 9(10)
- **Purpose**: Temporary numeric index for parenthesis formatting calculations
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: FORMAT-ADD-LEFT-SPACE, FORMAT-ADD-RIGHT-SPACE, FORMAT-ADD-BOTH-SPACES
  - READ in: All FORMAT-ADD-* procedures
- **Business Logic**: Calculates position indices for STRING operations during formatting

#### WS-WHICH-PAREN
- **Level**: 02
- **Picture**: X
- **Purpose**: Stores the specific parenthesis character ("(" or ")") being processed
- **Initial Value**: None
- **Usage**:
  - SET in: FORMAT-ADD-BOTH-SPACES
  - READ in: FORMAT-ADD-BOTH-SPACES (STRING operation)
- **Business Logic**: Preserves the parenthesis character when adding spaces on both sides

### Loop and Index Variables

#### WS-FORMAT-STR-INDEX
- **Level**: 01
- **Picture**: 9(10)
- **Purpose**: Primary index for character-by-character traversal in formatting procedures
- **Initial Value**: None (set to 1 or 3 in FORMAT-LISP-PROCEDURE)
- **Usage**:
  - MODIFIED in: FORMAT-LISP-PROCEDURE, FORMAT-CHECK-PAREN-PROCEDURE, FORMAT-ADD-* procedures, CALC-LISP-LENGTH
  - READ by: All formatting procedures
- **Business Logic**: Tracks current position while iterating through LISP string during formatting

#### WS-COUNT
- **Level**: 01
- **Picture**: 9(10)
- **Purpose**: General-purpose loop counter for various iterations
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: CAL-LENGTH-ALL-SYMBOLS, CALC-LENGTH-SYMBOL, TOKENIZE-LISP-PROCEDURE, PRINT-SYMBOL-TABLE
  - READ by: Same procedures (loop control)
- **Business Logic**: Used in PERFORM VARYING loops for symbol processing and table iteration

#### STRING-PTR
- **Level**: 01
- **Picture**: 9(10)
- **Purpose**: Pointer for UNSTRING operation tracking position in source string
- **Initial Value**: 1 (set in TOKENIZE-LISP-PROCEDURE)
- **Usage**:
  - SET in: TOKENIZE-LISP-PROCEDURE
  - MODIFIED by: UNSTRING statement (automatic increment)
- **Business Logic**: COBOL UNSTRING uses this to track parsing position through the input string

#### WS-TEMP-NUM
- **Level**: 01
- **Picture**: 9(10)
- **Purpose**: Temporary numeric variable for length tracking during file reading
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: APPEND-LISP-PROCEDURE, FILE-HANDLING-PROCEDURE
  - READ in: APPEND-LISP-PROCEDURE
- **Business Logic**: Tracks accumulated length of concatenated LISP lines during file reading

### General Purpose Flags

#### WS-FLAG
- **Level**: 01
- **Picture**: A(1)
- **Purpose**: General flag for loop termination in tokenization
- **Initial Value**: None
- **Condition Names**:
  - WS-FLAG-YES: VALUE 'Y', FALSE 'N'
- **Usage**:
  - SET in: TOKENIZE-LISP-PROCEDURE (when SPACES detected in symbol)
  - READ in: TOKENIZE-LISP-PROCEDURE (loop condition)
- **Business Logic**: Signals end of valid symbols during UNSTRING parsing

### Symbol Parsing Flags (WS-SYMBOL-FLAGS Group)

#### WS-OPEN-PAREN
- **Level**: 02
- **Picture**: X
- **Purpose**: Flag for detecting open parenthesis during parsing
- **Initial Value**: None
- **Condition Names**:
  - WS-OPEN-PAREN-YES: VALUE 'Y', FALSE 'N'
- **Usage**:
  - SET in: RESET-PARSE-FLAGS-PROCEDURE
- **Business Logic**: Used for parenthesis matching during expression parsing (currently set but not actively used in main flow)

#### WS-CLOSE-PAREN
- **Level**: 02
- **Picture**: X
- **Purpose**: Flag for detecting close parenthesis during parsing
- **Initial Value**: None
- **Condition Names**:
  - WS-CLOSE-PAREN-YES: VALUE 'Y', FALSE 'N'
- **Usage**:
  - SET in: RESET-PARSE-FLAGS-PROCEDURE
- **Business Logic**: Used for parenthesis matching during expression parsing (currently set but not actively used in main flow)

### Parse String Control Group (WS-PARSE-STR)

#### WS-PARSE-STR-INDEX
- **Level**: 02
- **Picture**: 9(5)
- **Purpose**: Index for character-by-character parsing within individual symbols
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: CALC-LENGTH-SYMBOL
  - READ in: CALC-LENGTH-SYMBOL
- **Business Logic**: Tracks position while calculating individual symbol lengths

#### WS-PARSE-STR-END
- **Level**: 02
- **Picture**: X
- **Purpose**: Flag indicating parsing has reached end of current symbol
- **Initial Value**: None
- **Condition Names**:
  - WS-PARSE-HAS-ENDED: VALUE 'Y', FALSE 'N'
- **Usage**:
  - SET in: CALC-LENGTH-SYMBOL (when space detected), CAL-LENGTH-ALL-SYMBOLS (initialization)
  - READ in: CALC-LENGTH-SYMBOL (loop condition)
- **Business Logic**: Controls symbol length calculation loop termination

#### WS-PARSE-STR-CHAR
- **Level**: 02
- **Picture**: X
- **Purpose**: Single character buffer for parse operations
- **Initial Value**: None
- **Usage**: Currently defined but not actively used in the program
- **Business Logic**: Reserved for potential character-by-character parsing operations

#### WS-PARSE-EXPRESSION-START
- **Level**: 02
- **Picture**: 9(5)
- **Purpose**: Starting position of current expression being parsed
- **Initial Value**: None
- **Usage**:
  - SET in: RESET-PARSE-FLAGS-PROCEDURE
  - READ in: PRINT-PARSE-FLAGS-PROCEDURE
- **Business Logic**: Marks beginning of parsed expression (currently reset but not actively used)

#### WS-PARSE-EXPRESSION-END
- **Level**: 02
- **Picture**: 9(5)
- **Purpose**: Ending position of current expression being parsed
- **Initial Value**: None
- **Usage**:
  - SET in: RESET-PARSE-FLAGS-PROCEDURE
  - READ in: PRINT-PARSE-FLAGS-PROCEDURE
- **Business Logic**: Marks end of parsed expression (currently reset but not actively used)

#### WS-PARSE-EXPRESSION-LEN
- **Level**: 02
- **Picture**: 9(5)
- **Purpose**: Length of current parsed expression/symbol
- **Initial Value**: None
- **Usage**:
  - MODIFIED in: CALC-LENGTH-SYMBOL, CAL-LENGTH-ALL-SYMBOLS
  - READ in: CALC-LENGTH-SYMBOL, CAL-LENGTH-ALL-SYMBOLS
- **Business Logic**: Stores calculated length of each symbol (excluding trailing spaces)

### Logging Variables (WS Shared with LOGGER SubRoutine)

#### WS-LOG-OPERATION-FLAG
- **Level**: 01
- **Picture**: X(5)
- **Purpose**: Operation type flag for LOGGER subroutine calls
- **Initial Value**: None
- **Usage**:
  - SET to "ADD" in: FILE-HANDLING-PROCEDURE, TOKENIZE-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ by: LOGGER program (external call)
- **Business Logic**: Instructs LOGGER program on what operation to perform

#### WS-LOG-RECORD-FUNCTION-NAME
- **Level**: 02 (within WS-LOG-RECORD)
- **Picture**: X(40)
- **Purpose**: Name of function/procedure being logged
- **Initial Value**: None
- **Usage**:
  - SET in: FILE-HANDLING-PROCEDURE, TOKENIZE-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ by: LOGGER program (external call)
- **Business Logic**: Identifies which procedure is being logged for audit trail

#### WS-LOG-RECORD-MESSAGE
- **Level**: 02 (within WS-LOG-RECORD)
- **Picture**: X(100)
- **Purpose**: Descriptive message for log entry
- **Initial Value**: None
- **Usage**:
  - SET in: FILE-HANDLING-PROCEDURE, TOKENIZE-LISP-PROCEDURE, FORMAT-LISP-PROCEDURE
  - READ by: LOGGER program (external call)
- **Business Logic**: Provides human-readable description of completed operation

---

## LINKAGE SECTION Variables

### LS-LISP-FILE-NAME
- **Level**: 01
- **Picture**: X(100)
- **Purpose**: Input parameter - LISP source file name from calling program
- **Direction**: INPUT
- **Usage**: Received from calling program (CISP), copied to WS-LISP-NAME
- **Business Logic**: Specifies which LISP file to tokenize

### LS-SYMBOL-LENGTH
- **Level**: 01
- **Picture**: 9(4)
- **Purpose**: Output parameter - total count of symbols tokenized
- **Direction**: OUTPUT (not explicitly set in current code)
- **Usage**: Intended to return symbol count to calling program
- **Business Logic**: Should contain total number of tokens parsed (currently not populated)

### LS-LISP-SYMBOLS Group

#### LS-SYMBOL-TABLE-SIZE
- **Level**: 02
- **Picture**: 9(4)
- **Purpose**: Output parameter - number of symbols stored in table
- **Direction**: OUTPUT
- **Usage**:
  - MODIFIED in: TOKENIZE-LISP-PROCEDURE (incremented for each valid symbol)
  - READ in: PRINT-SYMBOL-TABLE (loop boundary)
- **Business Logic**: Contains actual count of tokenized symbols (max 100)

#### LS-SYMBOL
- **Level**: 02
- **Picture**: X(50) OCCURS 100 TIMES
- **Purpose**: Output parameter array - tokenized LISP symbols
- **Direction**: OUTPUT
- **Usage**:
  - MODIFIED in: TOKENIZE-LISP-PROCEDURE (via UNSTRING), CALC-LENGTH-SYMBOL (character inspection)
  - READ by: Calling program (CISP)
- **Business Logic**: Stores each tokenized LISP element (atoms, operators, parentheses)

#### LS-SYMBOL-LEN
- **Level**: 02
- **Picture**: 9(2) OCCURS 100 TIMES
- **Purpose**: Output parameter array - length of each symbol (excluding trailing spaces)
- **Direction**: OUTPUT
- **Usage**:
  - MODIFIED in: CAL-LENGTH-ALL-SYMBOLS
  - READ by: Calling program (CISP), PRINT-SYMBOL-TABLE (display)
- **Business Logic**: Provides actual length of each symbol for precise substring operations

---

## FILE SECTION Variables

### IN-LISP-RECORD
- **Level**: 01
- **Picture**: X(200)
- **FD**: LISP-FILE
- **Purpose**: Record buffer for reading LISP file line by line
- **Usage**:
  - READ by: FILE-HANDLING-PROCEDURE, APPEND-LISP-PROCEDURE
  - MOVED to: WS-CALC-LENGTH-STR, WS-IN-LISP-RECORD
- **Business Logic**: Input buffer for each line read from LISP source file

---

## Data Relationships and Flow

### File Reading Flow
1. **LS-LISP-FILE-NAME** → **WS-LISP-NAME** → File opened dynamically
2. **IN-LISP-RECORD** → **WS-CALC-LENGTH-STR** → Length calculated
3. **IN-LISP-RECORD** → **WS-IN-LISP-RECORD** → Accumulated (non-comments only)

### Formatting Flow
1. **WS-IN-LISP-RECORD** → **WS-CALC-LENGTH-STR** → **WS-LISP-LENGTH** calculated
2. **WS-IN-LISP-RECORD** → **WS-PAREN-TEMP-STR** → Modified with spaces → **WS-IN-LISP-RECORD**

### Tokenization Flow
1. **WS-IN-LISP-RECORD** → UNSTRING operation → **LS-SYMBOL** array
2. **LS-SYMBOL** array → Character inspection → **LS-SYMBOL-LEN** array

### Logging Flow
1. Procedure name → **WS-LOG-RECORD-FUNCTION-NAME**
2. Message → **WS-LOG-RECORD-MESSAGE**
3. "ADD" → **WS-LOG-OPERATION-FLAG**
4. Call LOGGER with WS-LOG-OPERATION-FLAG and WS-LOG-RECORD

---

## Special Handling Notes

### REDEFINES
- No REDEFINES clauses used in this program

### OCCURS Clauses
- **LS-SYMBOL**: OCCURS 100 TIMES - Fixed array for token storage
- **LS-SYMBOL-LEN**: OCCURS 100 TIMES - Parallel array for token lengths

### COMP Fields
- No COMP or COMP-3 fields used (all display format)

### Condition Names (88-level)
- **WS-IS-COMMENT-YES**: Simplifies comment detection logic
- **WS-PAREN-RIGHT-YES/FALSE**: Boolean flag for right parenthesis spacing
- **WS-PAREN-LEFT-YES/FALSE**: Boolean flag for left parenthesis spacing
- **WS-FLAG-YES/FALSE**: Boolean flag for tokenization loop control
- **WS-OPEN-PAREN-YES/FALSE**: Reserved for expression parsing
- **WS-CLOSE-PAREN-YES/FALSE**: Reserved for expression parsing
- **WS-PARSE-HAS-ENDED**: Controls symbol length calculation loop

---

## Technical Debt and Improvement Opportunities

1. **LS-SYMBOL-LENGTH not populated**: Output parameter defined but never set
2. **Parse flags underutilized**: WS-OPEN-PAREN, WS-CLOSE-PAREN, WS-PARSE-STR-CHAR defined but not used in main logic flow
3. **Magic number**: Hard-coded array size of 100 - should match between caller and callee
4. **Fixed buffer sizes**: 200-byte limit on LISP line length may be restrictive for complex expressions
5. **Comment handling**: Only single-line comments (";") supported; multi-line comment blocks not handled

---

## Cross-Reference Summary

### Most Frequently Modified Variables
1. **WS-FORMAT-STR-INDEX**: Modified in 8 procedures (primary iteration variable)
2. **WS-LISP-LENGTH**: Modified in 6 procedures (length tracking through processing)
3. **WS-IN-LISP-RECORD**: Modified in 4 procedures (central data buffer)
4. **WS-COUNT**: Modified in 4 procedures (general loop counter)

### Critical Control Flags
1. **WS-LISP-EOF**: Controls file reading loop termination
2. **WS-IS-COMMENT**: Filters out comment lines from processing
3. **WS-FLAG**: Controls tokenization loop termination
4. **WS-PARSE-HAS-ENDED**: Controls symbol length calculation

---

*This data dictionary is AI-generated and should be reviewed by COBOL experts for accuracy. Last updated: 2026-01-20*
