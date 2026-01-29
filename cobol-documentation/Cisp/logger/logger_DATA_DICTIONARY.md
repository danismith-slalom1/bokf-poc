# LOGGER Data Dictionary

**Last Updated**: January 20, 2026  
**Program**: LOGGER  
**Repository**: lauryndbrown/Cisp  
**Purpose**: Comprehensive data structure documentation for the LOGGER utility program

## Overview

This data dictionary documents all data structures used in the LOGGER program, including file records, working storage variables, and linkage section parameters. The program uses a simple data model optimized for sequential log file writing.

---

## FILE SECTION

### LOG-FILE

**File Description**:
- **Organization**: LINE SEQUENTIAL
- **Access Mode**: OUTPUT (for initialization), sequential writes thereafter
- **Assignment**: DYNAMIC (runtime assignment via WS-LOG-FILE-NAME)
- **Select Modifier**: OPTIONAL (file need not exist at program start)
- **File Path**: `..\logs\log.data` (Windows-style relative path)

### LOG-RECORD (FD Level)

**Record Structure**: Fixed-length record with three fields

#### LOG-RECORD-ID
- **Level**: 02
- **Picture**: PIC 9(10)
- **Type**: Numeric (unsigned integer)
- **Size**: 10 bytes
- **Initial Value**: Initialized to 1 in LOG-INIT-PROCEDURE
- **Purpose**: Sequential record counter for all log entries within a single program session
- **Usage Pattern**: 
  - **Initialized**: LOG-INIT-PROCEDURE (set to 1)
  - **Modified**: LOG-WRITE-TO-PROCEDURE (incremented by 1), LOG-CLOSE-PROCEDURE (incremented by 1)
  - **Read**: Implicitly read during WRITE operations
- **Related Variables**: Part of LOG-RECORD group
- **Special Handling**: 
  - Acts as monotonically increasing counter
  - Maximum value: 9,999,999,999 (10 digits)
  - No overflow protection implemented
  - Resets to 1 each time LOG-INIT is called
- **Business Context**: Provides sequential ordering of log entries for chronological analysis
- **Mutation Risk**: Modified in 3 locations (INIT, WRITE-TO, CLOSE) - see [logger_VARIABLE_MUTATIONS.md](logger_VARIABLE_MUTATIONS.md)

#### LOG-RECORD-FUNCTION-NAME
- **Level**: 02
- **Picture**: PIC X(40)
- **Type**: Alphanumeric
- **Size**: 40 bytes
- **Initial Value**: Space-filled by default
- **Purpose**: Identifies the calling program or function that generated the log entry
- **Usage Pattern**:
  - **Modified**: All logging procedures (INIT, WRITE-TO, CLOSE)
  - **Read**: Implicitly during WRITE operations
- **Related Variables**: 
  - Sourced from LS-LOG-RECORD-FUNCTION-NAME (LINKAGE) in LOG-WRITE-TO-PROCEDURE
  - Hard-coded in LOG-INIT-PROCEDURE and LOG-CLOSE-PROCEDURE
- **Special Handling**:
  - Fixed-width field (40 characters)
  - Shorter names will be space-padded on the right
  - Longer names will be truncated (potential data loss)
  - No validation of function name format
- **Business Context**: Enables filtering and analysis of log entries by source program/function
- **Examples**:
  - `"LOG-INIT-PROCEDURE"` (initialization)
  - `"LOGGER:LOG-CLOSE-PROCEDURE"` (closure)
  - User-supplied function names from calling programs

#### LOG-RECORD-MESSAGE
- **Level**: 02
- **Picture**: PIC X(100)
- **Type**: Alphanumeric
- **Size**: 100 bytes
- **Initial Value**: Space-filled by default
- **Purpose**: Free-form log message describing the logged event or activity
- **Usage Pattern**:
  - **Modified**: All logging procedures
  - **Read**: Implicitly during WRITE operations
- **Related Variables**:
  - Sourced from LS-LOG-RECORD-MESSAGE (LINKAGE) in LOG-WRITE-TO-PROCEDURE
  - Hard-coded in LOG-INIT-PROCEDURE and LOG-CLOSE-PROCEDURE
- **Special Handling**:
  - Fixed-width field (100 characters)
  - Messages longer than 100 characters will be truncated
  - No newline or special character handling
  - Space-padded for shorter messages
- **Business Context**: Contains human-readable description of logged events for debugging and audit trail
- **Examples**:
  - `"Starting Program!"` (initialization)
  - `"Closed logging file"` (closure)
  - User-supplied messages from calling programs

---

## WORKING-STORAGE SECTION

### WS-LOG-FILE-NAME
- **Level**: 01
- **Picture**: PIC X(20)
- **Type**: Alphanumeric
- **Size**: 20 bytes
- **Initial Value**: Not initialized (spaces by default)
- **Purpose**: Holds the dynamic file path for the log file used in SELECT ASSIGN TO DYNAMIC
- **Usage Pattern**:
  - **Modified**: LOG-INIT-PROCEDURE (set to `..\logs\log.data`)
  - **Read**: FILE-CONTROL section (dynamic assignment during OPEN)
- **Related Variables**: Referenced by LOG-FILE in FILE-CONTROL SELECT statement
- **Special Handling**:
  - **CRITICAL**: Maximum path length is 20 characters
  - Current hard-coded path `..\logs\log.data` is 17 characters (safe)
  - Longer paths would be truncated, causing file operation failures
  - Path format is Windows-style (backslashes) - may fail on Unix systems
  - No validation or error checking on path format
- **Business Context**: Enables flexible log file location (though currently hard-coded)
- **Potential Issues**:
  - Fixed at 20 bytes - insufficient for most absolute paths
  - Hard-coded Windows path syntax
  - No environment variable expansion
  - Relative path assumes specific working directory
- **Recommendations**:
  - Consider increasing size to PIC X(100) or X(255) for portability
  - Implement path validation
  - Support environment variable or configuration file for path

---

## LINKAGE SECTION

The LINKAGE SECTION defines parameters passed by calling programs. These variables do not consume storage in LOGGER but reference memory from the caller.

### LS-LOG-OPERATION-FLAG
- **Level**: 01
- **Picture**: PIC X(5)
- **Type**: Alphanumeric
- **Size**: 5 bytes
- **Purpose**: Command flag indicating which logging operation to perform
- **Usage Pattern**:
  - **Read**: MAIN-PROCEDURE (EVALUATE statement)
  - **Not Modified**: Input parameter only
- **Valid Values**:
  - `"OPEN"` - Initialize log file and write first entry
  - `"CLOSE"` - Write closing entry and close log file
  - `"ADD"` - Append new log entry
  - Any other value triggers LOG-FLAG-ERROR-PROCEDURE
- **Related Variables**: Controls dispatch to LOG-INIT, LOG-CLOSE, LOG-WRITE-TO, or LOG-FLAG-ERROR procedures
- **Special Handling**:
  - Case-sensitive comparison (must be uppercase)
  - Exact match required (e.g., "open" or " OPEN" would fail)
  - No trimming of leading/trailing spaces
  - 5-byte field allows for extension (currently uses 3-5 bytes)
- **Business Context**: Provides simple command interface for calling programs
- **Error Handling**: Invalid flags display "READ FLAG ERROR" but program continues
- **Recommendations**:
  - Document case-sensitivity requirement for calling programs
  - Consider adding more descriptive error message
  - Consider adding return code for error feedback to caller

### LS-LOG-RECORD
- **Level**: 01
- **Type**: Group item (record structure)
- **Size**: 140 bytes (40 + 100)
- **Purpose**: Container for log entry data passed from calling program
- **Usage Pattern**:
  - **Read**: LOG-WRITE-TO-PROCEDURE
  - **Not Modified**: Input parameter only
- **Related Variables**: Source data for LOG-RECORD-FUNCTION-NAME and LOG-RECORD-MESSAGE
- **Special Handling**: Group item with two subordinate fields
- **Business Context**: Standardized log entry format for all calling programs

#### LS-LOG-RECORD-FUNCTION-NAME
- **Level**: 02
- **Picture**: PIC X(40)
- **Type**: Alphanumeric
- **Size**: 40 bytes
- **Purpose**: Function or program name from calling code
- **Usage Pattern**:
  - **Read**: LOG-WRITE-TO-PROCEDURE
  - Moved to LOG-RECORD-FUNCTION-NAME
- **Related Variables**: Copied to LOG-RECORD-FUNCTION-NAME in file record
- **Special Handling**:
  - Must match LOG-RECORD-FUNCTION-NAME size (40 bytes)
  - Calling program responsible for proper formatting
  - No validation performed
- **Business Context**: Identifies source of log entry in calling code

#### LS-LOG-RECORD-MESSAGE
- **Level**: 02
- **Picture**: PIC X(100)
- **Type**: Alphanumeric
- **Size**: 100 bytes
- **Purpose**: Log message text from calling program
- **Usage Pattern**:
  - **Read**: LOG-WRITE-TO-PROCEDURE
  - Moved to LOG-RECORD-MESSAGE
- **Related Variables**: Copied to LOG-RECORD-MESSAGE in file record
- **Special Handling**:
  - Must match LOG-RECORD-MESSAGE size (100 bytes)
  - Calling program responsible for formatting
  - No validation or sanitization
  - Long messages truncated silently
- **Business Context**: Free-form descriptive text for log entry

---

## Data Structure Relationships

### File I/O Data Flow

```
Calling Program
    ↓ (LINKAGE)
LS-LOG-OPERATION-FLAG → MAIN-PROCEDURE → [EVALUATE dispatch]
    ↓
LS-LOG-RECORD-FUNCTION-NAME → LOG-RECORD-FUNCTION-NAME → LOG-FILE
LS-LOG-RECORD-MESSAGE → LOG-RECORD-MESSAGE → LOG-FILE
    ↓
LOG-RECORD-ID (auto-incremented) → LOG-FILE
```

### Record Counter Flow

```
LOG-INIT-PROCEDURE
    ↓
LOG-RECORD-ID = 1
    ↓
LOG-WRITE-TO-PROCEDURE (0+ times)
    ↓
ADD 1 TO LOG-RECORD-ID (incremented with each call)
    ↓
LOG-CLOSE-PROCEDURE
    ↓
ADD 1 TO LOG-RECORD-ID (final increment)
```

---

## Data Validation and Constraints

### Size Constraints
| Field | Max Size | Current Usage | Risk Level |
|-------|----------|---------------|------------|
| WS-LOG-FILE-NAME | 20 bytes | 17 bytes | **HIGH** - Insufficient for absolute paths |
| LOG-RECORD-FUNCTION-NAME | 40 bytes | Variable | **MEDIUM** - May truncate long function names |
| LOG-RECORD-MESSAGE | 100 bytes | Variable | **MEDIUM** - May truncate long messages |
| LOG-RECORD-ID | 10 digits | Variable | **LOW** - Allows 10 billion records |

### Missing Validations
- **No FILE STATUS checking**: File operations may fail silently
- **No path validation**: Invalid paths in WS-LOG-FILE-NAME not detected
- **No operation flag validation**: Only basic EVALUATE with catch-all OTHER
- **No message sanitization**: Special characters, newlines not handled
- **No overflow protection**: LOG-RECORD-ID could exceed 10 digits (unlikely)

---

## Expert Review Notes

**⚠️ AI-GENERATED DOCUMENTATION**: This data dictionary was generated by AI analysis and must be reviewed by COBOL experts.

**Areas Requiring Expert Validation**:
1. Confirm LOG-RECORD-ID maximum value and overflow behavior
2. Verify WS-LOG-FILE-NAME size adequacy for deployment environment
3. Validate that 40-byte function name is sufficient for actual usage
4. Confirm whether LINE SEQUENTIAL format meets requirements
5. Review whether OPTIONAL SELECT is appropriate for production use

**Known Limitations**:
- No static analysis cross-reference report was available
- Data usage patterns inferred from code inspection only
- Business logic significance based on code analysis, not domain knowledge

---

## Related Documentation

- **[logger_INDEX.md](logger_INDEX.md)** - Program structure overview
- **[logger_VARIABLE_MUTATIONS.md](logger_VARIABLE_MUTATIONS.md)** - Detailed LOG-RECORD-ID mutation analysis
- **[logger_ERROR_HANDLING.md](logger_ERROR_HANDLING.md)** - File I/O error handling analysis
- **[logger_COMPREHENSIVE_DOC.md](logger_COMPREHENSIVE_DOC.md)** - Complete program documentation
