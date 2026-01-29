# LOGGER Program Documentation Index

**Last Updated**: January 20, 2026  
**Repository**: lauryndbrown/Cisp  
**Program File**: logger.cbl  
**Program Size**: 64 lines (Small program - documented as single unit)

## Program Overview

- **Program ID**: LOGGER
- **Author**: Lauryn Brown
- **Purpose**: Log activity done by other programs
- **Type**: Utility/Service Program (called via LINKAGE)
- **Compiler**: GnuCOBOL (cobc)

## Program Structure Analysis

### IDENTIFICATION DIVISION (Lines 8-9)
- Standard program identification
- Program name: LOGGER

### ENVIRONMENT DIVISION (Lines 10-15)
- **INPUT-OUTPUT SECTION**: Dynamic file assignment
- **FILE-CONTROL**: Sequential log file with OPTIONAL SELECT and DYNAMIC assignment

### DATA DIVISION (Lines 16-28)
- **FILE SECTION** (Lines 17-21): LOG-RECORD structure with ID, function name, and message
- **WORKING-STORAGE SECTION** (Lines 22-23): File name storage
- **LINKAGE SECTION** (Lines 24-28): Operation flag and log record parameters

### PROCEDURE DIVISION (Lines 29-64)
- **MAIN-PROCEDURE** (Lines 30-39): Evaluates operation flag and dispatches to appropriate handler
- **LOG-INIT-PROCEDURE** (Lines 40-47): Opens log file and writes initial record
- **LOG-WRITE-TO-PROCEDURE** (Lines 48-52): Appends log entries
- **LOG-FLAG-ERROR-PROCEDURE** (Lines 53-54): Handles invalid operation flags
- **LOG-CLOSE-PROCEDURE** (Lines 55-62): Writes closing record and closes file

## Chunking Strategy

**Strategy**: Document as single unit (program is <100 lines and logically cohesive)

This small utility program will be documented in its entirety:
- Data Dictionary for all data structures
- Individual paragraph documentation
- Call graph showing dispatch pattern
- Variable mutation analysis (particularly LOG-RECORD-ID counter)

## Dependencies

### External Files
- **Log File**: `..\logs\log.data` (relative path, Windows-style)
- **File Organization**: LINE SEQUENTIAL
- **Access Mode**: OUTPUT (initialization), then sequential writes

### Called By
- External programs via LINKAGE SECTION
- Receives operation flag and log record structure

### Calls To
- None (leaf-level utility program)

## Static Analysis Summary

### Key Variables
- **WS-LOG-FILE-NAME**: Dynamic file name for log file
- **LOG-RECORD-ID**: Sequential counter (initialized to 1, incremented on each write)
- **LS-LOG-OPERATION-FLAG**: Controls program behavior (OPEN/CLOSE/ADD/OTHER)

### File Operations
- **OPEN OUTPUT LOG-FILE**: Opens/creates log file (LOG-INIT-PROCEDURE)
- **WRITE LOG-RECORD**: Appends records to log (all procedures except error handler)
- **CLOSE LOG-FILE**: Closes log file (LOG-CLOSE-PROCEDURE)

### Control Flow
- EVALUATE-based dispatch pattern in MAIN-PROCEDURE
- Four handler paragraphs based on operation flag
- Sequential execution (GOBACK after dispatch)

## Documentation Artifacts

This index references the following documentation files:

1. **[logger_DATA_DICTIONARY.md](logger_DATA_DICTIONARY.md)** - All data structures and variables
2. **[logger_CALL_GRAPH.md](logger_CALL_GRAPH.md)** - PERFORM hierarchy and control flow
3. **[logger_VARIABLE_MUTATIONS.md](logger_VARIABLE_MUTATIONS.md)** - LOG-RECORD-ID mutation tracking
4. **[logger_ERROR_HANDLING.md](logger_ERROR_HANDLING.md)** - Error handling analysis and risks
5. **[logger_COMPREHENSIVE_DOC.md](logger_COMPREHENSIVE_DOC.md)** - Complete program documentation
6. **[logger_MERMAID_DIAGRAMS.md](logger_MERMAID_DIAGRAMS.md)** - Visual diagrams and flowcharts
7. **[paragraphs/](paragraphs/)** - Individual paragraph documentation

## Documentation Sequence

1. ✅ Program structure analysis (this document)
2. 🔄 Data dictionary generation
3. ⏳ Paragraph documentation
4. ⏳ Call graph creation
5. ⏳ Variable mutation analysis
6. ⏳ Error handling analysis
7. ⏳ Comprehensive documentation synthesis

## Notes

- **Small Program**: Can be analyzed in single context window
- **Simple Control Flow**: EVALUATE dispatch with no nested PERFORMs
- **Utility Purpose**: Designed to be called by other programs for centralized logging
- **Path Format**: Uses Windows-style relative path (..\logs\log.data) - may need adjustment for Unix systems
- **No Error Handling**: Note absence of FILE STATUS checking - potential risk area
