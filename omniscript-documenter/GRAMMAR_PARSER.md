# OMNISCRIPT Grammar Parser

A deterministic parser for OMNISCRIPT language that extracts structured program information to improve analysis accuracy.

## Overview

The OMNISCRIPT Grammar Parser analyzes OMNISCRIPT programs and extracts:

- **Variables**: Types (numeric/string), declarations, assignments, and references
- **Routines**: Structure, local variables, calls, and control flow
- **Database Operations**: Views, updates, field accesses with line numbers
- **Control Flow**: If statements, loops, and nesting structure
- **Function Calls**: Built-in function usage with arguments
- **Call Graph**: Routine-to-routine relationships
- **Comments**: Header documentation extraction

## Installation

The parser is a standalone Python 3 script with no external dependencies.

```bash
cd omniscript-documenter
chmod +x omniscript_grammar_parser.py
```

## Usage

### Command Line

```bash
# Parse a single file
python3 omniscript_grammar_parser.py path/to/program.cbl

# Output is printed to console and saved to *_PARSER_CONTEXT.txt
```

### As a Module

```python
from omniscript_grammar_parser import OmniscriptParser

parser = OmniscriptParser()
program = parser.parse_file('path/to/program.cbl')

# Access parsed data
print(f"Global variables: {list(program.global_variables.keys())}")
print(f"Routines: {list(program.routines.keys())}")
print(f"Database operations: {len(program.database_operations)}")

# Generate report
report = parser.generate_context_report(program)
print(report)
```

## Language Features Recognized

### Variable Declarations

```omniscript
OcLVar_Define(x.FileName n.SevenDaysAgo x.RKPlan n.TradeDate);
```

Extracts:
- Variable names: `FileName`, `SevenDaysAgo`, `RKPlan`, `TradeDate`
- Types: `x` (string), `n` (numeric)
- Scope: Global or routine-specific

### Routines

```omniscript
ROUTINE 'CHECK.SSSA';
  /* routine code */
GOBACK;
```

Captures:
- Routine name
- Start/end line numbers
- Local variables
- Called routines
- Database operations within routine

### Database Operations

```omniscript
poppobj_view(securityid:'POOLLOAN3' datelo:SevenDaysAgo datehi:LastBusiness);
loop while poppobj_next();
  RKPlan = poppobj_de(030);
  TradeDate = poppobj_numde(008);
  poppobj_setde(denum:877 value:Secondary1Buys);
  poppobj_update();
endloop;
```

Recognizes:
- `*obj_view()` with parameters
- `*obj_next()` iterations
- `*obj_de()` and `*obj_numde()` field access
- `*obj_setde()` field updates
- `*obj_update()` commits

Supported objects: `poppobj`, `sssaobj`, `trustobj`, `planobj`

### Control Structures

```omniscript
if OcDate_Valid(RunDate);
   SevenDaysAgo = OcDate_AddDays(RunDate -7);
else;
   SevenDaysAgo = OcDate_AddDays(OcDate_Current() -7);
end;

loop while poppobj_next();
  /* loop body */
endloop;
```

Tracks:
- If/else/end blocks
- Loop/while/endloop structures
- Nesting levels
- Conditions

### Built-in Functions

Recognizes 15+ built-in functions including:
- Date/Time: `OcDate_Current()`, `OcTime_Current()`, `OcDate_AddDays()`, `OcDate_AddBusDays()`, `OcDate_Valid()`
- String: `OcText_string()`, `OCTEXT_GETENV()`, `OcText_Set()`
- Formatting: `OCFMT()`, `OcFMT()`
- File I/O: `OcFile1_Open()`, `OcFile1_Write()`, `OcFile1_Close()`
- Conversion: `octext_tonum()`
- Debug: `OcShow()`

### PERFORM Statements

```omniscript
PERFORM 'CHECK.SSSA';
```

Builds call graph showing which routines call other routines.

### Comments

```omniscript
************************************************
*  NewLoanCash.txt 
*
*  This script reads Plan Position Accounts...
************************************************
```

Extracts header documentation automatically.

## Output Format

### Context Report Structure

```
================================================================================
OMNISCRIPT PROGRAM ANALYSIS: filename.cbl
================================================================================

PROGRAM DOCUMENTATION:
--------------------------------------------------------------------------------
  [Header comments extracted from file]

GLOBAL VARIABLES:
--------------------------------------------------------------------------------
  VariableName (Type)
    Declared: Line X
    Assignments: Lines X, Y, Z

DATABASE OPERATIONS:
--------------------------------------------------------------------------------
  objectname_operation: Lines X, Y, Z

ROUTINES:
--------------------------------------------------------------------------------
  ROUTINE.NAME
    Lines: X-Y
    Local variables: var1, var2
    Calls: OTHER.ROUTINE
    DB Operations: poppobj_view, poppobj_update

CONTROL FLOW:
--------------------------------------------------------------------------------
  IF condition (Line X)
    LOOP while condition (Line Y)
    ENDLOOP (Line Z)
  END (Line W)

BUILT-IN FUNCTION USAGE:
--------------------------------------------------------------------------------
  FunctionName: Used on lines X, Y, Z
```

## Integration with Documentation Workflow

### Before Documentation

Run the parser to generate structured context:

```bash
python3 omniscript_grammar_parser.py temp-repos/santized/GAP_NewLoanCash.cbl
```

This creates `GAP_NewLoanCash_PARSER_CONTEXT.txt` with deterministic analysis.

### During Documentation

When using the omniscript-documenter module, include parser output:

```markdown
## Attach Parser Context

I have run the OMNISCRIPT grammar parser. Here is the structured context:

[Paste contents of *_PARSER_CONTEXT.txt]

Please use this deterministic analysis to ensure accurate documentation.
```

### Automated Integration

Add to `INITIALIZER_PROMPT.md`:

```bash
# Step 1: Generate Parser Context
python3 omniscript_grammar_parser.py path/to/program.cbl

# Step 2: Include in documentation prompt
cat path/to/program_PARSER_CONTEXT.txt
```

## Data Structures

### ParsedProgram

Complete representation of an OMNISCRIPT program:

```python
@dataclass
class ParsedProgram:
    filename: str
    header_comments: List[str]
    global_variables: Dict[str, Variable]
    routines: Dict[str, Routine]
    main_code_lines: Tuple[int, int]
    database_operations: List[DatabaseOperation]
    function_calls: List[FunctionCall]
    control_structures: List[Dict]
    call_graph: Dict[str, List[str]]
```

### Variable

```python
@dataclass
class Variable:
    name: str
    var_type: str  # 'n' or 'x'
    first_assignment: Optional[int]
    assignments: List[int]
    references: List[int]
    scope: str  # 'global' or routine name
```

### Routine

```python
@dataclass
class Routine:
    name: str
    start_line: int
    end_line: int
    variables: Dict[str, Variable]
    calls_to: List[str]
    database_operations: List[Dict]
    control_flow: List[str]
```

### DatabaseOperation

```python
@dataclass
class DatabaseOperation:
    operation_type: str  # 'view', 'next', 'update', etc.
    object_name: str     # 'poppobj', 'sssaobj', etc.
    line_number: int
    parameters: Dict[str, str]
    fields: List[str]
```

## Benefits for Deterministic Analysis

### 1. Accurate Variable Tracking
- Distinguishes between numeric and string types
- Tracks all assignments and references
- Identifies scope (global vs. local)

### 2. Database Operation Context
- Maps all database reads and writes
- Links operations to specific objects and fields
- Tracks transaction patterns

### 3. Control Flow Understanding
- Identifies all conditional logic
- Maps loop structures
- Shows nesting relationships

### 4. Call Graph Generation
- Shows routine dependencies
- Identifies entry points
- Maps execution flow

### 5. Consistent Interpretation
- Parser rules are explicit and documented
- Same code always produces same output
- Reduces AI interpretation variability

## Extending the Parser

### Adding New Built-in Functions

Edit `BUILTIN_FUNCTIONS` set:

```python
BUILTIN_FUNCTIONS = {
    'OcLVar_Define', 'OcText_string',
    'YourNewFunction',  # Add here
    # ...
}
```

### Adding New Database Objects

Edit `DB_OBJECTS` set:

```python
DB_OBJECTS = {
    'poppobj', 'sssaobj',
    'yournewobj',  # Add here
}
```

### Custom Analysis

Subclass `OmniscriptParser` and override methods:

```python
class CustomParser(OmniscriptParser):
    def _parse_custom_construct(self, line, line_num, program, routine):
        # Your custom parsing logic
        pass
```

## Troubleshooting

### Issue: Parser doesn't recognize a function

**Solution**: Add the function to `BUILTIN_FUNCTIONS` set in the parser.

### Issue: Nested function calls not parsed correctly

**Solution**: The current argument parser uses simple splitting. For complex nested calls, use the raw `function_calls` list and manually parse the line.

### Issue: Comment blocks interfere with parsing

**Solution**: The parser strips `/* */` comments. For `*` line comments, they're already handled.

### Issue: Database object not recognized

**Solution**: Add the object prefix to `DB_OBJECTS` set (without `_operation` suffix).

## Future Enhancements

Potential additions to the parser:

1. **Enhanced Argument Parsing**: Better handling of nested function calls
2. **Type Inference**: Deduce variable types from usage patterns
3. **Data Flow Analysis**: Track how values flow through variables
4. **Complexity Metrics**: Calculate cyclomatic complexity
5. **Dead Code Detection**: Identify unused variables and unreachable code
6. **Cross-Reference Generation**: Link all uses of each variable
7. **SQL Pattern Recognition**: Identify embedded SQL patterns
8. **Error Pattern Detection**: Find common error-prone patterns

## Examples

See `examples/parser_usage.md` for complete examples of using the parser programmatically.

## License

Part of the OMNISCRIPT Documentation Module.
