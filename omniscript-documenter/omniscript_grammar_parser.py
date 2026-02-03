#!/usr/bin/env python3
"""
OMNISCRIPT Language Grammar Parser
Provides deterministic parsing and analysis of OMNISCRIPT programs.

This parser extracts structured information from OMNISCRIPT files including:
- Variables and their declarations
- Routines and their control flow
- Database operations (views, reads, updates)
- File operations
- Built-in function calls
- Comments and documentation
"""

import re
from typing import Dict, List, Tuple, Optional, Set
from dataclasses import dataclass, field
from enum import Enum


class TokenType(Enum):
    """OMNISCRIPT token types"""
    # Keywords
    KEYWORD_IF = "IF"
    KEYWORD_ELSE = "ELSE"
    KEYWORD_END = "END"
    KEYWORD_LOOP = "LOOP"
    KEYWORD_WHILE = "WHILE"
    KEYWORD_ENDLOOP = "ENDLOOP"
    KEYWORD_ROUTINE = "ROUTINE"
    KEYWORD_PERFORM = "PERFORM"
    KEYWORD_GOBACK = "GOBACK"
    
    # Data types
    TYPE_NUMERIC = "n."
    TYPE_STRING = "x."
    
    # Operators
    OPERATOR_ASSIGN = "="
    OPERATOR_COMPARE = "<>|>|<|>=|<="
    OPERATOR_ARITHMETIC = "+|-|*|/"
    OPERATOR_AND = "and"
    OPERATOR_OR = "or"
    
    # Identifiers
    IDENTIFIER = "IDENTIFIER"
    STRING_LITERAL = "STRING"
    NUMBER_LITERAL = "NUMBER"
    
    # Comments
    COMMENT_BLOCK = "COMMENT_BLOCK"
    COMMENT_INLINE = "COMMENT_INLINE"
    
    # Built-in functions
    BUILTIN_FUNC = "BUILTIN"
    
    # Database operations
    DB_VIEW = "DB_VIEW"
    DB_NEXT = "DB_NEXT"
    DB_UPDATE = "DB_UPDATE"
    
    # File operations
    FILE_OPEN = "FILE_OPEN"
    FILE_WRITE = "FILE_WRITE"
    FILE_CLOSE = "FILE_CLOSE"


@dataclass
class Variable:
    """Represents a variable in OMNISCRIPT"""
    name: str
    var_type: str  # 'n' for numeric, 'x' for string
    first_assignment: Optional[int] = None  # line number
    assignments: List[int] = field(default_factory=list)  # all assignment lines
    references: List[int] = field(default_factory=list)  # all reference lines
    is_parameter: bool = False
    scope: str = "global"  # 'global' or routine name


@dataclass
class Routine:
    """Represents a routine/procedure in OMNISCRIPT"""
    name: str
    start_line: int
    end_line: int
    variables: Dict[str, Variable] = field(default_factory=dict)
    calls_to: List[str] = field(default_factory=list)  # routines this calls
    database_operations: List[Dict] = field(default_factory=list)
    control_flow: List[str] = field(default_factory=list)  # if, loop, etc.


@dataclass
class DatabaseOperation:
    """Represents a database operation"""
    operation_type: str  # 'view', 'next', 'update', 'setde', 'de', 'numde'
    object_name: str  # e.g., 'poppobj', 'sssaobj'
    line_number: int
    parameters: Dict[str, str] = field(default_factory=dict)
    fields: List[str] = field(default_factory=list)


@dataclass
class FunctionCall:
    """Represents a built-in function call"""
    function_name: str
    line_number: int
    arguments: List[str] = field(default_factory=list)
    return_var: Optional[str] = None


@dataclass
class ParsedProgram:
    """Complete parsed OMNISCRIPT program"""
    filename: str
    header_comments: List[str] = field(default_factory=list)
    global_variables: Dict[str, Variable] = field(default_factory=dict)
    routines: Dict[str, Routine] = field(default_factory=dict)
    main_code_lines: Tuple[int, int] = (0, 0)
    database_operations: List[DatabaseOperation] = field(default_factory=list)
    function_calls: List[FunctionCall] = field(default_factory=list)
    file_operations: List[Dict] = field(default_factory=list)
    control_structures: List[Dict] = field(default_factory=list)
    call_graph: Dict[str, List[str]] = field(default_factory=dict)


class OmniscriptParser:
    """Parser for OMNISCRIPT language"""
    
    # Built-in function patterns
    BUILTIN_FUNCTIONS = {
        'OcLVar_Define', 'OcText_string', 'OCTEXT_GETENV', 'OCFMT', 'OcFMT',
        'OcDate_Current', 'OcTime_Current', 'OcShow', 'OcFile1_Open',
        'OcFile1_Write', 'OcFile1_Close', 'octext_tonum', 'OcDate_Valid',
        'OcDate_AddDays', 'OcDate_AddBusDays', 'OcText_Set'
    }
    
    # Database object patterns
    DB_OBJECTS = {
        'poppobj', 'sssaobj', 'trustobj', 'planobj'
    }
    
    # DB operations
    DB_OPS = {
        'view', 'next', 'update', 'setde', 'de', 'numde', 'delete', 'add'
    }
    
    def __init__(self):
        self.current_line = 0
        self.current_routine = None
        
    def parse_file(self, filepath: str) -> ParsedProgram:
        """Parse an OMNISCRIPT file and return structured program data"""
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
        
        return self.parse_lines(lines, filepath)
    
    def parse_lines(self, lines: List[str], filename: str = "unknown") -> ParsedProgram:
        """Parse OMNISCRIPT code lines"""
        program = ParsedProgram(filename=filename)
        
        # Extract header comments
        program.header_comments = self._extract_header_comments(lines)
        
        # First pass: identify routines
        routines = self._identify_routines(lines)
        program.routines = routines
        
        # Second pass: parse main code and routines
        in_routine = None
        routine_start = None
        main_start = None
        main_end = None
        
        for i, line in enumerate(lines, 1):
            self.current_line = i
            cleaned = line.strip()
            
            # Skip empty lines and comments
            if not cleaned or cleaned.startswith('*'):
                continue
            
            # Check for routine definition
            if self._is_routine_start(cleaned):
                routine_name = self._extract_routine_name(cleaned)
                in_routine = routine_name
                routine_start = i
                if routine_name in routines:
                    routines[routine_name].start_line = i
                continue
            
            # Check for routine end
            if cleaned.upper() == 'GOBACK;':
                if in_routine and in_routine in routines:
                    routines[in_routine].end_line = i
                in_routine = None
                continue
            
            # Track main code boundaries
            if main_start is None and not in_routine and not cleaned.startswith('*'):
                main_start = i
            if not in_routine and not cleaned.startswith('*'):
                main_end = i
            
            # Parse the line
            self._parse_line(cleaned, i, program, in_routine)
        
        program.main_code_lines = (main_start or 1, main_end or len(lines))
        
        # Build call graph
        program.call_graph = self._build_call_graph(program)
        
        return program
    
    def _extract_header_comments(self, lines: List[str]) -> List[str]:
        """Extract header comment block"""
        comments = []
        in_header = False
        
        for line in lines:
            stripped = line.strip()
            if stripped.startswith('*'):
                in_header = True
                # Remove leading * and spaces
                comment_text = stripped.lstrip('*').strip()
                if comment_text:
                    comments.append(comment_text)
            elif in_header and stripped:
                # End of header comments
                break
        
        return comments
    
    def _is_routine_start(self, line: str) -> bool:
        """Check if line starts a routine definition"""
        return line.upper().startswith('ROUTINE ')
    
    def _extract_routine_name(self, line: str) -> str:
        """Extract routine name from ROUTINE declaration"""
        match = re.search(r"ROUTINE\s+'([^']+)'", line, re.IGNORECASE)
        if match:
            return match.group(1)
        return "UNKNOWN"
    
    def _identify_routines(self, lines: List[str]) -> Dict[str, Routine]:
        """First pass: identify all routines in the program"""
        routines = {}
        
        for i, line in enumerate(lines, 1):
            cleaned = line.strip()
            if self._is_routine_start(cleaned):
                routine_name = self._extract_routine_name(cleaned)
                routines[routine_name] = Routine(
                    name=routine_name,
                    start_line=i,
                    end_line=-1
                )
        
        return routines
    
    def _parse_line(self, line: str, line_num: int, program: ParsedProgram, 
                    current_routine: Optional[str]):
        """Parse a single line of OMNISCRIPT code"""
        # Remove inline comments
        if '/*' in line:
            line = line.split('/*')[0]
        
        line = line.strip()
        if not line:
            return
        
        # Variable declaration: OcLVar_Define(...)
        if 'OcLVar_Define' in line:
            self._parse_variable_declaration(line, line_num, program, current_routine)
        
        # Assignment: var = value;
        elif '=' in line and not any(op in line for op in ['<>', '>=', '<=', '==', '!=']):
            self._parse_assignment(line, line_num, program, current_routine)
        
        # Database operations
        elif any(f'{obj}_' in line.lower() for obj in self.DB_OBJECTS):
            self._parse_database_operation(line, line_num, program, current_routine)
        
        # Function calls
        elif any(func in line for func in self.BUILTIN_FUNCTIONS):
            self._parse_function_call(line, line_num, program, current_routine)
        
        # Control structures
        elif line.upper().startswith(('IF ', 'LOOP ', 'END;', 'ENDLOOP;')):
            self._parse_control_structure(line, line_num, program, current_routine)
        
        # PERFORM calls
        elif 'PERFORM' in line.upper():
            self._parse_perform(line, line_num, program, current_routine)
    
    def _parse_variable_declaration(self, line: str, line_num: int, 
                                   program: ParsedProgram, current_routine: Optional[str]):
        """Parse OcLVar_Define statement"""
        # Extract variable names from OcLVar_Define(x.var1 n.var2 ...)
        match = re.search(r'OcLVar_Define\((.*?)\)', line, re.IGNORECASE)
        if not match:
            return
        
        var_string = match.group(1)
        # Split by whitespace, handling multiple spaces
        tokens = var_string.split()
        
        for token in tokens:
            token = token.strip()
            if not token:
                continue
            
            # Determine type and name
            if token.startswith('n.'):
                var_type = 'n'
                var_name = token[2:]
            elif token.startswith('x.'):
                var_type = 'x'
                var_name = token[2:]
            else:
                # No type prefix, assume string
                var_type = 'x'
                var_name = token
            
            variable = Variable(
                name=var_name,
                var_type=var_type,
                first_assignment=line_num,
                scope=current_routine or 'global'
            )
            
            if current_routine:
                if current_routine in program.routines:
                    program.routines[current_routine].variables[var_name] = variable
            else:
                program.global_variables[var_name] = variable
    
    def _parse_assignment(self, line: str, line_num: int, 
                         program: ParsedProgram, current_routine: Optional[str]):
        """Parse variable assignment"""
        # Simple assignment: VarName = value;
        match = re.match(r'(\w+)\s*=\s*(.+?);?$', line)
        if not match:
            return
        
        var_name = match.group(1)
        value = match.group(2).strip(';')
        
        # Find or create variable
        var_dict = program.global_variables
        if current_routine and current_routine in program.routines:
            var_dict = program.routines[current_routine].variables
        
        if var_name in var_dict:
            var_dict[var_name].assignments.append(line_num)
        elif var_name in program.global_variables:
            program.global_variables[var_name].assignments.append(line_num)
        else:
            # Create variable on first assignment
            variable = Variable(
                name=var_name,
                var_type='x',  # Default to string
                first_assignment=line_num,
                assignments=[line_num],
                scope=current_routine or 'global'
            )
            var_dict[var_name] = variable
    
    def _parse_database_operation(self, line: str, line_num: int,
                                  program: ParsedProgram, current_routine: Optional[str]):
        """Parse database operations like poppobj_view, sssaobj_next, etc."""
        line_lower = line.lower()
        
        # Find the database object and operation
        db_obj = None
        db_op = None
        
        for obj in self.DB_OBJECTS:
            for op in self.DB_OPS:
                pattern = f'{obj}_{op}'
                if pattern in line_lower:
                    db_obj = obj
                    db_op = op
                    break
            if db_obj:
                break
        
        if not db_obj or not db_op:
            return
        
        # Extract parameters for view operations
        params = {}
        if db_op == 'view':
            # Extract key:value pairs
            param_pattern = r'(\w+):([^\s,)]+)'
            matches = re.finditer(param_pattern, line)
            for match in matches:
                key = match.group(1)
                value = match.group(2).strip("'\"")
                params[key] = value
        
        # Extract field numbers for de/numde operations
        fields = []
        if db_op in ['de', 'numde', 'setde']:
            field_match = re.search(r'\((?:denum:)?(\d+)\)', line)
            if field_match:
                fields.append(field_match.group(1))
        
        db_operation = DatabaseOperation(
            operation_type=db_op,
            object_name=db_obj,
            line_number=line_num,
            parameters=params,
            fields=fields
        )
        
        program.database_operations.append(db_operation)
        
        if current_routine and current_routine in program.routines:
            program.routines[current_routine].database_operations.append({
                'type': db_op,
                'object': db_obj,
                'line': line_num
            })
    
    def _parse_function_call(self, line: str, line_num: int,
                           program: ParsedProgram, current_routine: Optional[str]):
        """Parse built-in function calls"""
        for func_name in self.BUILTIN_FUNCTIONS:
            if func_name in line:
                # Extract return variable if assignment
                return_var = None
                if '=' in line:
                    return_match = re.match(r'(\w+)\s*=', line)
                    if return_match:
                        return_var = return_match.group(1)
                
                # Extract arguments (simplified)
                arg_match = re.search(rf'{func_name}\((.*?)\)', line, re.IGNORECASE)
                args = []
                if arg_match:
                    arg_string = arg_match.group(1)
                    # Basic argument splitting (doesn't handle nested calls perfectly)
                    args = [arg.strip().strip("'\"") for arg in arg_string.split()]
                
                func_call = FunctionCall(
                    function_name=func_name,
                    line_number=line_num,
                    arguments=args,
                    return_var=return_var
                )
                
                program.function_calls.append(func_call)
                break
    
    def _parse_control_structure(self, line: str, line_num: int,
                                program: ParsedProgram, current_routine: Optional[str]):
        """Parse control structures (if, loop, end)"""
        line_upper = line.upper()
        
        structure_type = None
        condition = None
        
        if line_upper.startswith('IF '):
            structure_type = 'if'
            condition = line[3:].strip().rstrip(';')
        elif line_upper.startswith('LOOP '):
            structure_type = 'loop'
            condition = line[5:].strip().rstrip(';')
        elif line_upper.startswith('END;'):
            structure_type = 'end'
        elif line_upper.startswith('ENDLOOP;'):
            structure_type = 'endloop'
        elif line_upper.startswith('ELSE;'):
            structure_type = 'else'
        
        if structure_type:
            control = {
                'type': structure_type,
                'line': line_num,
                'condition': condition
            }
            program.control_structures.append(control)
            
            if current_routine and current_routine in program.routines:
                program.routines[current_routine].control_flow.append(structure_type)
    
    def _parse_perform(self, line: str, line_num: int,
                      program: ParsedProgram, current_routine: Optional[str]):
        """Parse PERFORM statements"""
        match = re.search(r"PERFORM\s+'([^']+)'", line, re.IGNORECASE)
        if match:
            called_routine = match.group(1)
            
            if current_routine and current_routine in program.routines:
                program.routines[current_routine].calls_to.append(called_routine)
    
    def _build_call_graph(self, program: ParsedProgram) -> Dict[str, List[str]]:
        """Build routine call graph"""
        call_graph = {}
        
        # Main program calls
        main_calls = []
        for routine in program.routines.values():
            if routine.start_line > program.main_code_lines[0]:
                # This is a routine definition, not a call
                continue
        
        # Check for PERFORM in main code section
        # This requires re-parsing which we'll simplify here
        
        # Routine to routine calls
        for routine_name, routine in program.routines.items():
            call_graph[routine_name] = routine.calls_to
        
        return call_graph
    
    def generate_context_report(self, program: ParsedProgram) -> str:
        """Generate a comprehensive context report for the parsed program"""
        report = []
        
        report.append("=" * 80)
        report.append(f"OMNISCRIPT PROGRAM ANALYSIS: {program.filename}")
        report.append("=" * 80)
        report.append("")
        
        # Header comments
        if program.header_comments:
            report.append("PROGRAM DOCUMENTATION:")
            report.append("-" * 80)
            for comment in program.header_comments:
                report.append(f"  {comment}")
            report.append("")
        
        # Global variables
        if program.global_variables:
            report.append("GLOBAL VARIABLES:")
            report.append("-" * 80)
            for var_name, var in sorted(program.global_variables.items()):
                var_type_name = "Numeric" if var.var_type == 'n' else "String"
                report.append(f"  {var_name} ({var_type_name})")
                if var.first_assignment:
                    report.append(f"    Declared: Line {var.first_assignment}")
                if var.assignments:
                    report.append(f"    Assignments: Lines {', '.join(map(str, var.assignments))}")
            report.append("")
        
        # Database operations summary
        if program.database_operations:
            report.append("DATABASE OPERATIONS:")
            report.append("-" * 80)
            db_summary = {}
            for db_op in program.database_operations:
                key = f"{db_op.object_name}_{db_op.operation_type}"
                if key not in db_summary:
                    db_summary[key] = []
                db_summary[key].append(db_op.line_number)
            
            for key, lines in sorted(db_summary.items()):
                report.append(f"  {key}: Lines {', '.join(map(str, lines))}")
            report.append("")
        
        # Routines
        if program.routines:
            report.append("ROUTINES:")
            report.append("-" * 80)
            for routine_name, routine in sorted(program.routines.items()):
                report.append(f"  {routine_name}")
                report.append(f"    Lines: {routine.start_line}-{routine.end_line}")
                if routine.variables:
                    report.append(f"    Local variables: {', '.join(routine.variables.keys())}")
                if routine.calls_to:
                    report.append(f"    Calls: {', '.join(routine.calls_to)}")
                if routine.database_operations:
                    db_ops = [f"{op['object']}_{op['type']}" for op in routine.database_operations]
                    report.append(f"    DB Operations: {', '.join(set(db_ops))}")
                report.append("")
        
        # Control structures
        if program.control_structures:
            report.append("CONTROL FLOW:")
            report.append("-" * 80)
            indent = 0
            for ctrl in program.control_structures:
                if ctrl['type'] in ['end', 'endloop']:
                    indent = max(0, indent - 2)
                
                prefix = " " * indent
                if ctrl['condition']:
                    report.append(f"  {prefix}{ctrl['type'].upper()} {ctrl['condition']} (Line {ctrl['line']})")
                else:
                    report.append(f"  {prefix}{ctrl['type'].upper()} (Line {ctrl['line']})")
                
                if ctrl['type'] in ['if', 'loop']:
                    indent += 2
            report.append("")
        
        # Function calls summary
        if program.function_calls:
            report.append("BUILT-IN FUNCTION USAGE:")
            report.append("-" * 80)
            func_summary = {}
            for func_call in program.function_calls:
                if func_call.function_name not in func_summary:
                    func_summary[func_call.function_name] = []
                func_summary[func_call.function_name].append(func_call.line_number)
            
            for func_name, lines in sorted(func_summary.items()):
                report.append(f"  {func_name}: Used on lines {', '.join(map(str, lines))}")
            report.append("")
        
        report.append("=" * 80)
        return "\n".join(report)


def main():
    """CLI interface for the parser"""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python omniscript_grammar_parser.py <omniscript_file>")
        sys.exit(1)
    
    filepath = sys.argv[1]
    parser = OmniscriptParser()
    
    try:
        program = parser.parse_file(filepath)
        report = parser.generate_context_report(program)
        print(report)
        
        # Optionally save to file
        import os
        base = os.path.splitext(filepath)[0]
        output_file = f"{base}_PARSER_CONTEXT.txt"
        with open(output_file, 'w') as f:
            f.write(report)
        print(f"\nContext report saved to: {output_file}")
        
    except Exception as e:
        print(f"Error parsing file: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
