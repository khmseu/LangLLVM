#
%chain = (
           "attribute_namespace" => "identifier",
           "constant_expression" => "conditional_expression",
           "declaration_statement" => "block_declaration",
           "enum_name" => "identifier",
           "expression_list" => "initializer_list",
           "for_range_initializer" => "expr_or_braced_init_list",
           "namespace_alias" => "identifier",
           "template_name" => "identifier",
           "typedef_name" => "identifier",
           "ud_suffix" => "identifier"
         );
-
// Copyright (c) 2019 Kai Henningsen <kai.extern+LangLLVM@gmail.com>
//
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

start
  = translation_unit _
  ;

white_space
  = [ \t\n\v\f\r]+
  ;

cc_char
  = [^*]
  / "*" !"/"
  ;

cc_char_sequence
  = cc_char*
  ;

c_comment
  = "/*" cc_char_sequence "*/"
  ;

cppc_char
  = [^\n]
  ;

cppc_char_sequence
  = cppc_char*
  ;

cpp_comment
  = "//" cppc_char_sequence "\n"
  ;

_ "white space"
  = 
   (
      white_space
    / c_comment
    / cpp_comment
   )*
  ;

namespace_name
  = identifier
  / identifier
  ;

class_name
  = identifier
  / simple_template_id
  ;

hex_quad
  = hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit
  ;

universal_character_name
  = "\\u" hex_quad
  / "\\U" hex_quad hex_quad
  ;

identifier
  = _ identifier_nondigit 
   (
      identifier_nondigit
    / digit
   )*
  ;

identifier_nondigit
  = nondigit
  / universal_character_name
  ;

// one of
nondigit
  = "a"
  / "b"
  / "c"
  / "d"
  / "e"
  / "f"
  / "g"
  / "h"
  / "i"
  / "j"
  / "k"
  / "l"
  / "m"
  / "n"
  / "o"
  / "p"
  / "q"
  / "r"
  / "s"
  / "t"
  / "u"
  / "v"
  / "w"
  / "x"
  / "y"
  / "z"
  / "A"
  / "B"
  / "C"
  / "D"
  / "E"
  / "F"
  / "G"
  / "H"
  / "I"
  / "J"
  / "K"
  / "L"
  / "M"
  / "N"
  / "O"
  / "P"
  / "Q"
  / "R"
  / "S"
  / "T"
  / "U"
  / "V"
  / "W"
  / "X"
  / "Y"
  / "Z"
  / "_"
  ;

// one of
digit
  = "0"
  / "1"
  / "2"
  / "3"
  / "4"
  / "5"
  / "6"
  / "7"
  / "8"
  / "9"
  ;

literal
  = integer_literal
  / _ character_literal
  / floating_literal
  / string_literal
  / boolean_literal
  / pointer_literal
  / user_defined_literal
  ;

integer_literal
  = _ binary_literal integer_suffix?
  / _ octal_literal integer_suffix?
  / _ decimal_literal integer_suffix?
  / _ hexadecimal_literal integer_suffix?
  ;

binary_literal
  = 
   (
      "0b" binary_digit
    / "0B" binary_digit
   ) 
   (
      "’"? binary_digit
   )*
  ;

octal_literal
  = "0" 
   (
      "’"? octal_digit
   )*
  ;

decimal_literal
  = nonzero_digit 
   (
      "’"? digit
   )*
  ;

hexadecimal_literal
  = hexadecimal_prefix hexadecimal_digit_sequence
  ;

binary_digit
  = "0"
  / "1"
  ;

// one of
octal_digit
  = "0"
  / "1"
  / "2"
  / "3"
  / "4"
  / "5"
  / "6"
  / "7"
  ;

// one of
nonzero_digit
  = "1"
  / "2"
  / "3"
  / "4"
  / "5"
  / "6"
  / "7"
  / "8"
  / "9"
  ;

// one of
hexadecimal_prefix
  = "0x"
  / "0X"
  ;

hexadecimal_digit_sequence
  = hexadecimal_digit 
   (
      "’"? hexadecimal_digit
   )*
  ;

// one of
hexadecimal_digit
  = "0"
  / "1"
  / "2"
  / "3"
  / "4"
  / "5"
  / "6"
  / "7"
  / "8"
  / "9"
  / "a"
  / "b"
  / "c"
  / "d"
  / "e"
  / "f"
  / "A"
  / "B"
  / "C"
  / "D"
  / "E"
  / "F"
  ;

integer_suffix
  = unsigned_suffix long_suffix?
  / unsigned_suffix long_long_suffix?
  / long_suffix unsigned_suffix?
  / long_long_suffix unsigned_suffix?
  ;

// one of
unsigned_suffix
  = "u"
  / "U"
  ;

// one of
long_suffix
  = "l"
  / "L"
  ;

// one of
long_long_suffix
  = "ll"
  / "LL"
  ;

character_literal
  = encoding_prefix? "’" c_char_sequence "’"
  ;

// one of
encoding_prefix
  = "u8"
  / "u"
  / "U"
  / "L"
  ;

c_char_sequence
  = c_char+
  ;

c_char
  = [^'\\\n]
  / escape_sequence
  / universal_character_name
  ;

escape_sequence
  = simple_escape_sequence
  / octal_escape_sequence
  / hexadecimal_escape_sequence
  ;

// one of
simple_escape_sequence
  = "\\’"
  / "\\\""
  / "\\?"
  / "\\\\"
  / "\\a"
  / "\\b"
  / "\\f"
  / "\\n"
  / "\\r"
  / "\\t"
  / "\\v"
  ;

octal_escape_sequence
  = "\\" octal_digit
  / "\\" octal_digit octal_digit
  / "\\" octal_digit octal_digit octal_digit
  ;

hexadecimal_escape_sequence
  = 
   (
      "\\x" hexadecimal_digit
   ) hexadecimal_digit*
  ;

floating_literal
  = decimal_floating_literal
  / hexadecimal_floating_literal
  ;

decimal_floating_literal
  = _ fractional_constant exponent_part? floating_suffix?
  / _ digit_sequence exponent_part floating_suffix?
  ;

hexadecimal_floating_literal
  = _ hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part floating_suffix?
  / _ hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part floating_suffix?
  ;

fractional_constant
  = digit_sequence? "." digit_sequence
  / digit_sequence "."
  ;

hexadecimal_fractional_constant
  = hexadecimal_digit_sequence? "." hexadecimal_digit_sequence
  / hexadecimal_digit_sequence "."
  ;

exponent_part
  = "e" sign? digit_sequence
  / "E" sign? digit_sequence
  ;

binary_exponent_part
  = "p" sign? digit_sequence
  / "P" sign? digit_sequence
  ;

// one of
sign
  = "+"
  / "-"
  ;

digit_sequence
  = digit 
   (
      "’"? digit
   )*
  ;

// one of
floating_suffix
  = "f"
  / "l"
  / "F"
  / "L"
  ;

string_literal
  = _ encoding_prefix? '"' s_char_sequence? '"'
  / _ encoding_prefix? "R" raw_string
  ;

s_char_sequence
  = s_char+
  ;

s_char
  = [^"\\\n"]
  / escape_sequence
  / universal_character_name
  ;

raw_string
  = '"' d_char_sequence? "(" r_char_sequence? ")" d_char_sequence? '"'
  ;

r_char_sequence
  = r_char+
  ;

/*initial*/
r_char
  = [^)]
  / ")" !
   (
      d_char_sequence '"'
   )
  ;

d_char_sequence
  = d_char+
  ;

d_char
  = [^ ()\\\t\v\f\n\r]
  ;

boolean_literal
  = _ "false"
  / _ "true"
  ;

pointer_literal
  = _ "nullptr"
  ;

user_defined_literal
  = user_defined_integer_literal
  / user_defined_floating_literal
  / user_defined_string_literal
  / user_defined_character_literal
  ;

user_defined_integer_literal
  = _ decimal_literal identifier
  / _ octal_literal identifier
  / _ hexadecimal_literal identifier
  / _ binary_literal identifier
  ;

user_defined_floating_literal
  = _ fractional_constant exponent_part? identifier
  / _ digit_sequence exponent_part identifier
  / _ hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part identifier
  / _ hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part identifier
  ;

user_defined_string_literal
  = string_literal identifier
  ;

user_defined_character_literal
  = _ character_literal identifier
  ;

translation_unit
  = declaration_seq?
  ;

primary_expression
  = literal
  / _ "this"
  / _ "(" expression _ ")"
  / id_expression
  / lambda_expression
  / fold_expression
  ;

id_expression
  = unqualified_id
  / qualified_id
  ;

unqualified_id
  = identifier
  / operator_function_id
  / conversion_function_id
  / literal_operator_id
  / _ "~" class_name
  / _ "~" decltype_specifier
  / template_id
  ;

qualified_id
  = nested_name_specifier _ "template"? unqualified_id
  ;

nested_name_specifier
  = 
   (
      _ "::"
    / type_name _ "::"
    / namespace_name _ "::"
    / decltype_specifier _ "::"
   ) 
   (
      identifier _ "::"
    / _ "template"? simple_template_id _ "::"
   )*
  ;

lambda_expression
  = lambda_introducer lambda_declarator? compound_statement
  ;

lambda_introducer
  = _ "[" lambda_capture? _ "]"
  ;

lambda_declarator
  = _ "(" parameter_declaration_clause _ ")" decl_specifier_seq?
  / noexcept_specifier? attribute_specifier_seq? trailing_return_type?
  ;

lambda_capture
  = capture_default
  / capture_list
  / capture_default _ "," capture_list
  ;

capture_default
  = _ "&"
  / _ "="
  ;

capture_list
  = 
   (
      capture _ "..."?
   ) 
   (
      _ "," capture _ "..."?
   )*
  ;

capture
  = simple_capture
  / init_capture
  ;

simple_capture
  = identifier
  / _ "&" identifier
  / _ "this"
  / _ "*" _ "this"
  ;

init_capture
  = identifier initializer
  / _ "&" identifier initializer
  ;

fold_expression
  = _ "(" cast_expression fold_operator _ "..." _ ")"
  / _ "(" _ "..." fold_operator cast_expression _ ")"
  / _ "(" cast_expression fold_operator _ "..." fold_operator cast_expression _ ")"
  ;

// one of
fold_operator
  = _ "+"
  / _ "-"
  / _ "*"
  / _ "/"
  / _ "%"
  / _ "^"
  / _ "&"
  / _ "|"
  / _ "<<"
  / _ ">>"
  / _ "+="
  / _ "-="
  / _ "*="
  / _ "/="
  / _ "%="
  / _ "^="
  / _ "&="
  / _ "|="
  / _ "<<="
  / _ ">>="
  / _ "="
  / _ "=="
  / _ "!="
  / _ "<"
  / _ ">"
  / _ "<="
  / _ ">="
  / _ "&&"
  / _ "||"
  / _ ","
  / _ ".*"
  / _ "->*"
  ;

postfix_expression
  = 
   (
      primary_expression
    / simple_type_specifier _ "(" initializer_list? _ ")"
    / typename_specifier _ "(" initializer_list? _ ")"
    / simple_type_specifier braced_init_list
    / typename_specifier braced_init_list
    / _ "dynamic_cast" _ "<" type_id _ ">" _ "(" expression _ ")"
    / _ "static_cast" _ "<" type_id _ ">" _ "(" expression _ ")"
    / _ "reinterpret_cast" _ "<" type_id _ ">" _ "(" expression _ ")"
    / _ "const_cast" _ "<" type_id _ ">" _ "(" expression _ ")"
    / _ "typeid" _ "(" expression _ ")"
    / _ "typeid" _ "(" type_id _ ")"
   ) 
   (
      _ "[" expr_or_braced_init_list _ "]"
    / _ "(" initializer_list? _ ")"
    / _ "." _ "template"? id_expression
    / _ "->" _ "template"? id_expression
    / _ "." pseudo_destructor_name
    / _ "->" pseudo_destructor_name
    / _ "++"
    / _ "--"
   )*
  ;

pseudo_destructor_name
  = nested_name_specifier? type_name _ "::" _ "~" type_name
  / nested_name_specifier _ "template" simple_template_id _ "::" _ "~" type_name
  / _ "~" type_name
  / _ "~" decltype_specifier
  ;

unary_expression
  = postfix_expression
  / _ "++" cast_expression
  / _ "--" cast_expression
  / unary_operator cast_expression
  / _ "sizeof" unary_expression
  / _ "sizeof" _ "(" type_id _ ")"
  / _ "sizeof" _ "..." _ "(" identifier _ ")"
  / _ "alignof" _ "(" type_id _ ")"
  / noexcept_expression
  / new_expression
  / delete_expression
  ;

// one of
unary_operator
  = _ "*"
  / _ "&"
  / _ "+"
  / _ "-"
  / _ "!"
  / _ "~"
  ;

new_expression
  = _ "::"? _ "new" new_placement? new_type_id new_initializer?
  / _ "::"? _ "new" new_placement? _ "(" type_id _ ")" new_initializer?
  ;

new_placement
  = _ "(" initializer_list _ ")"
  ;

new_type_id
  = type_specifier_seq new_declarator?
  ;

new_declarator
  = ptr_operator new_declarator?
  / noptr_new_declarator
  ;

noptr_new_declarator
  = 
   (
      _ "[" expression _ "]" attribute_specifier_seq?
   ) 
   (
      _ "[" conditional_expression _ "]" attribute_specifier_seq?
   )*
  ;

new_initializer
  = _ "(" initializer_list? _ ")"
  / braced_init_list
  ;

delete_expression
  = _ "::"? _ "delete" cast_expression
  / _ "::"? _ "delete" _ "[" _ "]" cast_expression
  ;

noexcept_expression
  = _ "noexcept" _ "(" expression _ ")"
  ;

cast_expression
  = unary_expression
  / _ "(" type_id _ ")" cast_expression
  ;

pm_expression
  = cast_expression 
   (
      _ ".*" cast_expression
    / _ "->*" cast_expression
   )*
  ;

multiplicative_expression
  = pm_expression 
   (
      _ "*" pm_expression
    / _ "/" pm_expression
    / _ "%" pm_expression
   )*
  ;

additive_expression
  = multiplicative_expression 
   (
      _ "+" multiplicative_expression
    / _ "-" multiplicative_expression
   )*
  ;

shift_expression
  = additive_expression 
   (
      _ "<<" additive_expression
    / _ ">>" additive_expression
   )*
  ;

relational_expression
  = shift_expression 
   (
      _ "<" shift_expression
    / _ ">" shift_expression
    / _ "<=" shift_expression
    / _ ">=" shift_expression
   )*
  ;

equality_expression
  = relational_expression 
   (
      _ "==" relational_expression
    / _ "!=" relational_expression
   )*
  ;

and_expression
  = equality_expression 
   (
      _ "&" equality_expression
   )*
  ;

exclusive_or_expression
  = and_expression 
   (
      _ "^" and_expression
   )*
  ;

inclusive_or_expression
  = exclusive_or_expression 
   (
      _ "|" exclusive_or_expression
   )*
  ;

logical_and_expression
  = inclusive_or_expression 
   (
      _ "&&" inclusive_or_expression
   )*
  ;

logical_or_expression
  = logical_and_expression 
   (
      _ "||" logical_and_expression
   )*
  ;

conditional_expression
  = logical_or_expression
  / logical_or_expression _ "?" expression _ ":" assignment_expression
  ;

throw_expression
  = _ "throw" assignment_expression?
  ;

assignment_expression
  = conditional_expression
  / logical_or_expression assignment_operator initializer_clause
  / throw_expression
  ;

// one of
assignment_operator
  = _ "="
  / _ "*="
  / _ "/="
  / _ "%="
  / _ "+="
  / _ "-="
  / _ ">>="
  / _ "<<="
  / _ "&="
  / _ "^="
  / _ "|="
  ;

expression
  = assignment_expression 
   (
      _ "," assignment_expression
   )*
  ;

statement
  = labeled_statement
  / attribute_specifier_seq? expression_statement
  / attribute_specifier_seq? compound_statement
  / attribute_specifier_seq? selection_statement
  / attribute_specifier_seq? iteration_statement
  / attribute_specifier_seq? jump_statement
  / block_declaration
  / attribute_specifier_seq? try_block
  ;

init_statement
  = expression_statement
  / simple_declaration
  ;

condition
  = expression
  / attribute_specifier_seq? decl_specifier_seq declarator brace_or_equal_initializer
  ;

labeled_statement
  = attribute_specifier_seq? identifier _ ":" statement
  / attribute_specifier_seq? _ "case" conditional_expression _ ":" statement
  / attribute_specifier_seq? _ "default" _ ":" statement
  ;

expression_statement
  = expression? _ ";"
  ;

compound_statement
  = _ "{" statement_seq? _ "}"
  ;

statement_seq
  = statement+
  ;

selection_statement
  = _ "if" _ "constexpr"? _ "(" init_statement? condition _ ")" statement
  / _ "if" _ "constexpr"? _ "(" init_statement? condition _ ")" statement _ "else" statement
  / _ "switch" _ "(" init_statement? condition _ ")" statement
  ;

iteration_statement
  = _ "while" _ "(" condition _ ")" statement
  / _ "do" statement _ "while" _ "(" expression _ ")" _ ";"
  / _ "for" _ "(" init_statement condition? _ ";" expression? _ ")" statement
  / _ "for" _ "(" for_range_declaration _ ":" expr_or_braced_init_list _ ")" statement
  ;

for_range_declaration
  = attribute_specifier_seq? decl_specifier_seq declarator
  / attribute_specifier_seq? decl_specifier_seq ref_qualifier? _ "[" identifier_list _ "]"
  ;

jump_statement
  = _ "break" _ ";"
  / _ "continue" _ ";"
  / _ "return" expr_or_braced_init_list? _ ";"
  / _ "goto" identifier _ ";"
  ;

declaration_seq
  = declaration+
  ;

declaration
  = block_declaration
  / nodeclspec_function_declaration
  / function_definition
  / template_declaration
  / deduction_guide
  / explicit_instantiation
  / explicit_specialization
  / linkage_specification
  / namespace_definition
  / empty_declaration
  / attribute_declaration
  ;

block_declaration
  = simple_declaration
  / asm_definition
  / namespace_alias_definition
  / using_declaration
  / using_directive
  / _ "static_assert_declaration"
  / alias_declaration
  / opaque_enum_declaration
  ;

nodeclspec_function_declaration
  = attribute_specifier_seq? declarator _ ";"
  ;

alias_declaration
  = _ "using" identifier attribute_specifier_seq? _ "=" defining_type_id _ ";"
  ;

simple_declaration
  = decl_specifier_seq init_declarator_list? _ ";"
  / attribute_specifier_seq decl_specifier_seq init_declarator_list _ ";"
  / attribute_specifier_seq? decl_specifier_seq ref_qualifier? _ "[" identifier_list _ "]" initializer _ ";"
  / _ "static_assert_declaration:"
  / _ "static_assert" _ "(" conditional_expression _ ")" _ ";"
  / _ "static_assert" _ "(" conditional_expression _ "," string_literal _ ")" _ ";"
  ;

empty_declaration
  = _ ";"
  ;

attribute_declaration
  = attribute_specifier_seq _ ";"
  ;

decl_specifier
  = storage_class_specifier
  / defining_type_specifier
  / function_specifier
  / _ "friend"
  / _ "typedef"
  / _ "constexpr"
  / _ "inline"
  ;

decl_specifier_seq
  = decl_specifier attribute_specifier_seq?
  / decl_specifier decl_specifier_seq
  ;

storage_class_specifier
  = _ "static"
  / _ "thread_local"
  / _ "extern"
  / _ "mutable"
  ;

function_specifier
  = _ "virtual"
  / _ "explicit"
  ;

type_specifier
  = simple_type_specifier
  / elaborated_type_specifier
  / typename_specifier
  / cv_qualifier
  ;

type_specifier_seq
  = type_specifier attribute_specifier_seq?
  / type_specifier type_specifier_seq
  ;

defining_type_specifier
  = type_specifier
  / class_specifier
  / enum_specifier
  ;

defining_type_specifier_seq
  = defining_type_specifier attribute_specifier_seq?
  / defining_type_specifier defining_type_specifier_seq
  ;

simple_type_specifier
  = nested_name_specifier? type_name
  / nested_name_specifier _ "template" simple_template_id
  / nested_name_specifier? identifier
  / _ "char"
  / _ "char16_t"
  / _ "char32_t"
  / _ "wchar_t"
  / _ "bool"
  / _ "short"
  / _ "int"
  / _ "long"
  / _ "signed"
  / _ "unsigned"
  / _ "float"
  / _ "double"
  / _ "void"
  / _ "auto"
  / decltype_specifier
  ;

type_name
  = class_name
  / identifier
  / identifier
  / simple_template_id
  ;

decltype_specifier
  = _ "decltype" _ "(" expression _ ")"
  / _ "decltype" _ "(" _ "auto" _ ")"
  ;

elaborated_type_specifier
  = class_key attribute_specifier_seq? nested_name_specifier? identifier
  / class_key simple_template_id
  / class_key nested_name_specifier _ "template"? simple_template_id
  / _ "enum" nested_name_specifier? identifier
  ;

enum_specifier
  = enum_head _ "{" enumerator_list? _ "}"
  / enum_head _ "{" enumerator_list _ "," _ "}"
  ;

enum_head
  = enum_key attribute_specifier_seq? enum_head_name? enum_base?
  ;

enum_head_name
  = nested_name_specifier? identifier
  ;

opaque_enum_declaration
  = enum_key attribute_specifier_seq? nested_name_specifier? identifier enum_base? _ ";"
  ;

enum_key
  = _ "enum"
  / _ "enum" _ "class"
  / _ "enum" _ "struct"
  ;

enum_base
  = _ ":" type_specifier_seq
  ;

enumerator_list
  = enumerator_definition 
   (
      _ "," enumerator_definition
   )*
  ;

enumerator_definition
  = enumerator
  / enumerator _ "=" conditional_expression
  ;

enumerator
  = identifier attribute_specifier_seq?
  ;

namespace_definition
  = named_namespace_definition
  / unnamed_namespace_definition
  / nested_namespace_definition
  ;

named_namespace_definition
  = _ "inline"? _ "namespace" attribute_specifier_seq? identifier _ "{" namespace_body _ "}"
  ;

unnamed_namespace_definition
  = _ "inline"? _ "namespace" attribute_specifier_seq? _ "{" namespace_body _ "}"
  ;

nested_namespace_definition
  = _ "namespace" enclosing_namespace_specifier _ "::" identifier _ "{" namespace_body _ "}"
  ;

enclosing_namespace_specifier
  = identifier 
   (
      _ "::" identifier
   )*
  ;

namespace_body
  = declaration_seq?
  ;

namespace_alias_definition
  = _ "namespace" identifier _ "=" qualified_namespace_specifier _ ";"
  ;

qualified_namespace_specifier
  = nested_name_specifier? namespace_name
  ;

using_declaration
  = _ "using" using_declarator_list _ ";"
  ;

using_declarator_list
  = 
   (
      using_declarator _ "..."?
   ) 
   (
      _ "," using_declarator _ "..."?
   )*
  ;

using_declarator
  = _ "typename"? nested_name_specifier unqualified_id
  ;

using_directive
  = attribute_specifier_seq? _ "using" _ "namespace" nested_name_specifier? namespace_name _ ";"
  ;

asm_definition
  = attribute_specifier_seq? _ "asm" _ "(" string_literal _ ")" _ ";"
  ;

linkage_specification
  = _ "extern" string_literal _ "{" declaration_seq? _ "}"
  / _ "extern" string_literal declaration
  ;

attribute_specifier_seq
  = attribute_specifier+
  ;

attribute_specifier
  = _ "[" _ "[" attribute_using_prefix? attribute_list _ "]" _ "]"
  / alignment_specifier
  ;

alignment_specifier
  = _ "alignas" _ "(" type_id _ "..."? _ ")"
  / _ "alignas" _ "(" conditional_expression _ "..."? _ ")"
  ;

attribute_using_prefix
  = _ "using" identifier _ ":"
  ;

attribute_list
  = 
   (
      attribute?
    / attribute _ "..."
   ) 
   (
      _ "," attribute?
    / _ "," attribute _ "..."
   )*
  ;

attribute
  = attribute_token attribute_argument_clause?
  ;

attribute_token
  = identifier
  / attribute_scoped_token
  ;

attribute_scoped_token
  = identifier _ "::" identifier
  ;

attribute_argument_clause
  = _ "(" balanced_token_seq? _ ")"
  ;

balanced_token_seq
  = balanced_token+
  ;

balanced_token
  = _ "(" balanced_token_seq? _ ")"
  / _ "[" balanced_token_seq? _ "]"
  / _ "{" balanced_token_seq? _ "}"
  / _ "any token other than a parenthesis, a bracket, or a brace"
  ;

init_declarator_list
  = init_declarator 
   (
      _ "," init_declarator
   )*
  ;

init_declarator
  = declarator initializer?
  ;

declarator
  = ptr_declarator
  / noptr_declarator parameters_and_qualifiers? trailing_return_type
  ;

ptr_declarator
  = noptr_declarator
  / ptr_operator ptr_declarator
  ;

noptr_declarator
  = 
   (
      declarator_id attribute_specifier_seq?
    / _ "(" ptr_declarator _ ")"
   ) 
   (
      parameters_and_qualifiers
    / _ "[" conditional_expression? _ "]" attribute_specifier_seq?
   )*
  ;

parameters_and_qualifiers
  = _ "(" parameter_declaration_clause _ ")" cv_qualifier_seq?
  / ref_qualifier noexcept_specifier? attribute_specifier_seq?
  / ref_qualifier? noexcept_specifier attribute_specifier_seq?
  / ref_qualifier? noexcept_specifier? attribute_specifier_seq
  ;

trailing_return_type
  = _ "->" type_id
  ;

ptr_operator
  = _ "*" attribute_specifier_seq? cv_qualifier_seq?
  / _ "&" attribute_specifier_seq?
  / _ "&&" attribute_specifier_seq?
  / nested_name_specifier _ "*" attribute_specifier_seq? cv_qualifier_seq?
  ;

cv_qualifier_seq
  = cv_qualifier cv_qualifier_seq?
  ;

cv_qualifier
  = _ "const"
  / _ "volatile"
  ;

ref_qualifier
  = _ "&"
  / _ "&&"
  ;

declarator_id
  = _ "..."? id_expression
  ;

type_id
  = type_specifier_seq abstract_declarator?
  ;

defining_type_id
  = defining_type_specifier_seq abstract_declarator?
  ;

abstract_declarator
  = ptr_abstract_declarator
  / noptr_abstract_declarator? parameters_and_qualifiers? trailing_return_type
  / abstract_pack_declarator
  ;

ptr_abstract_declarator
  = noptr_abstract_declarator
  / ptr_operator ptr_abstract_declarator?
  ;

noptr_abstract_declarator
  = 
   (
      parameters_and_qualifiers?
    / _ "[" conditional_expression? _ "]" attribute_specifier_seq?
    / _ "(" ptr_abstract_declarator _ ")"
   ) 
   (
      parameters_and_qualifiers
    / _ "[" conditional_expression? _ "]" attribute_specifier_seq?
   )*
  ;

abstract_pack_declarator
  = noptr_abstract_pack_declarator
  / ptr_operator abstract_pack_declarator
  ;

noptr_abstract_pack_declarator
  = 
   (
      _ "..."
   ) 
   (
      parameters_and_qualifiers
    / _ "[" conditional_expression? _ "]" attribute_specifier_seq?
   )*
  ;

parameter_declaration_clause
  = parameter_declaration_list? _ "..."?
  / parameter_declaration_list _ "," _ "..."
  ;

parameter_declaration_list
  = parameter_declaration 
   (
      _ "," parameter_declaration
   )*
  ;

parameter_declaration
  = attribute_specifier_seq? decl_specifier_seq declarator
  / attribute_specifier_seq? decl_specifier_seq declarator _ "=" initializer_clause
  / attribute_specifier_seq? decl_specifier_seq abstract_declarator?
  / attribute_specifier_seq? decl_specifier_seq abstract_declarator? _ "=" initializer_clause
  ;

function_definition
  = attribute_specifier_seq? decl_specifier_seq? declarator virt_specifier_seq? function_body
  ;

function_body
  = ctor_initializer? compound_statement
  / function_try_block
  / _ "=" _ "default" _ ";"
  / _ "=" _ "delete" _ ";"
  ;

initializer
  = brace_or_equal_initializer
  / _ "(" initializer_list _ ")"
  ;

brace_or_equal_initializer
  = _ "=" initializer_clause
  / braced_init_list
  ;

initializer_clause
  = assignment_expression
  / braced_init_list
  ;

initializer_list
  = 
   (
      initializer_clause _ "..."?
   ) 
   (
      _ "," initializer_clause _ "..."?
   )*
  ;

braced_init_list
  = _ "{" initializer_list _ ","? _ "}"
  / _ "{}"
  ;

expr_or_braced_init_list
  = expression
  / braced_init_list
  ;

class_specifier
  = class_head _ "{" member_specification? _ "}"
  ;

class_head
  = class_key attribute_specifier_seq? class_head_name class_virt_specifier? base_clause?
  / class_key attribute_specifier_seq? base_clause?
  ;

class_head_name
  = nested_name_specifier? class_name
  ;

class_virt_specifier
  = _ "final"
  ;

class_key
  = _ "class"
  / _ "struct"
  / _ "union"
  ;

member_specification
  = member_declaration member_specification?
  / access_specifier _ ":" member_specification?
  ;

member_declaration
  = attribute_specifier_seq? decl_specifier_seq? member_declarator_list? _ ";"
  / function_definition
  / using_declaration
  / _ "static_assert_declaration"
  / template_declaration
  / deduction_guide
  / alias_declaration
  / empty_declaration
  ;

member_declarator_list
  = member_declarator 
   (
      _ "," member_declarator
   )*
  ;

member_declarator
  = declarator virt_specifier_seq? pure_specifier?
  / declarator brace_or_equal_initializer?
  / identifier? attribute_specifier_seq? _ ":" conditional_expression
  ;

virt_specifier_seq
  = virt_specifier+
  ;

virt_specifier
  = _ "override"
  / _ "final"
  ;

pure_specifier
  = _ "=" _ "0"
  ;

base_clause
  = _ ":" base_specifier_list
  ;

base_specifier_list
  = 
   (
      base_specifier _ "..."?
   ) 
   (
      _ "," base_specifier _ "..."?
   )*
  ;

base_specifier
  = attribute_specifier_seq? class_or_decltype
  / attribute_specifier_seq? _ "virtual" access_specifier? class_or_decltype
  / attribute_specifier_seq? access_specifier _ "virtual"? class_or_decltype
  ;

class_or_decltype
  = nested_name_specifier? class_name
  / nested_name_specifier _ "template" simple_template_id
  / decltype_specifier
  ;

access_specifier
  = _ "private"
  / _ "protected"
  / _ "public"
  ;

conversion_function_id
  = operator conversion_type_id
  ;

conversion_type_id
  = type_specifier_seq conversion_declarator?
  ;

conversion_declarator
  = ptr_operator conversion_declarator?
  ;

ctor_initializer
  = _ ":" mem_initializer_list
  ;

mem_initializer_list
  = 
   (
      mem_initializer _ "..."?
   ) 
   (
      _ "," mem_initializer _ "..."?
   )*
  ;

mem_initializer
  = mem_initializer_id _ "(" initializer_list? _ ")"
  / mem_initializer_id braced_init_list
  ;

mem_initializer_id
  = class_or_decltype
  / identifier
  ;

operator_function_id
  = operator operator
  ;

// one of
operator
  = _ "new"
  / _ "delete"
  / _ "new" _ "[" _ "]"
  / _ "delete" _ "[" _ "]"
  / _ "+"
  / _ "-"
  / _ "*"
  / _ "/"
  / _ "%"
  / _ "^"
  / _ "&"
  / _ "|"
  / _ "~"
  / _ "!"
  / _ "="
  / _ "<"
  / _ ">"
  / _ "+="
  / _ "-="
  / _ "*="
  / _ "/="
  / _ "%="
  / _ "^="
  / _ "&="
  / _ "|="
  / _ "<<"
  / _ ">>"
  / _ ">>="
  / _ "<<="
  / _ "=="
  / _ "!="
  / _ "<="
  / _ ">="
  / _ "&&"
  / _ "||"
  / _ "++"
  / _ "--"
  / _ ","
  / _ "->*"
  / _ "->"
  / _ "()"
  / _ "[]"
  ;

literal_operator_id
  = operator string_literal identifier
  / operator user_defined_string_literal
  ;

template_declaration
  = _ "template" _ "<" template_parameter_list _ ">" declaration
  ;

template_parameter_list
  = template_parameter 
   (
      _ "," template_parameter
   )*
  ;

template_parameter
  = type_parameter
  / parameter_declaration
  ;

type_parameter
  = type_parameter_key _ "..."? identifier?
  / type_parameter_key identifier? _ "=" type_id
  / _ "template" _ "<" template_parameter_list _ ">" type_parameter_key _ "..."? identifier?
  / _ "template" _ "<" template_parameter_list _ ">" type_parameter_key identifier? _ "=" id_expression
  ;

type_parameter_key
  = _ "class"
  / _ "typename"
  ;

simple_template_id
  = identifier _ "<" template_argument_list? _ ">"
  ;

template_id
  = simple_template_id
  / operator_function_id _ "<" template_argument_list? _ ">"
  / literal_operator_id _ "<" template_argument_list? _ ">"
  ;

template_argument_list
  = 
   (
      template_argument _ "..."?
   ) 
   (
      _ "," template_argument _ "..."?
   )*
  ;

template_argument
  = conditional_expression
  / type_id
  / id_expression
  ;

typename_specifier
  = _ "typename" nested_name_specifier identifier
  / _ "typename" nested_name_specifier _ "template"? simple_template_id
  ;

explicit_instantiation
  = _ "extern"? _ "template" declaration
  ;

explicit_specialization
  = _ "template" _ "<" _ ">" declaration
  ;

deduction_guide
  = _ "explicit"? identifier _ "(" parameter_declaration_clause _ ")" _ "->" simple_template_id _ ";"
  ;

try_block
  = _ "try" compound_statement handler_seq
  ;

function_try_block
  = _ "try" ctor_initializer? compound_statement handler_seq
  ;

handler_seq
  = handler handler_seq?
  ;

handler
  = _ "catch" _ "(" exception_declaration _ ")" compound_statement
  ;

exception_declaration
  = attribute_specifier_seq? type_specifier_seq declarator
  / attribute_specifier_seq? type_specifier_seq abstract_declarator?
  / _ "..."
  ;

noexcept_specifier
  = _ "noexcept" _ "(" conditional_expression _ ")"
  / _ "noexcept"
  / _ "throw" _ "(" _ ")"
  ;

identifier_list
  = identifier 
   (
      _ "," identifier
   )*
  ;

