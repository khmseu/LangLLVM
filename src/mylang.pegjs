// Copyright 2019 kai
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE_2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
start
  = something
/* operator
  = "new" / "delete" / "new" "[]" / "delete" "[]"
  / "+" / "-" / "*" / "/" / "%" / "^" / "&" / "|" / "~"
  / "!" / "=" / "<" / ">" / "+=" / "-=" / "*=" / "/=" / "%="
  / "^=" / "&=" / "|=" / "<<" / ">>" / ">>=" / "<<=" / "==" / "!="
  / "<=" / ">=" / "&&" / "||" / "++" / "--" / "," / "->*" / "->"
  / "()" / "[]" */
/* keyword
  = "alignas" / "continue" / "friend" / "register" / "true"
  / "alignof" / "decltype" / "goto" / "reinterpret_cast" / "try"
  / "asm" / "default" / "if" / "return" / "typedef"
  / "auto" / "delete" / "inline" / "short" / "typeid"
  / "bool" / "do" / "int" / "signed" / "typename"
  / "break" / "double" / "long" / "sizeof" / "union"
  / "case" / "dynamic_cast" / "mutable" / "static" / "unsigned"
  / "catch" / "else" / "namespace" / "static_assert" / "using"
  / "char" / "enum" / "new" / "static_cast" / "virtual"
  / "char16_t" / "explicit" / "noexcept" / "struct" / "void"
  / "char32_t" / "export" / "nullptr" / "switch" / "volatile"
  / "class" / "extern" / "operator" / "template" / "wchar_t"
  / "const" / "false" / "private" / "this" / "while"
  / "constexpr" / "float" / "protected" / "thread_local"
  / "const_cast" / "for" / "public" / "throw" */
hex_quad
  = hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit
universal_character_name
  = "\\u" hex_quad
  / "\\U" hex_quad hex_quad
/* preprocessing_token
  = header_name
  / identifier
  / pp_number
  / character_literal
  / user_defined_character_literal
  / string_literal
  / user_defined_string_literal
  / preprocessing_op_or_punc
  / [^ \t\v\n\f] */
/*token
  = identifier
  / keyword
  / literal
  / operator
  / punctuator*/
/* header_name
  = "<" h_char_sequence ">"
  / '"' q_char_sequence '"' */
/* h_char_sequence
  = h_char+ */
/* h_char
  = [^\n>] */
/* q_char_sequence
  = q_char+ */
/* q_char
  = [^\n"] */
/* pp_number
  = "."? digit (
    digit
  /  identifier_nondigit
  /  "'" digit
  /  "'" nondigit
  /  "e" sign
  /  "E" sign
  /  "p" sign
  /  "P" sign
  /  "." )* */
identifier
  = identifier_nondigit ( identifier_nondigit / digit )*
identifier_nondigit
  = nondigit
  / universal_character_name
nondigit
  = "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m"
  / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z"
  / "A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M"
  / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "_"
digit
  = "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
/* preprocessing_op_or_punc
  = "{" / "}" / "[" / "]" / "#" / "##" / "(" / ")"
  / "<:" / ":>" / "<%" / "%>" / "%:" / "%:%:" / ";" / ":" / "..."
  / "new" / "delete" / "?" / "::" / "." / ".*"
  / "+" / "-" / "*" / "/" / "%" / "^" / "&" / "|" / "~"
  / "!" / "=" / "<" / ">" / "+=" / "-=" / "*=" / "/=" / "%="
  / "^=" / "&=" / "|=" / "<<" / ">>" / ">>=" / "<<=" / "==" / "!="
  / "<=" / ">=" / "&&" / "||" / "++" / "--" / "," / "->*" / "->"
  / "and" / "and_eq" / "bitand" / "bitor" / "compl" / "not" / "not_eq"
  / "or" / "or_eq" / "xor" / "xor_eq" */
literal
  = integer_literal
  / character_literal
  / floating_literal
  / string_literal
  / boolean_literal
  / pointer_literal
  / user_defined_literal
integer_literal
  = binary_literal /* integer_suffix? */
  / octal_literal /* integer_suffix? */
  / decimal_literal /* integer_suffix? */
  / hexadecimal_literal /* integer_suffix? */
binary_literal
  = ("0b" 
  / "0B") binary_digit ( "'"? binary_digit )*
octal_literal
  = "0" ( octal_literal "'"? octal_digit )*
decimal_literal
  = nonzero_digit ( "'"? digit )*
hexadecimal_literal
  = hexadecimal_prefix hexadecimal_digit_sequence
binary_digit
  = "0"
  / "1"
octal_digit
  = "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7"
nonzero_digit
  = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
hexadecimal_prefix
  = "0x" / "0X"
hexadecimal_digit_sequence
  = hexadecimal_digit ( "'"? hexadecimal_digit )*
hexadecimal_digit
  = "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"
  / "a" / "b" / "c" / "d" / "e" / "f"
  / "A" / "B" / "C" / "D" / "E" / "F"
/* integer_suffix
  = unsigned_suffix long_suffix?
  / unsigned_suffix long_long_suffix?
  / long_suffix unsigned_suffix?
  / long_long_suffix unsigned_suffix? */
/* unsigned_suffix
  = "u" / "U" */
/* long_suffix
  = "l" / "L" */
/* long_long_suffix
  = "ll" / "LL" */
character_literal
  = /* encoding_prefix? */ "'" c_char_sequence "'"
/* encoding_prefix
  = "u8" / "u" / "U" / "L" */
c_char_sequence
  = c_char ( c_char )*
c_char
  = [^'\\\n]
  / escape_sequence
  / universal_character_name
escape_sequence
  = simple_escape_sequence
  / octal_escape_sequence
  / hexadecimal_escape_sequence
simple_escape_sequence
  = "\'" / "\"" / "\?" / "\\"
  / "\a" / "\b" / "\f" / "\n" / "\r" / "\t" / "\v"
octal_escape_sequence
  = "\\" octal_digit
  / "\\" octal_digit octal_digit
  / "\\" octal_digit octal_digit octal_digit
hexadecimal_escape_sequence
  = "\\x" hexadecimal_digit ( hexadecimal_digit )*
floating_literal
  = decimal_floating_literal
  / hexadecimal_floating_literal
decimal_floating_literal
  = fractional_constant exponent_part? /* floating_suffix? */
  / digit_sequence exponent_part /* floating_suffix? */
hexadecimal_floating_literal
  = hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part /* floating_suffix? */
  / hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part /* floating_suffix? */
fractional_constant
  = digit_sequence? "." digit_sequence
  / digit_sequence "."
hexadecimal_fractional_constant
  = hexadecimal_digit_sequence? "." hexadecimal_digit_sequence
  / hexadecimal_digit_sequence "."
exponent_part
  = "e" sign? digit_sequence
  / "E" sign? digit_sequence
binary_exponent_part
  = "p" sign? digit_sequence
  / "P" sign? digit_sequence
sign
  = "+" / "-"
digit_sequence
  = digit ( "'"? digit )*
/* floating_suffix
  = "f" / "l" / "F" / "L" */
string_literal
  = /* encoding_prefix? */ '"' s_char_sequence? '"'
  / /* encoding_prefix? */ "R" raw_string
s_char_sequence
  = s_char ( s_char )*
s_char
  = [^"\\\n]
  / escape_sequence
  / universal_character_name
raw_string
  = '"' d_char_sequence? "(" r_char_sequence? ")" d_char_sequence? '"'
r_char_sequence
  = r_char ( r_char )*
r_char
  = . //except ")" d_char_sequence? '"'
d_char_sequence
  = d_char ( d_char )*
d_char
  = [^ ()\\\t\v\f\n]
boolean_literal
  = "false"
  / "true"
pointer_literal
  = "nullptr"
user_defined_literal
  = user_defined_integer_literal
  / user_defined_floating_literal
  / user_defined_string_literal
  / user_defined_character_literal
user_defined_integer_literal
  = decimal_literal ud_suffix
  / octal_literal ud_suffix
  / hexadecimal_literal ud_suffix
  / binary_literal ud_suffix
user_defined_floating_literal
  = fractional_constant exponent_part? ud_suffix
  / digit_sequence exponent_part ud_suffix
  / hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part ud_suffix
  / hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part ud_suffix
user_defined_string_literal
  = string_literal ud_suffix
user_defined_character_literal
  = character_literal ud_suffix
ud_suffix
  = identifier
primary_expression
  = literal
  / "this"
  / "(" expression ")"
  / id_expression
  / lambda_expression
  / fold_expression
id_expression
  = unqualified_id
  / qualified_id
unqualified_id
  = identifier
//  / operator_function_id
//  / conversion_function_id
//  / literal_operator_id
//  / "~" class_name
//  / "~" decltype_specifier
//  / template_id
qualified_id
  = nested_name_specifier "template"? unqualified_id
nested_name_specifier
  = ( "::"
  / /* type_name */identifier "::"
  / /* namespace_name */identifier "::"
  / /* decltype_specifier */identifier "::" ) (
    identifier "::"
  /  "template"? /* simple_template_id */identifier "::" )*
lambda_expression
  = lambda_introducer lambda_declarator? /*compound_statement*/
lambda_introducer
  = "[" lambda_capture? "]"
lambda_declarator
  = "(" /*parameter_declaration_clause*/ ")" /*decl_specifier_seq?*/
//  / noexcept_specifier? attribute_specifier_seq? trailing_return_type?
lambda_capture
  = capture_default
  / capture_list
  / capture_default "," capture_list
capture_default
  = "&"
  / "="
capture_list
  = capture "..."? ( "," capture "..."? )*
capture
  = simple_capture
  / init_capture
simple_capture
  = identifier
  / "&" identifier
  / "this"
  / "*" "this"
init_capture
  = identifier /*initializer*/
  / "&" identifier /*initializer*/
fold_expression
  = "(" cast_expression fold_operator "..." ")"
  / "(" "..." fold_operator cast_expression ")"
  / "(" cast_expression fold_operator "..." fold_operator cast_expression ")"
fold_operator
  = "+" / "-" / "*" / "/" / "%" / "^" / "&" / "|" / "<<" / ">>"
  / "+=" / "-=" / "*=" / "/=" / "%=" / "^=" / "&=" / "|=" / "<<=" / ">>=" / "="
  / "==" / "!=" / "<" / ">" / "<=" / ">=" / "&&" / "||" / "," / ".*" / "->*"
postfix_expression
  = ( primary_expression
  / /*simple_type_specifier*/ "(" expression_list? ")"
  / /*typename_specifier*/ "(" expression_list? ")"
//  / simple_type_specifier braced_init_list
//  / typename_specifier braced_init_list
  / "dynamic_cast" "<" /*type_id*/ ">" "(" expression ")"
  / "static_cast" "<" /*type_id*/ ">" "(" expression ")"
  / "reinterpret_cast" "<" /*type_id*/ ">" "(" expression ")"
  / "const_cast" "<" /*type_id*/ ">" "(" expression ")"
  / "typeid" "(" expression ")"
  / "typeid" "(" /*type_id*/ ")" ) (
    "[" /*expr_or_braced_init_list*/ "]"
  /  "(" expression_list? ")"
  /  "." "template"? id_expression
  /  "->" "template"? id_expression
  /  "." pseudo_destructor_name
  /  "->" pseudo_destructor_name
  /  "++"
  /  "--" )*
expression_list
  = /*initializer_list*/something
pseudo_destructor_name
  = nested_name_specifier? /*type_name*/ "::" "~" /*type_name*/
  / nested_name_specifier "template" /*simple_template_id*/ "::" "~" /*type_name*/
  / "~" /*type_name*/
  / "~" /*decltype_specifier*/
unary_expression
  = postfix_expression
  / "++" cast_expression
  / "--" cast_expression
  / unary_operator cast_expression
  / "sizeof" unary_expression
  / "sizeof" "(" /*type_id*/ ")"
  / "sizeof" "..." "(" identifier ")"
  / "alignof" "(" /*type_id*/ ")"
  / noexcept_expression
  / new_expression
  / delete_expression
unary_operator
  = "*" / "&" / "+" / "-" / "!" / "~"
new_expression
  = "::"? "new" new_placement? new_type_id new_initializer?
  / "::"? "new" new_placement? "(" /*type_id*/ ")" new_initializer?
new_placement
  = "(" expression_list ")"
new_type_id
  = /*type_specifier_seq*/ new_declarator?
new_declarator
  = /*ptr_operator new_declarator?
  /*/ noptr_new_declarator
noptr_new_declarator
  = "[" expression "]" /*attribute_specifier_seq?*/ (
   noptr_new_declarator "[" constant_expression "]" /*attribute_specifier_seq?*/ )*
new_initializer
  = "(" expression_list? ")"
//  / braced_init_list
delete_expression
  = "::"? "delete" cast_expression
  / "::"? "delete" "[" "]" cast_expression
noexcept_expression
  = "noexcept" "(" expression ")"
cast_expression
  = unary_expression
  / "(" /*type_id*/ ")" cast_expression
pm_expression
  = cast_expression (
    ".*" cast_expression
  /  "->*" cast_expression )*
multiplicative_expression
  = pm_expression (
    "*" pm_expression
  /  "/" pm_expression
  /  "%" pm_expression )*
additive_expression
  = multiplicative_expression (
    "+" multiplicative_expression
  /  "-" multiplicative_expression )*
shift_expression
  = additive_expression (
    "<<" additive_expression
  /  ">>" additive_expression )*
relational_expression
  = shift_expression (
    "<" shift_expression
  /  ">" shift_expression
  /  "<=" shift_expression
  /  ">=" shift_expression )*
equality_expression
  = relational_expression (
    "==" relational_expression
  /  "!=" relational_expression )*
and_expression
  = equality_expression (
   "&" equality_expression )*
exclusive_or_expression
  = and_expression ( "^" and_expression )*
inclusive_or_expression
  = exclusive_or_expression ( "|" exclusive_or_expression )*
logical_and_expression
  = inclusive_or_expression ( "&&" inclusive_or_expression )*
logical_or_expression
  = logical_and_expression ( "||" logical_and_expression )*
conditional_expression
  = logical_or_expression
  / logical_or_expression "?" expression ":" assignment_expression
throw_expression
  = "throw" assignment_expression?
assignment_expression
  = conditional_expression
  / logical_or_expression assignment_operator /*initializer_clause*/
  / throw_expression
assignment_operator
  = "=" / "*=" / "/=" / "%=" / "+=" / "-=" / ">>=" / "<<=" / "&=" / "^=" / "|="
expression
  = assignment_expression ( "," assignment_expression )*
constant_expression
  = conditional_expression
something = "something"
