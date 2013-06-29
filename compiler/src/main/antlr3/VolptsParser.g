parser grammar VolptsParser;

options {
  tokenVocab = VolptsLexer;
  output = AST;
}

@header {
  package volpts.compiler.antlr;
}

@members {
  ParserUtils util = new ParserUtils();
}

compilation_unit : (decl semi)+ ;

semi @init { util.promoteNEW_LINE(retval, input); } : SEMICOLON | EOF | NEW_LINE ;

type_generic : BACK_QUOTE ID ;

type_args : LBRACKET (type_generic EQUAL)? type (COMMA (type_generic EQUAL)? type)* RBRACKET ;

type_app : qual_id type_args? ;

type_simple : type_generic | type_app ;

type_fun_multiple : LPAREN type_simple (COMMA type_simple)+ RPAREN ARROW type ;

type_fun_single : type_simple ARROW type ;

type : type_fun_multiple | (type_simple ARROW) => type_fun_single | type_simple ;

type_params : LBRACKET type_generic (COMMA type_generic)* RBRACKET ;

type_annot : COLON type ;

adt_part : ID OF type semi ;

gadt_part : ID COLON (type_fun_single | type_fun_multiple) semi ;

variant : VARIANT LCBRACKET (gadt_part+ | adt_part+) RCBRACKET ;

record : RECORD LCBRACKET (ID COLON type semi)+ RCBRACKET ;

type_decl : TYPE ID type_params? EQUAL (record | variant | type);

val_decl : VAL ID type_annot EQUAL expr ;

import_decl : IMPORT qual_id (AS ID)? ;

decl : type_decl | val_decl | import_decl ;

integer_literal : MINUS? INTEGER_LITERAL ;

floating_point_literal : MINUS? FLOATING_POINT_LITERAL ;

boolean_literal : TRUE | FALSE;

string_literal : STRING_LITERAL ;

literal : integer_literal | floating_point_literal | boolean_literal | string_literal ;

type_expr : type_decl semi expr ;

import_expr : import_decl semi expr ;

let_expr : LET ID type_annot? EQUAL expr semi expr ;

let_rec_expr : LET REC ID type_annot? EQUAL expr semi expr ;

lambda_expr_single : ID ARROW expr ;

lambda_expr_multiple : LPAREN ID (COMMA ID)+ RPAREN ARROW expr ;

qual_id : ID (DOT ID)* ;

qual_expr : qual_id ;

id_expr : ID ; // qual_expr does not match single ID if the next 'semi' is implicit (i.e. promoted NEW_LINE)...

app_expr : (LPAREN expr RPAREN | qual_id) LPAREN (ID EQUAL)? expr (COMMA (ID EQUAL)? expr)* RPAREN ;

match_part : CASE pat (IF expr)? DOUBLE_ARROW expr ;

match_expr : MATCH expr LCBRACKET match_part+ RCBRACKET ;

record_expr : RECORD LCBRACKET (ID EQUAL expr semi)+ RCBRACKET ;

if_expr : IF LPAREN expr RPAREN expr ELSE expr ;

compound_expr : LCBRACKET (expr semi)+ RCBRACKET ;

expr_raw : lambda_expr_multiple | (ID ARROW) => lambda_expr_single | app_expr | (ID DOT) => qual_expr | id_expr | match_expr | compound_expr | let_rec_expr | let_expr | literal | if_expr | type_expr | import_expr | record_expr ;

expr : (LPAREN ID COMMA) => expr_raw | (LPAREN) => LPAREN expr RPAREN | expr_raw ;

ident_pat : ID type_annot?;

unapply_pat : qual_id LPAREN pat (COMMA pat)* RPAREN ;

literal_pat : literal ;

pat : unapply_pat | ident_pat | literal_pat ;
