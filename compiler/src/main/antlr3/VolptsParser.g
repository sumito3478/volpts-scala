parser grammar VolptsParser;

options {
  tokenVocab = VolptsLexer;
  output = AST;
  backtrack = true;
}

@header {
  package volpts.compiler.antlr;
}

@members {
  ParserUtils util = new ParserUtils();
}

compilation_unit : (decl semi)+ ;

semi @init { util.promoteNEW_LINE(retval, input); } : SEMICOLON | EOF | NEW_LINE ;

type_generic_raw : BACK_QUOTE ID ;

type_generic : LPAREN type_generic RPAREN | type_generic_raw ;

type_args : LBRACKET (type_generic_raw EQUAL)? type (COMMA (type_generic_raw EQUAL)? type)* RBRACKET ;

type_app_raw : qual_id type_args? ;

type_app : LPAREN type_app RPAREN | type_app_raw ;

type_simple : type_app | type_generic ;

type_fun_raw : (LPAREN type_simple (COMMA type_simple)+ RPAREN | type_simple) ARROW type ;

type_fun : LPAREN type_fun RPAREN | type_fun_raw ;

type : type_fun | type_simple ;

type_params : LBRACKET type_generic_raw (COMMA type_generic_raw)* RBRACKET ;

type_annot : COLON type ;

adt_part : ID OF type semi ;

gadt_part : ID COLON type_fun_raw semi ;

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

lambda_expr :  (LPAREN ID (COMMA ID)+ RPAREN | ID) ARROW expr;

qual_id : ID (DOT ID)* ;

app_expr : (LPAREN expr RPAREN | qual_id) LPAREN (ID EQUAL)? expr (COMMA (ID EQUAL)? expr)* RPAREN ;

match_part : CASE pat (IF expr)? DOUBLE_ARROW expr ;

match_expr : MATCH expr LCBRACKET match_part+ RCBRACKET ;

record_expr : RECORD LCBRACKET (ID EQUAL expr semi)+ RCBRACKET ;

if_expr : IF LPAREN expr RPAREN expr ELSE expr ;

compound_expr : LCBRACKET (expr semi)+ RCBRACKET ;

expr_raw : lambda_expr | app_expr | qual_id | match_expr | compound_expr | let_rec_expr | let_expr | literal | if_expr | type_expr | import_expr | record_expr ;

expr : LPAREN expr RPAREN | expr_raw ;

ident_pat : ID type_annot?;

unapply_pat : qual_id LPAREN pat (COMMA pat)* RPAREN ;

literal_pat : literal ;

pat : unapply_pat | ident_pat | literal_pat ;
