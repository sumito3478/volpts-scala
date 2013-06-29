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

type_generic : BACK_QUOTE ID ;

type_args : LBRACKET (type_generic EQUAL)? type (COMMA (type_generic EQUAL)? type)* RBRACKET ;

type_app : qual_id type_args? ;

type_part : LPAREN type (COMMA type)* RPAREN ;

type : type_part (ARROW type_part)* | type_app | type_generic ;

type_params : LBRACKET type_generic RBRACKET ;

type_annot : COLON type ;

integer_literal : MINUS? INTEGER_LITERAL ;

floating_point_literal : MINUS? FLOATING_POINT_LITERAL ;

boolean_literal : TRUE | FALSE;

string_literal : STRING_LITERAL ;

literal : integer_literal | floating_point_literal | boolean_literal | string_literal ;

constr : ID (LPAREN (type (COMMA type)* | ID COLON type (COMMA ID COLON type)*)? RPAREN)? ;

data_expr : DATA ID type_params? EQUAL constr (OR constr)* semi expr ;

val_expr : VAL ident_pat EQUAL expr semi expr ;

let_expr : LET pat EQUAL expr semi expr ;

let_rec_expr : LET REC pat EQUAL expr semi expr ;

tuple_expr : LPAREN expr (COMMA expr)* RPAREN ;

arg_part : ident_pat (COMMA arg_part)?;

lambda_expr : LPAREN pat (COMMA pat)* RPAREN ARROW expr;

qual_id : ID (DOT ID)* ;

app_expr : (qual_id | LPAREN expr RPAREN) LPAREN (ID EQUAL)? expr (COMMA (ID EQUAL)? expr)* RPAREN;

match_part : CASE pat (IF expr)? DOUBLE_ARROW expr ;

match_expr : MATCH expr LCBRACKET match_part ( match_part)* RCBRACKET ;

if_expr : IF LPAREN expr RPAREN expr ELSE expr ;

compound_expr : LCBRACKET expr (semi expr)* RCBRACKET ;

import_expr : IMPORT qual_id (AS ID)? semi expr;

expr : lambda_expr | tuple_expr | app_expr | match_expr | compound_expr | let_rec_expr | let_expr | qual_id | literal | if_expr | data_expr | val_expr | import_expr ;

ident_pat : ID type_annot?;

unapply_pat : qual_id LPAREN pat (COMMA pat)* RPAREN ;

literal_pat : literal ;

pat : unapply_pat | ident_pat | literal_pat ;
