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

compilation_unit : (decl semi!)+ ;

semi @init { util.promoteNEW_LINE(retval, input); } : SEMICOLON! | EOF! | NEW_LINE! ;

type_generic : BACK_QUOTE ID -> ^(TYPE_GENERIC ID) ;

type_args : LBRACKET! type (COMMA! type)* RBRACKET! ;

type_app : qual_id type_args? -> ^(TYPE_APP qual_id type_args?);

type_simple : type_generic | type_app ;

type_fun_params : LPAREN! type (COMMA! type)+ RPAREN! ;

type_fun_multiple : type_fun_params ARROW type -> ^(TYPE_FUNCTION type_fun_params type) ;

type_fun_single : type_simple ARROW type -> ^(TYPE_FUNCTION type_simple type) ;

type_fun : type_fun_multiple | type_fun_single ;

type : type_fun_multiple | (type_simple ARROW) => type_fun_single | type_simple ;

type_params : LBRACKET type_generic (COMMA type_generic)* RBRACKET -> ^(TYPE_PARAMS type_generic+) ;

type_annot : COLON! type ;

adt_part : ID OF type semi -> ^(ADT_PART ID type) ;

gadt_part : ID COLON type_fun semi -> ^(GADT_PART ID type_fun);

variant_parts : adt_part+ | gadt_part+ ;

variant : VARIANT^ LCBRACKET! variant_parts RCBRACKET! ;

record_part : ID COLON type semi -> ^(RECORD_PART ID type);

record : RECORD^ LCBRACKET! record_part+ RCBRACKET! ;

type_def : record | variant | type ;

type_decl : TYPE^ ID type_params? EQUAL! type_def;

val_decl : VAL? ID type_annot EQUAL! expr ;

import_decl : IMPORT qual_id (AS ID)? -> ^(IMPORT_DECL qual_id ID?);

decl_raw : type_decl | val_decl | import_decl ;

decl : decl_raw -> ^(DECL decl_raw) ;

integer_literal : MINUS? INTEGER_LITERAL -> ^(INTEGER MINUS? INTEGER_LITERAL) ;

floating_point_literal : MINUS? FLOATING_POINT_LITERAL -> ^(FLOATING_POINT MINUS? FLOATING_POINT_LITERAL);

boolean_part : TRUE | FALSE ;

boolean_literal : boolean_part -> ^(BOOLEAN boolean_part) ;

string_literal : STRING_LITERAL -> ^(STRING STRING_LITERAL) ;

literal : integer_literal | floating_point_literal | boolean_literal | string_literal ;

literal_expr : literal -> ^(LITERAL_EXPR literal) ;

type_expr : type_decl semi expr -> ^(TYPE_EXPR type_decl expr);

import_expr : import_decl semi expr -> ^(IMPORT_EXPR import_decl expr);

let_expr : LET^ ID type_annot? EQUAL! expr semi! expr;

let_rec_expr : LET REC ID type_annot? EQUAL expr semi expr -> ^(LETREC_EXPR ID type_annot? expr expr) ;

lambda_expr_single : ID ARROW expr -> ^(LAMBDA_EXPR ID expr);

lambda_expr_multiple : LPAREN ID (COMMA ID)+ RPAREN ARROW expr -> ^(LAMBDA_EXPR ID+ expr) ;

qual_id : ID (DOT ID)* -> ^(QUAL_ID ID+) ;

qual_expr : qual_id -> ^(ID_EXPR qual_id) ;

id_expr : ID -> ^(ID_EXPR ID); // qual_expr does not match single ID if the next 'semi' is implicit (i.e. promoted NEW_LINE)...

app_arg : (ID EQUAL)? expr -> ^(APP_ARG ID? expr);

app_expr : qual_expr LPAREN app_arg (COMMA app_arg)* RPAREN -> ^(APP_EXPR qual_expr app_arg+) ;

match_guard : IF^ expr ;

match_part : CASE^ pat match_guard? DOUBLE_ARROW! expr ;

match_expr : MATCH^ expr LCBRACKET! match_part+ RCBRACKET! ;

record_expr_part : ID EQUAL expr semi -> ^(RECORD_EXPR_PART ID expr);

record_expr : RECORD LCBRACKET record_expr_part+ RCBRACKET -> ^(RECORD_EXPR record_expr_part+);

if_expr : IF^ LPAREN! expr RPAREN! expr ELSE! expr ;

compound_expr : LCBRACKET (expr semi)+ RCBRACKET -> ^(COMPOUND_EXPR expr+);

expr_raw : lambda_expr_multiple | (ID ARROW) => lambda_expr_single | app_expr | (ID DOT) => qual_expr | id_expr | match_expr | compound_expr | let_rec_expr | let_expr | literal | if_expr | type_expr | import_expr | record_expr ;

expr : (LPAREN ID COMMA) => expr_raw | (LPAREN) => LPAREN expr RPAREN | expr_raw ;

ident_pat : ID type_annot? -> ^(IDENT_PAT ID type_annot?);

unapply_pat : qual_id LPAREN pat (COMMA pat)* RPAREN -> ^(UNAPPLY_PAT qual_id pat+);

literal_pat : literal -> ^(LITERAL_PAT literal);

pat : unapply_pat | ident_pat | literal_pat ;
