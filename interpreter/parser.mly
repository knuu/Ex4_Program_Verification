%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV MOD
%token LT GT LTE GTE NE NOT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET IN EQ
%token RARROW FUN
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }

Expr :
    IfExpr { $1 }
  | ORExpr { $1 }
  | LetExpr { $1 }
  | REExpr { $1 }
  | FunExpr { $1 }
  | LetRecExpr { $1 }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) }

LetRecExpr :
    LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }

ORExpr :
    ORExpr OR ANDExpr { BinOp (Or, $1, $3) }
  | ANDExpr { $1 }

ANDExpr :
    ANDExpr AND NOTExpr { BinOp (And, $1, $3) }
  | NOTExpr { $1 }

NOTExpr :
    NOT NOTExpr { UnaryOp (Not, $2) }
  | REExpr { $1 }

REExpr : 
    REExpr LT PExpr { BinOp (Lt, $1, $3) }
  | REExpr GT PExpr { BinOp (Gt, $1, $3) }
  | REExpr LTE PExpr { BinOp (Lte, $1, $3) }
  | REExpr GTE PExpr { BinOp (Gte, $1, $3) }
  | REExpr EQ PExpr { BinOp (Eq, $1, $3) }
  | REExpr NE PExpr { BinOp (Ne, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | PExpr MINUS MExpr { BinOp (Minus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT UMExpr { BinOp (Mult, $1, $3) }
  | MExpr DIV UMExpr { BinOp (Div, $1, $3) }
  | MExpr MOD UMExpr { BinOp (Mod, $1, $3) }
  | UMExpr { $1 }

UMExpr :
    MINUS UMExpr { UnaryOp (UMinus, $2) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

   
