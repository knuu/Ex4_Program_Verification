%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV MOD
%token LT GT LTE GTE NE NOT
%token IF THEN ELSE TRUE FALSE
%token LOGAND LOGOR
%token LET IN EQ
%token RARROW FUN DFUN
%token REC AND

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | DeclListExpr SEMISEMI { $1 }

DeclListExpr :
    DeclExpr DeclListExpr { DeclList ($1, $2) }
  | DeclExpr { $1 }

DeclExpr :
    LET ID EQ Expr { Decl ($2, $4) }
  | LET ID EQ Expr AND AndDeclExpr { AndDecl ($2, $4, $6) }
  | LET REC ID EQ FUN ID RARROW Expr { RecDecl ($3, $6, $8) }

AndDeclExpr :
    ID EQ Expr { Decl ($1, $3) }
  | ID EQ Expr AND AndDeclExpr { AndDecl ($1, $3, $5) }

Expr :
    IfExpr { $1 }
  | LogicalOrExpr { $1 }
  | LetExpr { $1 }
  | FunExpr { $1 }
  | LetRecExpr { $1 }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) } 
  | DFUN ID RARROW Expr { DFunExp ($2, $4) }

LetRecExpr :
    LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }

LogicalOrExpr :
    LogicalOrExpr LOGOR LogicalAndExpr { BinOp (Or, $1, $3) }
  | LogicalAndExpr { $1 }

LogicalAndExpr :
    LogicalAndExpr LOGAND NotExpr { BinOp (And, $1, $3) }
  | NotExpr { $1 }

NotExpr :
    NOT NotExpr { UnaryOp (Not, $2) }
  | ReExpr { $1 }

ReExpr : 
    ReExpr LT PExpr { BinOp (Lt, $1, $3) }
  | ReExpr GT PExpr { BinOp (Gt, $1, $3) }
  | ReExpr LTE PExpr { BinOp (Lte, $1, $3) }
  | ReExpr GTE PExpr { BinOp (Gte, $1, $3) }
  | ReExpr EQ PExpr { BinOp (Eq, $1, $3) }
  | ReExpr NE PExpr { BinOp (Ne, $1, $3) }
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

   
