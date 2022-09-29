/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_hw_common.h" //Extern variables that communicate with lex
    #include "string.h"
    #define YYDEBUG 1
    int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    typedef struct {
        int Scope;
        int Index;
        char Name[10];
        char Type[10];
        int Address;
        int Lineno;
        char ele_type[10];
    } entry;

    entry table[100];
    int scope_level = 0;
    int id = 0;
    int addr_ct = 0;
    char found_type[10];
    char prev_type[10];
    int hold_f = 1;
    int differ_f = 0;
    int non_bool_f = 0;
    int isFound = 0;

    int yylex_destroy ();
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static void insert_symbol(char* name, char* type, int line, char* var_type);
    static void lookup_symbol(char* name);
    static void dump_symbol();

    /* Global variables */
    bool HAS_ERROR = false;
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    char *id_name;
    /* ... */
}

/* Token without return */
%token ADD SUB MUL QUO REM
%token INC DEC
%token GTR LSS GEQ LEQ EQL NEQ
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token AND OR NOT
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMICOLON COLON COMMA NEWLINE
%token PRINT PRINTLN
%token IF ELSE FOR SWITCH CASE
%token TRUE FALSE
%token RETURN DEFAULT
%token PACKAGE FUNC COMMENT

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT VAR INT FLOAT STRING BOOL IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type
%type <s_val> Expression
%type <s_val> andExpr
%type <s_val> cmpExpr
%type <s_val> addExpr
%type <s_val> mulExpr
%type <s_val> UnaryExpr
%type <s_val> PrimaryExpr
%type <s_val> cmp_op
%type <s_val> add_op
%type <s_val> mul_op
%type <s_val> unary_op
%type <s_val> assign_op
%type <s_val> Operand
%type <s_val> IndexExpr

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : GlobalStatementList
;

Block
    : LBRACEexpr StatementList RBRACE { dump_symbol(); }
;

LBRACEexpr
    : LBRACE { create_symbol(); }
;

GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement
;

GlobalStatement
    : PackageStmt NEWLINE
    | FunctionDeclStmt
    | NEWLINE
;

PackageStmt
    : PACKAGE IDENT
;

FunctionDeclStmt
    : FuncOpen LPAREN RPAREN ReturnType FuncBlock
    | FuncOpen LPAREN ParameterList RPAREN ReturnType FuncBlock
;

FuncOpen
    : FUNC IDENT
;

ReturnType
    : RETURN
    | RETURN Type
;

ParameterList
    : IDENT Type
    | ParameterList COMMA IDENT Type
;

FuncBlock
    : LBRACEexpr StatementList RBRACE { dump_symbol(); }
;

StatementList
    : Statement
    | Statement StatementList
;

Type
    : INT { $$ = "int32"; }
    | FLOAT { $$ = "float32"; }
    | STRING { $$ = "string"; }
    | BOOL { $$ = "bool"; }
;

Literal
    : INT_LIT {
        printf("INT_LIT %d\n", $<i_val>$);
        if(hold_f) {
            if(strcmp("int",found_type) == 0){
                differ_f = 0;
            }
            else{
                differ_f = 1;
                strncpy(prev_type,found_type,10);
            }
            strncpy(found_type, "int", 10);
        }
    }
    | FLOAT_LIT {
        printf("FLOAT_LIT %f\n", $<f_val>$);
        if(hold_f){
            if(strcmp("float",found_type) == 0){
                differ_f = 0;
            }
            else{
                differ_f = 1;
                strncpy(prev_type,found_type,10);
            }
            strncpy(found_type, "float", 10);
        }
    }
    | TRUE {
        printf("TRUE\n");
        if(strcmp("bool",found_type) == 0){
            differ_f = 0;
        }
        else{
            differ_f = 1;
            strncpy(prev_type,found_type,10);
        }
        strncpy(found_type, "bool", 10);
    }
    | FALSE {
        printf("FALSE\n");
        if(strcmp("bool",found_type) == 0){
            differ_f = 0;
        }
        else{
            differ_f = 1;
            strncpy(prev_type,found_type,10);
        }
        strncpy(found_type, "bool", 10);
    }
    | STRING_LIT {
        printf("STRING_LIT %s\n", $<s_val>$);
        if(strcmp("string",found_type) == 0){
            differ_f = 0;
        }
        else{
            differ_f = 1;
            strncpy(prev_type,found_type,10);
        }
        strncpy(found_type, "string", 10);
    }
;

Statement
    : DeclarationStmt NEWLINE
    | SimpleStmt NEWLINE
    | Block
    | IfStmt
    | ForStmt
    | SwitchStmt
    | CaseStmt
    | PrintStmt NEWLINE
    | ReturnStmt NEWLINE
    | NEWLINE
;

DeclarationStmt
    : VAR IDENT Type { insert_symbol($2, $1, yylineno ,"-"); }
    | VAR IDENT Type ASSIGN Expression { 
        insert_symbol($2, $1, yylineno ,"-"); 
    }
;

Expression 
    : Expression OR andExpr { 
        char tp[100];
        if(strcmp(found_type, "bool") != 0){
            sprintf(tp,"invalid operation: (operator OR not defined on %s)", found_type);
            yyerror(tp);
        }
        else if(differ_f){
            if(strcmp(prev_type, "bool") != 0)
                sprintf(tp,"invalid operation: (operator OR not defined on %s)", prev_type);
            yyerror(tp);
        }  
        printf("OR\n");
    }
    | andExpr { $$ = $1; }
;

andExpr
    : andExpr AND cmpExpr {
        char tp[100];
        if(strcmp(found_type, "bool") != 0){
            sprintf(tp,"invalid operation: (operator AND not defined on %s)", found_type);
            yyerror(tp);
        }
        else if(differ_f){
            if(strcmp(prev_type, "bool") != 0)
                sprintf(tp,"invalid operation: (operator AND not defined on %s)", prev_type);
            yyerror(tp);
        }  
        printf("AND\n"); 
    }
    | cmpExpr 
;

cmpExpr
    : cmpExpr cmp_op addExpr { printf("%s\n", $2); strncpy(found_type, "bool", 10);}
    | addExpr
;

addExpr
    : addExpr add_op mulExpr { 
        char tp[100];
        if(differ_f){
            sprintf(tp,"invalid operation: %s (mismatched types %s and %s)", $2, prev_type, found_type);
            yyerror(tp);
        }
        printf("%s\n", $2); 
    }
    | mulExpr
;

mulExpr
    : mulExpr mul_op UnaryExpr {
        char tp[100];
        if(strcmp($2, "REM") == 0 && strcmp(found_type, "int") != 0){
            sprintf(tp,"invalid operation: (operator %s not defined on %s)", $2, found_type);
            yyerror(tp);
        }
        else if(differ_f){
            if(strcmp($2, "REM") == 0 && strcmp(prev_type, "int") != 0)
                sprintf(tp,"invalid operation: (operator %s not defined on %s)", $2, prev_type);
            else
                sprintf(tp,"invalid operation: %s (mismatched types %s and %s)", $2, prev_type, found_type);
            yyerror(tp);
        } 
        printf("%s\n", $2); 
    }
    | UnaryExpr 
;

UnaryExpr  
    : PrimaryExpr 
    | unary_op UnaryExpr { printf("%s\n", $1); }
;

cmp_op
    : EQL { $$ =  "EQL"; }
    | NEQ { $$ =  "NEQ"; }
    | LSS { $$ =  "LSS"; }
    | LEQ { $$ =  "LEQ"; }
    | GTR { $$ =  "GTR"; }
    | GEQ { $$ =  "GEQ"; }
;

add_op
    : ADD { $$ = "ADD"; }
    | SUB { $$ = "SUB"; }
;

mul_op
    : MUL { $$ = "MUL"; }
    | QUO { $$ = "QUO"; }
    | REM { $$ = "REM"; }
;

unary_op
    : ADD { $$ = "POS"; }
    | SUB { $$ = "NEG"; }
    | NOT { $$ = "NOT"; }
;

PrimaryExpr
    : Operand
    | IndexExpr
    | ConversionExpr
;

Operand
    : Literal { $$ = "LIT"; }
    | IDENT { lookup_symbol($1); }
    | LPAREN Expression RPAREN
;

IndexExpr 
    : PrimaryExpr LBRACKexpr Expression RBRACKexpr
;

LBRACKexpr
    : LBRACK { hold_f = 0; }
;

RBRACKexpr
    : RBRACK { hold_f = 1; }
;

ConversionExpr
    : Type LPAREN Expression RPAREN {
        strncpy(found_type, $1,10);
        if(strcmp($1,prev_type) == 0){
            differ_f = 0;
        }
        else{
            differ_f = 1;
        }
    }
;

SimpleStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecStmt
;

AssignmentStmt
    : Expression assign_op Expression{
        char tp[100];
        if(strcmp($1,"LIT") == 0){
            sprintf(tp,"cannot assign to %s", prev_type);
            yyerror(tp);
        }
        else if(differ_f && isFound){
            sprintf(tp,"invalid operation: %s (mismatched types %s and %s)", $2, prev_type, found_type);
            yyerror(tp);
        }
        printf("%s\n", $2); 
    }
;

assign_op
    : ASSIGN { $$ = "ASSIGN" ; }
    | ADD_ASSIGN { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN { $$ = "REM_ASSIGN"; }
;

IncDecStmt
    : Expression INC { printf("INC\n"); }
    | Expression DEC { printf("DEC\n"); }
;

ExpressionStmt
    : Expression
;

IfStmt
    : IF Condition Block
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;

Condition
    : Expression {
        char tp[100];
        if(strcmp(found_type,"bool") != 0){
            sprintf(tp,"non-bool (type %s) used as for condition", found_type);
            non_bool_f = 1;
            yyerror(tp);
        }
    }
;

ForStmt   
    : FOR LPAREN Condition RPAREN Block
    | FOR LPAREN ForClause RPAREN Block
;

ForClause
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt
;

InitStmt
    : SimpleStmt
;

PostStmt
    : SimpleStmt
;

PrintStmt 
    : PRINT LPAREN Expression RPAREN SEMICOLON {
        if($3)
            printf("PRINT %s\n", found_type);
    }
;

ReturnStmt
    : RETURN
    | RETURN Type
;

SwitchStmt
    : SWITCH Expression Block
;

CaseStmt
    : CASE INT_LIT COLON Block
    | DEFAULT COLON Block
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 0;
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    printf("> Create symbol table (scope level %d)\n", scope_level);
    scope_level++;
    id = 0;
}

static void insert_symbol(char* name, char* type, int line, char* var_type) {table[addr_ct].Scope = scope_level;
    table[addr_ct].Index = id++;
    strncpy(table[addr_ct].Name, name, 10);
    strncpy(table[addr_ct].Type, type, 10);
    table[addr_ct].Address = addr_ct;
    table[addr_ct].Lineno = line;
    if(var_type)
        strncpy(table[addr_ct].ele_type, var_type, 10);
    else
        strncpy(table[addr_ct].ele_type, "-", 10);
    addr_ct++;
            
    printf("> Insert `%s` (addr: %d) to scope level %d\n", name, addr_ct, scope_level);
}

static void lookup_symbol(char* name) {
    char tp[100];
        isFound = 0;
        for(int scp = scope_level; scp > -1; scp--){
            for(int i = 0; i <= addr_ct; i++){
                if(strcmp(table[i].Name, name) == 0 && table[i].Scope == scp){
                    printf("IDENT (name=%s, address=%d)\n", name, i);
                    if(strcmp(table[i].ele_type,"-") != 0)
                        strncpy(found_type,table[i].ele_type,10);
                    else{
                        if(strcmp(table[i].Type,found_type) == 0){
                            differ_f = 0;
                            strncpy(prev_type,found_type,10);
                        }
                        else{
                            differ_f = 1;
                            strncpy(prev_type,found_type,10);
                        }
                        strncpy(found_type,table[i].Type,10);
                    }
                    scp = 0;
                    isFound = 1;
                    break;
                }
            }
        }
        if(!isFound){
            sprintf(tp, "undefined: %s", name);
            differ_f = 0;
            yyerror(tp);
        }
}

static void dump_symbol() {
    printf("\n> Dump symbol table (scope level: %d)\n", scope_level);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s\n",
           "Index", "Name", "Type", "Addr", "Lineno", "Func_sig");
    int i = addr_ct;
    id = 0;
    for(i = 0; i < addr_ct; i++) {
        if(table[i].Scope == scope_level) {
            printf("%-10d%-10s%-10s%-10d%-10d%-10s\n",
                    table[i].Index, table[i].Name, table[i].Type, i, table[i].Lineno, table[i].ele_type); // table[i].ele_type -> func_sig
            table[i].Scope = -2;
        }
        else if(table[i].Scope == scope_level-1) {
            id = table[i].Index + 1;
        }
    }
    scope_level--;
}