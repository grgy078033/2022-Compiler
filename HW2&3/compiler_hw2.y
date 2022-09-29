/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #include "string.h"
    // #define YYDEBUG 1
    // int yydebug = 1;

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
    int scope = 0, id = 0,addr_ct = 0;
    char found_type[10], prev_type[10];
    int hold_f = 1, differ_f = 0, non_bool_f = 0;
    int isFound = 0;

    void yyerror (char const *s)
    {
        if(non_bool_f){
            printf("error:%d: %s\n", yylineno+1, s);
            non_bool_f = 0;
        }
        else
            printf("error:%d: %s\n", yylineno, s);
    }


    /* Symbol table function - you can add new function if needed. */
    static void create_symbol()
    {
        scope++;
        id = 0;
    }
    static void insert_symbol(char* name, char* type, int line, char* var_type)
    {
        char tp[100];
        int i;
        for(i=0;i<addr_ct;i++){
            if(strcmp(table[i].Name, name) == 0 && table[i].Scope == scope){
                sprintf(tp, "%s redeclared in this block. previous declaration at line %d", name, table[i].Lineno);
                yyerror(tp);
                i = -1;
                break;
            }
        }
        if(i != -1){
            table[addr_ct].Scope = scope;
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
            
            printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
        }
    }
    static void lookup_symbol(char* name)
    {
        char tp[100];
        isFound = 0;
        for(int scp = scope;scp>-1;scp--){
            for(int i=0;i<=addr_ct;i++){
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
    static void dump_symbol()
    {
        printf("> Dump symbol table (scope level: %d)\n", scope);
        printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno","Element type");
        int i = addr_ct;
        id = 0;
        for(i = 0;i<addr_ct;i++){
            if(table[i].Scope == scope){
                printf("%-10d%-10s%-10s%-10d%-10d%s\n",table[i].Index, table[i].Name,table[i].Type,i, table[i].Lineno,table[i].ele_type);
                table[i].Scope = -2;
            }
            else if(table[i].Scope == scope-1){
                id = table[i].Index +1;
            }
        }
        scope--;
    }
%}



/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    char *id_name;
    /* ... */
}

/* Token without return */
%token INC DEC ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN NOT LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMICOLON COMMA QUOTA
%token COMMENT
%token PRINT RETURN IF ELSE FOR WHILE TRUE FALSE CONTINUE BREAK VOID

%token EQL NEQ LSS LEQ GTR GEQ
%token ADD SUB
%token AND OR 
%token MUL QUO REM 
/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT INT FLOAT BOOL STRING IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type TypeName Expression andExpr cmpExpr addExpr mulExpr cmp_op add_op mul_op unary_op assign_op Operand PrimaryExpr UnaryExpr




/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList { dump_symbol(); }
;

Block
    : LBRACEexpr StatementList RBRACE { dump_symbol(); }
;

LBRACEexpr
    : LBRACE { create_symbol(); }
;

StatementList
    : Statement 
    | Statement StatementList
;

Type
    : TypeName 
;

TypeName
    : INT   { $$ = "int";  }
    | FLOAT { $$ = "float"; }
    | STRING    { $$ = "string"; }
    | BOOL  { $$ = "bool"; }
;

Literal
    : INT_LIT {
        printf("INT_LIT %d\n", $<i_val>$);
        if(hold_f){
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
;

Statement
    : DeclarationStmt    
    | AssignmentStmt    
    | IncDecStmt    
    | ArithStmt
    | Block  {create_symbol();}
    | IfStmt    
    | WhileStmt    
    | ForStmt    
    | PrintStmt
;

DeclarationStmt 
    : Type IDENT SEMICOLON  { insert_symbol($2, $1, yylineno ,"-"); }
    | Type IDENT ASSIGN Expression SEMICOLON { 
        insert_symbol($2, $1, yylineno ,"-"); 
    }
    | Type IDENT LBRACK Expression RBRACK SEMICOLON { 
        insert_symbol($2, "array", yylineno ,$1);
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
    : ADD { $$ =  "ADD"; }
    | SUB { $$ =  "SUB"; }
;

mul_op     
    : MUL { $$ =  "MUL"; }
    | QUO { $$ =  "QUO"; } 
    | REM { $$ =  "REM"; }
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
    : LPAREN Type RPAREN Expression { 
        printf("%c to %c\n", found_type[0] + 'A'-'a', $2[0] + 'A' - 'a');
        strncpy(found_type, $2,10);
        if(strcmp($2,prev_type) == 0){
            differ_f = 0;
        }
        else{
            differ_f = 1;
        }
    }
;

AssignmentExpr 
    : Expression assign_op Expression { 
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

AssignmentStmt 
    : AssignmentExpr SEMICOLON
;

assign_op      
    : ASSIGN { $$ = "ASSIGN" ;}
    | ADD_ASSIGN { $$ = "ADD_ASSIGN" ;}
    | SUB_ASSIGN { $$ = "SUB_ASSIGN" ;}
    | MUL_ASSIGN { $$ = "MUL_ASSIGN" ;}
    | QUO_ASSIGN { $$ = "QUO_ASSIGN" ;}
    | REM_ASSIGN { $$ = "REM_ASSIGN" ;}
;

IncDecExpr 
    : Expression INC { printf("INC\n"); }
    | Expression DEC { printf("DEC\n"); }
;

IncDecStmt 
    : IncDecExpr SEMICOLON
;

ArithStmt
    : Expression SEMICOLON
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

WhileStmt 
    : WHILE LPAREN Condition RPAREN Block 
;

ForStmt   
    : FOR LPAREN ForClause RPAREN Block
;

ForClause 
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt
;

InitStmt  
    : SimpleExpr
;

PostStmt  
    : SimpleExpr
;

SimpleExpr 
    : AssignmentExpr 
    | Expression 
    | IncDecExpr
;

PrintStmt 
    : PRINT LPAREN Expression RPAREN SEMICOLON {
        if($3)
            printf("PRINT %s\n", found_type);
    }
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
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

