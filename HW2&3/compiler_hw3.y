/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

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
    
    /* Other global variables */
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    int INDENT = 0;

    entry table[100];
    int scope = 0, id = 0,addr_ct = 0;
    char found_type[10], prev_type[10];
    int hold_f = 1, differ_f = 0, non_bool_f = 0;
    int isFound = 0;

    int label_ct = 0, loop_label_ct = 0, if_label_ct = 0;
    int last_var_addr = 0;
    int last_assign_addr = 0;
    int is_assign = 0;
    int is_array = 0;
    int loop_ct = 0;
    int if_ct = 0;
    int isloop = 0;
    int isfound_arr = 0;
    char arr_type[10];
    int is_for = 0;
    char post_stmt[100];

    void yyerror (char const *s)
    {
        if(non_bool_f){
            printf("error:%d: %s\n", yylineno+1, s);
            non_bool_f = 0;
        }
        else
            printf("error:%d: %s\n", yylineno, s);
        
        remove("hw3.j");
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
            last_var_addr = addr_ct;
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
            
            //printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
        }
    }
    static void lookup_symbol(char* name)
    {
        char tp[100];
        isFound = 0;
        for(int scp = scope;scp>-1;scp--){
            for(int i=0;i<=addr_ct;i++){
                if(strcmp(table[i].Name, name) == 0 && table[i].Scope == scp){
                    //printf("IDENT (name=%s, address=%d)\n", name, i);
                    
                    if(strcmp(table[i].ele_type,"-") != 0){
                        strncpy(found_type,table[i].ele_type,10);
                        isfound_arr = 1;
                    }
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
                    if(!is_assign)
                        last_assign_addr = i;
                    else{
                        last_var_addr = i;
                        if(strcmp(found_type, "int") == 0 || strcmp(found_type, "bool") == 0) 
                            ;//printf("iload %d\n", i);
                        else if(strcmp(found_type, "float") == 0)
                            ;//printf("fload %d\n", i);
                    }
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
        //printf("> Dump symbol table (scope level: %d)\n", scope);
        //printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno","Element type");
        int i = addr_ct;
        id = 0;
        for(i = 0;i<addr_ct;i++){
            if(table[i].Scope == scope){
                //printf("%-10d%-10s%-10s%-10d%-10d%s\n",table[i].Index, table[i].Name,table[i].Type,i, table[i].Lineno,table[i].ele_type);
                table[i].Scope = -2;
            }
            else if(table[i].Scope == scope-1){
                id = table[i].Index +1;
            }
        }
        scope--;
    }
%}

%error-verbose

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
%type <s_val> Type TypeName Expression andExpr cmpExpr addExpr mulExpr cmp_op add_op mul_op unary_op assign_op Operand PrimaryExpr UnaryExpr PostStmt
/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList { dump_symbol(); }
;

Block
    : LBRACEexpr StatementList RBRACE { dump_symbol();}
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
        codegen("ldc %d\n", $<i_val>$);
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
        codegen("ldc %f\n", $<f_val>$);
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
        codegen("ldc \"%s\"\n", $<s_val>$);
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
        codegen("iconst_1\n");
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
        codegen("iconst_0\n");
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
    : Type IDENT SEMICOLON  { 
        insert_symbol($2, $1, yylineno ,"-"); 
        if(strcmp($1,"int") == 0 || strcmp($1,"bool") == 0){
            codegen("iconst_0\n");
            codegen("istore %d\n",last_var_addr);
        }
        if(strcmp($1,"float") == 0){
            codegen("fconst_0\n");
            codegen("fstore %d\n",last_var_addr);
        }
        if(strcmp($1,"string") == 0){
            codegen("ldc \"\"\n");
            codegen("astore %d\n",last_var_addr);
        }
            
    }
    | Type IDENT ASSIGN Expression SEMICOLON { 
        insert_symbol($2, $1, yylineno ,"-"); 
        if(strcmp($1,"int") == 0 || strcmp($1,"bool") == 0)
            codegen("istore %d\n",last_var_addr);
        if(strcmp($1,"float") == 0)
            codegen("fstore %d\n",last_var_addr);
        if(strcmp($1,"string") == 0)
            codegen("astore %d\n",last_var_addr);
    }
    | Type IDENT LBRACK Expression RBRACK SEMICOLON { 
        insert_symbol($2, "array", yylineno ,$1);
        codegen("newarray %s\n",$1);
        codegen("astore %d\n",last_var_addr);
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
        codegen("ior\n");
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
        codegen("iand\n"); 
    }
    | cmpExpr 
;

cmpExpr
    : cmpExpr cmp_op addExpr { 
        //codegen("%s\n", $2); 
        if(strcmp(found_type, "int") == 0){
            codegen("isub\n");
            codegen("%s L_cmp_%d\n", $2, label_ct);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\nL_cmp_%d:\n", label_ct+1,label_ct);
            codegen("iconst_1\nL_cmp_%d:\n", label_ct+1);
        }
        else if(strcmp(found_type, "float") == 0){
            codegen("fcmpl\n");
            codegen("%s L_cmp_%d\n", $2, label_ct);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\nL_cmp_%d:\n", label_ct+1,label_ct);
            codegen("iconst_1\nL_cmp_%d:\n", label_ct+1);
        }

        label_ct+=2;
        
        strncpy(found_type, "bool", 10);
        }
    | addExpr
;

addExpr
    : addExpr add_op mulExpr { 
        char tp[100];
        if(differ_f){
            sprintf(tp,"invalid operation: %s (mismatched types %s and %s)", $2, prev_type, found_type);
            yyerror(tp);
        }
        if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0)
            codegen("i%s\n", $2); 
        else if(strcmp(found_type,"float") == 0)
            codegen("f%s\n", $2); 
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
        if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0)
            codegen("i%s\n", $2); 
        else if(strcmp(found_type,"float") == 0)
            codegen("f%s\n", $2); 
    }
    | UnaryExpr 
;

UnaryExpr  
    : PrimaryExpr 
    | unary_op UnaryExpr { 
        if(strlen($1)) {
            codegen("%s\n", $1);
        } 
    }
;

cmp_op     
    : EQL { $$ =  "ifeq"; }
    | NEQ { $$ =  "ifne"; }
    | LSS { $$ =  "iflt"; }
    | LEQ { $$ =  "ifle"; }
    | GTR { $$ =  "ifgt"; }
    | GEQ { $$ =  "ifge"; }
;

add_op     
    : ADD { $$ =  "add"; }
    | SUB { $$ =  "sub"; }
;

mul_op     
    : MUL { $$ =  "mul"; }
    | QUO { $$ =  "div"; } 
    | REM { $$ =  "rem"; }
;

unary_op   
    : ADD { $$ = ""; }
    | SUB { 
        if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0)
            $$ = "ineg"; 
        else if(strcmp(found_type,"float") == 0)
            $$ = "fneg"; 
    }
    | NOT { $$ = "ixor"; 
            codegen("iconst_1\n");
    }
;

PrimaryExpr 
    : Operand 
    | IndexExpr 
    | ConversionExpr
;

Operand     
    : Literal { $$ = "LIT"; }
    | IDENT { 
        lookup_symbol($1); 
        if(is_assign){
            if(!isfound_arr){
                if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0)
                    codegen("iload %d\n", last_var_addr); 
                else if(strcmp(found_type,"float") == 0)
                    codegen("fload %d\n", last_var_addr); 
                else if(strcmp(found_type,"string") == 0)
                    codegen("aload %d\n", last_var_addr);
            }

        }
        isfound_arr = 0;
    }
    | LPARENexpr Expression RPAREN
;


IndexExpr 
    : PrimaryExpr LBRACKexpr Expression RBRACKexpr
;

LBRACKexpr
    : LBRACK { 
        strcpy(arr_type,found_type);
        strcpy(found_type, "int");
        hold_f = 0; 
        codegen("aload %d\n", last_var_addr);
        if(!is_assign)
            is_array = 1;
    }
;

RBRACKexpr
    : RBRACK { 
        hold_f = 1; 
        strcpy(found_type,arr_type);
        if(is_assign){
            if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0){
                codegen("iaload\n");
            }
            else if(strcmp(found_type,"float") == 0){
                codegen("faload\n");
            }
        }
    }
;

ConversionExpr 
    : LPARENexpr Type RPAREN Expression { 
        codegen("%c2%c\n", found_type[0], $2[0]);
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
        if(is_array){
            if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0){
                codegen("iastore\n");
            }
            else if(strcmp(found_type,"float") == 0){
                codegen("fastore\n");
            }
            is_array = 0;
        }
        else{
            if(strcmp(found_type,"int") == 0 || strcmp(found_type,"bool") == 0){
                if(strcmp($2,"ASSIGN") != 0){
                    codegen("iload %d\n", last_assign_addr);
                    if(strcmp($2,"ADD_ASSIGN") == 0)
                        codegen("iadd\n");
                    else if(strcmp($2,"SUB_ASSIGN") == 0){
                        codegen("swap\n");
                        codegen("isub\n");
                    }
                    else if(strcmp($2,"MUL_ASSIGN") == 0)
                        codegen("imul\n");
                    else if(strcmp($2,"QUO_ASSIGN") == 0){
                        codegen("swap\n");                        
                        codegen("idiv\n");
                    }
                    else if(strcmp($2,"REM_ASSIGN") == 0){
                        codegen("swap\n");
                        codegen("irem\n");
                    }
                }
                codegen("istore %d\n",last_assign_addr);
            }
            else if(strcmp(found_type,"float") == 0){
                if(strcmp($2,"ASSIGN") != 0){
                    codegen("fload %d\n", last_assign_addr);
                    if(strcmp($2,"ADD_ASSIGN") == 0)
                        codegen("fadd\n");
                    else if(strcmp($2,"SUB_ASSIGN") == 0){
                        codegen("swap\n");
                        codegen("fsub\n");
                    }
                    else if(strcmp($2,"MUL_ASSIGN") == 0)
                        codegen("fmul\n");
                    else if(strcmp($2,"QUO_ASSIGN") == 0){
                        codegen("swap\n");
                        codegen("fdiv\n");
                    }
                    else if(strcmp($2,"REM_ASSIGN") == 0){
                        codegen("swap\n");
                        codegen("frem\n");
                    }
                }
                codegen("fstore %d\n",last_assign_addr);
            }
            else if(strcmp(found_type,"string") == 0){
                codegen("astore %d\n",last_assign_addr);
            }
        }
        //codegen("%s\n", $2); 
        is_assign = 0;
    }
;

AssignmentStmt 
    : AssignmentExpr SEMICOLON
;

assign_op      
    : ASSIGN { $$ = "ASSIGN" ; is_assign = 1;}
    | ADD_ASSIGN { $$ = "ADD_ASSIGN" ; is_assign = 1;}
    | SUB_ASSIGN { $$ = "SUB_ASSIGN" ; is_assign = 1;}
    | MUL_ASSIGN { $$ = "MUL_ASSIGN" ; is_assign = 1;}
    | QUO_ASSIGN { $$ = "QUO_ASSIGN" ; is_assign = 1;}
    | REM_ASSIGN { $$ = "REM_ASSIGN" ; is_assign = 1;}
;

IncDecExpr 
    : Expression INC { 
        if(is_for){
            char tp[50];
            if(strcmp(found_type,"int") == 0){
                sprintf(post_stmt,"\ticonst_1\n");
                sprintf(tp,"\tiload %d\n", last_assign_addr);
                strcat(post_stmt,tp);
                strcat(post_stmt,"\tiadd\n");
                sprintf(tp,"\tistore %d\n",last_assign_addr); 
                strcat(post_stmt,tp);               
            }
            else if(strcmp(found_type,"float") == 0){
                sprintf(post_stmt,"\tfconst_1\n");
                sprintf(tp,"\tfload %d\n", last_assign_addr);
                strcat(post_stmt,tp);
                sprintf(post_stmt,"\tfadd\n");
                sprintf(tp,"\tfstore %d\n", last_assign_addr);
                strcat(post_stmt,tp);
            }
            is_for = 0;
        }
        else{
            if(strcmp(found_type,"int") == 0){
                codegen("iconst_1\n");
                codegen("iload %d\n", last_assign_addr);
                codegen("iadd\n");
                codegen("istore %d\n",last_assign_addr); 
            }
            else if(strcmp(found_type,"float") == 0){
                codegen("fconst_1\n");
                codegen("fload %d\n", last_assign_addr);
                codegen("fadd\n");
                codegen("fstore %d\n", last_assign_addr);
            }
        }
        
    }
    | Expression DEC { 
        if(is_for){
            char tp[50];
            if(strcmp(found_type,"int") == 0){
                sprintf(post_stmt,"\tiload %d\n", last_assign_addr);
                strcat(post_stmt,"\ticonst_1\n");
                strcat(post_stmt,"\tisub\n");
                sprintf(tp,"\tistore %d\n",last_assign_addr); 
                strcat(post_stmt,tp);               
            }
            else if(strcmp(found_type,"float") == 0){
                sprintf(post_stmt,"\tfload %d\n", last_assign_addr);
                strcat(post_stmt,"\tfconst_1\n");
                strcat(post_stmt,"\tfsub\n");
                sprintf(tp,"\tfstore %d\n",last_assign_addr); 
                strcat(post_stmt,tp);           
            }
            is_for = 0;
        }
        else{
            if(strcmp(found_type,"int") == 0){
                codegen("iload %d\n", last_assign_addr);
                codegen("iconst_1\n");
                codegen("isub\n");
                codegen("istore %d\n", last_assign_addr); 
            }
            else if(strcmp(found_type,"float") == 0){
                codegen("fload %d\n", last_assign_addr);
                codegen("fconst_1\n");
                codegen("fsub\n");
                codegen("fstore %d\n", last_assign_addr);
            }
        }
        
    }
;

IncDecStmt 
    : IncDecExpr SEMICOLON
;

ArithStmt
    : Expression SEMICOLON
;



IfStmt 
    : IF Condition Block{
        INDENT--;
        codegen("L_if_false%d", if_label_ct-1);/*
        for(int i=0;i<if_ct;i++){
            fprintf(fout, ".loop");
        }*/
        fprintf(fout, " :\n");
        if_ct--;
        INDENT++;
        if_label_ct--;
    }
    | IF Condition Block ELSEexpr IfStmt{
        INDENT--;
        codegen("L_if_exit%d",if_label_ct-1);/*
        for(int i=0;i<if_ct;i++){
            fprintf(fout, ".loop");
        }*/
        fprintf(fout, " :\n");
        INDENT++;
        if_ct--;
        if_label_ct--;       
    }
    | IF Condition Block ELSEexpr Block{
        INDENT--;
        codegen("L_if_exit%d",if_label_ct-1);/*
        for(int i=0;i<if_ct;i++){
            fprintf(fout, ".loop");
        }*/
        fprintf(fout, " :\n");
        INDENT++;
        if_ct--;
        if_label_ct--;
    }
;

ELSEexpr
    : ELSE{
        codegen("goto L_if_exit%d", if_label_ct-1);/*
        for(int i=0;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }*/
        fprintf(fout, "\n");
        INDENT--;
        codegen("L_if_false%d", if_label_ct-1);/*
        for(int i=0;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }*/
        fprintf(fout, " :\n");
        INDENT++;
    }
;

Condition 
    : Expression {
        char tp[100];
        if(strcmp(found_type,"bool") != 0){
            sprintf(tp,"non-bool (type %s) used as for condition", found_type);
            non_bool_f = 1;
            yyerror(tp);
        }
        
        if(isloop){
            codegen("ifeq L_for_exit%d", loop_label_ct-1);
            for(int i=1;i<loop_ct;i++){
                fprintf(fout, ".loop");
            }
            fprintf(fout, "\n");
            isloop = 0;
        }
        else{
            codegen("ifeq L_if_false%d", if_label_ct);
            for(int i=1;i<loop_ct;i++){
                fprintf(fout, ".loop");
            }
            fprintf(fout, "\n");
            if_label_ct++;
            if_ct++;
        }
        
        
        is_assign = 0;
    }
    
;

WhileStmt 
    : WHILEexpr LPARENexpr Condition RPAREN Block {
        codegen("goto L_for_begin%d", loop_label_ct-1);
        for(int i=1;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }
        fprintf(fout, "\n");
        INDENT--;
        codegen("L_for_exit%d",loop_label_ct-1);
        for(int i=1;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }
        fprintf(fout, " :\n");
        INDENT++;
        loop_ct--;
    }
;

WHILEexpr
    : WHILE {
        INDENT--;
        if(loop_ct) loop_label_ct--;
        codegen("L_for_begin%d", loop_label_ct);
        for(int i=0;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }
        fprintf(fout, " :\n");
        INDENT++;
        loop_label_ct++;
        loop_ct++;
        isloop = 1;
    }
;

ForStmt   
    : FORexpr LPARENexpr ForClause RPAREN Block{
        fprintf(fout, post_stmt);
        codegen("goto L_for_begin%d", loop_label_ct-1);
        for(int i=1;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }
        fprintf(fout, "\n");
        INDENT--;
        codegen("L_for_exit%d",loop_label_ct-1);
        for(int i=1;i<loop_ct;i++){
            fprintf(fout, ".loop");
        }
        fprintf(fout, " :\n");
        INDENT++;
        loop_ct--;
    }
;

FORexpr
    : FOR{
        
        isloop = 1;
        is_for = 1;
    }

ForClause 
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt{
        
    }
;

InitStmt  
    : SimpleExpr{
        if(is_for){
            INDENT--;
            if(loop_ct) loop_label_ct--;
            codegen("L_for_begin%d", loop_label_ct);
            for(int i=0;i<loop_ct;i++){
                fprintf(fout, ".loop");
            }
            fprintf(fout, " :\n");
            INDENT++;
            loop_label_ct++;
            loop_ct++;
            is_assign = 1;
        }
    }
;

PostStmt  
    : SimpleExpr{

    }
;

SimpleExpr 
    : AssignmentExpr 
    | Expression 
    | IncDecExpr
;

PrintStmt 
    : PRINT LPARENexpr Expression RPAREN SEMICOLON {
        if($3){
            if(strcmp(found_type,"int") == 0){
                codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                codegen("swap\n");
                codegen("invokevirtual java/io/PrintStream/print(I)V\n");
            }
            else if(strcmp(found_type,"float") == 0){
                codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                codegen("swap\n");
                codegen("invokevirtual java/io/PrintStream/print(F)V\n");
            }
            else if(strcmp(found_type,"string") == 0){
                codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                codegen("swap\n");
                codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
            }
            else if(strcmp(found_type,"bool") == 0){
                codegen("ifne L_cmp_%d\n",label_ct);
                codegen("ldc \"false\"\n");
                codegen("goto L_cmp_%d\nL_cmp_%d:\n", label_ct+1,label_ct);
                codegen("ldc \"true\"\nL_cmp_%d:\n", label_ct+1);
                codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                codegen("swap\n");
                codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
                label_ct += 2;
            }
            
        }
        is_assign = 0;
    }
;

LPARENexpr
    : LPAREN{
        if(!is_for)
            is_assign = 1;
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

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j\n");
    codegen(".class public Main\n");
    codegen(".super java/lang/Object\n");
    codegen(".method public static main([Ljava/lang/String;)V\n");
    codegen(".limit stack 100\n");
    codegen(".limit locals 100\n");
    INDENT++;

    yyparse();

	printf("Total lines: %d\n", yylineno);

    /* Codegen end */
    codegen("return\n");
    INDENT--;
    codegen(".end method\n");
    fclose(fout);
    fclose(yyin);

    if (HAS_ERROR) {
        remove(bytecode_filename);
    }
    return 0;
}
