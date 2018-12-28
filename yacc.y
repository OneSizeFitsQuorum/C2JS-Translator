%{
#include "prepare.h"
#include <stdarg.h>
#include <stdlib.h>
using namespace std;

typeASTNode *_type(typeEnum value);
intASTNode *_int(int value);
stringASTNode *_string(string value);
exprASTNode *_expr(int childNum,int _operator, ...);
exprListASTNode *_exprList(int childNum, ...);
statementASTNode *_statement(int childNum,int _statement, ...);
statementListASTNode *_statementList(int childNum, ...);
functionASTNode *_function(int childNum, ...);
functionListASTNode *_functionList(int childNum, ...);
parameterASTNode *_parameter(int childNum, ...);
parameterListASTNode *_parameterList(int childNum, ...);
idASTNode *_id(string value);
int getChildNum(ASTNode *p);
void freeNode(ASTNode *p);

void codeGenerator(ASTNode *p);

void test(const char* sFile);

%}

%token<intValue>INTEGER
%token<type>INT CHAR
%token<strValue>IDENTIFIER STRING

%token INC_OP DEC_OP INC_OP_LEFT INC_OP_RIGHT DEC_OP_LEFT DEC_OP_RIGHT GE_OP LE_OP EQ_OP NE_OP AND_OP OR_OP
%token DECLARE DECLARE_ARRAY
%token FOR WHILE BREAK CONTINUE IF ELSE RETURN PRINTF STRLEN

%nonassoc IFX
%nonassoc ELSE

%left EQ_OP NE_OP '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%type<ptr>functionList function parameterList parameter typeName statementList statement exprList expr
%start program

%%

program:
        functionList                                                                {codeGenerator($1);freeNode($1);}
       ;

functionList:
        function                                                                    {$$ = _functionList(1,$1);}
       |function functionList                                                       {$$ = _functionList(1 + getChildNum($2),$1,$2);}
       ;

function:
        typeName IDENTIFIER '(' parameterList ')' '{' statementList '}'             {$$ = _function(4,$1,_id($2),$4,$7);}
       |typeName IDENTIFIER '('')' '{' statementList '}'                            {$$ = _function(3,$1,_id($2),$6);}
       ;

parameterList:
        parameter                                                                   {$$ = _parameterList(1,$1);}
       |parameter ',' parameterList                                                 {$$ = _parameterList(1 + getChildNum($3),$1,$3);}
       ;

parameter:
        typeName IDENTIFIER                                                         {$$ = _parameter(2,$1,_id($2));}
       |typeName IDENTIFIER '[' ']'                                                 {$$ = _parameter(2,$1,_id($2));}
       ;

typeName:
        INT                                                                         {$$ = _type($1);}
       |CHAR                                                                        {$$ = _type($1);}
       ;

statementList:
        statement                                                                   {$$ = _statementList(1,$1);}
       |statement statementList                                                     {$$ = _statementList(1 + getChildNum($2),$1,$2);}
       ;

statement:
        FOR '(' statement expr ';' expr ')' '{' statementList '}'                   {$$ = _statement(4,FOR,$3,$4,$6,$9);}
       |WHILE '(' expr ')' '{' statementList '}'                                    {$$ = _statement(2,WHILE,$3,$6);}
       |BREAK ';'                                                                   {$$ = _statement(0,BREAK);}
       |CONTINUE ';'                                                                {$$ = _statement(0,CONTINUE);}
       |IF '(' expr ')' '{' statementList '}' %prec IFX                             {$$ = _statement(2,IF,$3,$6);}
       |IF '(' expr ')' '{' statementList '}' ELSE '{' statementList '}'            {$$ = _statement(3,ELSE,$3,$6,$10);}
       |RETURN expr ';'                                                             {$$ = _statement(1,RETURN,$2);}
       |PRINTF '(' exprList ')' ';'                                                 {$$ = _statement(1,PRINTF,$3);}
       |IDENTIFIER '(' exprList ')' ';'                                             {$$ = _statement(2,IDENTIFIER,_id($1),$3);}
       |IDENTIFIER '=' expr ';'                                                     {$$ = _statement(2,DECLARE,_id($1),$3);}
       |IDENTIFIER '[' expr ']' '=' expr ';'                                        {$$ = _statement(3,DECLARE_ARRAY,_id($1),$3,$6);}
       |typeName IDENTIFIER '=' expr ';'                                            {$$ = _statement(3,DECLARE,$1,_id($2),$4);}
       |typeName IDENTIFIER '[' ']' '=' expr ';'                                    {$$ = _statement(3,DECLARE_ARRAY,$1,_id($2),$6);}
       |typeName IDENTIFIER '[' INTEGER ']' ';'                                     {$$ = _statement(3,DECLARE,$1,_id($2),_int($4));}
       |typeName IDENTIFIER ';'                                                     {$$ = _statement(2,DECLARE,$1,_id($2));}
       |INC_OP expr ';'                                                             {$$ = _statement(1,INC_OP_LEFT,$2);}
       |DEC_OP expr ';'                                                             {$$ = _statement(1,DEC_OP_LEFT,$2);}
       |expr INC_OP ';'                                                             {$$ = _statement(1,INC_OP_RIGHT,$1);}
       |expr DEC_OP ';'                                                             {$$ = _statement(1,DEC_OP_RIGHT,$1);}
       ;

exprList:
        expr                                                                        {$$ = _exprList(1,$1);}
       |expr ',' exprList                                                           {$$ = _exprList(1 + getChildNum($3),$1,$3);}
       ;

expr:
        INTEGER                                                                     {$$ = _int($1);}
       |STRING                                                                      {$$ = _string($1);}
       |IDENTIFIER                                                                  {$$ = _id($1);}
       |'-' expr %prec UMINUS                                                       {$$ = _expr(1,UMINUS,$2);}
       |STRLEN '(' IDENTIFIER ')'                                                   {$$ = _expr(1,STRLEN,_id($3));}
       |IDENTIFIER '(' exprList ')'                                                 {$$ = _expr(2,IDENTIFIER,_id($1),$3);}
       |IDENTIFIER '[' expr ']'                                                     {$$ = _expr(2,'[',_id($1),$3);}
       |expr '+' expr                                                               {$$ = _expr(2,'+',$1,$3);}
       |expr '-' expr                                                               {$$ = _expr(2,'-',$1,$3);}
       |expr '*' expr                                                               {$$ = _expr(2,'*',$1,$3);}
       |expr '/' expr                                                               {$$ = _expr(2,'/',$1,$3);}
       |expr '<' expr                                                               {$$ = _expr(2,'<',$1,$3);}
       |expr '>' expr                                                               {$$ = _expr(2,'>',$1,$3);}
       |expr NE_OP expr                                                             {$$ = _expr(2,NE_OP,$1,$3);}
       |expr EQ_OP expr                                                             {$$ = _expr(2,EQ_OP,$1,$3);}
       |expr OR_OP expr                                                             {$$ = _expr(2,OR_OP,$1,$3);}
       |expr AND_OP expr                                                            {$$ = _expr(2,AND_OP,$1,$3);}
       |expr LE_OP expr                                                             {$$ = _expr(2,LE_OP,$1,$3);}
       |expr GE_OP expr                                                             {$$ = _expr(2,GE_OP,$1,$3);}
       |'!' expr                                                                    {$$ = _expr(1,'!',$2);}
       |'(' expr ')'                                                                {$$ = _expr(1,'(',$2);}
       |INC_OP expr                                                                 {$$ = _expr(1,INC_OP_LEFT,$2);}
       |DEC_OP expr                                                                 {$$ = _expr(1,DEC_OP_LEFT,$2);}
       |expr INC_OP                                                                 {$$ = _expr(1,INC_OP_RIGHT,$1);}
       |expr DEC_OP                                                                 {$$ = _expr(1,DEC_OP_RIGHT,$1);}
       ;

%%

void yyerror(const char *s)
{
	cerr<<s<<endl;
}

typeASTNode *_type(typeEnum value)
{
    typeASTNode *p = new typeASTNode();
    p->intValue = value;
    p->type = nodeEnum::typeType;
    return p;
}

intASTNode *_int(int value)
{
    intASTNode *p = new intASTNode();
    p->intValue = value;
    p->type = nodeEnum::typeInt;
    return p;
}

stringASTNode *_string(string value)
{
    stringASTNode *p = new stringASTNode();
    p->strValue = value;
    p->type = nodeEnum::typeString;
    return p;
}

exprASTNode *_expr(int childNum,int _operator,...)
{
    va_list para;
    exprASTNode *p = new exprASTNode();
    p->type = nodeEnum::typeExpr;
    p->intValue = _operator;
    p->childNum = childNum;
    va_start(para,_operator);
    for(int i = 0;i < childNum;i++)
        p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    va_end(para);
    return p;
}

exprListASTNode *_exprList(int childNum,...)
{
    va_list para;
    exprListASTNode *p = new exprListASTNode();
    p->type = nodeEnum::typeExprList;
    p->childNum = childNum;
    va_start(para,childNum);
    p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    if(childNum > 1)
    {
        exprListASTNode *_p = va_arg(para,exprListASTNode*);
        for(int i = 1;i < childNum;i++)
            p->child.push_back(_p->child[i-1]);
    }
    va_end(para);
    return p;
}

statementASTNode *_statement(int childNum,int _statement,...)
{
    va_list para;
    statementASTNode *p = new statementASTNode();
    p->type = nodeEnum::typeStatement;
    p->intValue = _statement;
    p->childNum = childNum;
    va_start(para,_statement);
    for(int i = 0;i < childNum;i++)
        p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    va_end(para);
    return p;
}

statementListASTNode *_statementList(int childNum,...)
{
    va_list para;
    statementListASTNode *p = new statementListASTNode();
    p->type = nodeEnum::typeStatementList;
    p->childNum = childNum;
    va_start(para,childNum);
    p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    if(childNum > 1)
    {
        statementListASTNode *_p = va_arg(para,statementListASTNode*);
        for(int i = 1;i < childNum;i++)
            p->child.push_back(_p->child[i-1]);
    }
    va_end(para);
    return p;

}

functionASTNode *_function(int childNum,...)
{
    va_list para;
    functionASTNode *p = new functionASTNode();
    p->type = nodeEnum::typeFunction;
    p->childNum = childNum;
    va_start(para,childNum);
    for(int i = 0;i < childNum;i++)
        p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    va_end(para);
    return p;
}

functionListASTNode *_functionList(int childNum,...)
{
    va_list para;
    functionListASTNode *p = new functionListASTNode();
    p->type = nodeEnum::typeFunctionList;
    p->childNum = childNum;
    va_start(para,childNum);
    p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    if(childNum > 1)
    {
        functionListASTNode *_p = va_arg(para,functionListASTNode*);
        for(int i = 1;i < childNum;i++)
            p->child.push_back(_p->child[i-1]);
    }
    va_end(para);
    return p;
}

parameterASTNode *_parameter(int childNum,...)
{
    va_list para;
    parameterASTNode *p = new parameterASTNode();
    p->type = nodeEnum::typeParameter;
    p->childNum = childNum;
    va_start(para,childNum);
    for(int i = 0;i < childNum;i++)
        p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    va_end(para);
    return p;
}

parameterListASTNode *_parameterList(int childNum,...)
{
    va_list para;
    parameterListASTNode *p = new parameterListASTNode();
    p->type = nodeEnum::typeParameterList;
    p->childNum = childNum;
    va_start(para,childNum);
    p->child.push_back(va_arg(para,ASTNodeNonTerminal*));
    if(childNum > 1)
    {
        parameterListASTNode *_p = va_arg(para,parameterListASTNode*);
        for(int i = 1;i < childNum;i++)
            p->child.push_back(_p->child[i-1]);
    }
    va_end(para);
    return p;
}

idASTNode *_id(string value)
{
    idASTNode *p = new idASTNode();
    p->type = nodeEnum::typeId;
    p->strValue = value;
    return p;
}

int getChildNum(ASTNode *p)
{
    return ((ASTNodeNonTerminal*)p)->childNum;
}

void freeNode(ASTNode *p)
{
    if (!p) return;
    switch (p->type)
    {
        case typeExpr:
        case typeExprList:
        case typeStatement:
        case typeStatementList:
        case typeFunction:
        case typeFunctionList:
        case typeParameter:
        case typeParameterList:
            {
                ASTNodeNonTerminal* ptr = (ASTNodeNonTerminal*)p;
                for (int i = 0; i < ptr->childNum; i++)
                    freeNode(ptr->child[i]);
            }
            break;
    }
    delete p;
}


void codeGenerator(ASTNode *p)
{

}

void test(const char* sFile)
{
	FILE* fp=fopen(sFile, "r");
	if(fp==NULL)
	{
		printf("cannot open %s\n", sFile);
		return;
	}
	extern FILE* yyin;
	yyin=fp;

	printf("-----begin parsing %s\n", sFile);
	yyparse();
	puts("-----end parsing");

	fclose(fp);
}

int main()
{
    const char* level1 = "level1.c";
    test(level1);
    const char* level2 = "level2.c";
    test(level2);
	return 0;
}
