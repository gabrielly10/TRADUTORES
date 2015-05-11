%error-verbose // mostra os erros
%debug // ativa o modo de debug
%defines // cria o arquivo tab .h com as definições
%pure-parser


%code requires {
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
	
	extern int yylex () ;
	extern FILE *yyin;
	extern int linha;

/* estrutura para montar a tabela de simbolos 
*  campos: nome, valor, tipo e foiDefinido,
*/

typedef struct Simbolo Simbolo;
struct Simbolo {
	char* nome;
	char* valor;
	char* tipo;
	int foiDefinido;

};	
	
/* Estrutura da arvore utilizada para montar a arvore */

typedef struct Arvore Arvore;
struct Arvore {
	char* name;
	char* value;
	int contaFilho;
	Arvore *pai;
	Arvore *filho;
	Arvore *irmao;

};

/*Definicoes das funcoes e variaveis que sao utilizadas pelo programa*/

Arvore * nova_arvore(const char* name, char* val) ;
void adiciona_filho(Arvore *, Arvore *);
void adiciona_irmao(Arvore *, Arvore *);
void mostra_arvore(Arvore*, int);

void yyerror (char const *s);
FILE *fp;

}
/*
%type <Arvore> definition
%type <Arvore> states
%type <Arvore> symbols
%type <Arvore> ftrans
*/

	
%token INT
%token IF						
%token ELSE												
%token WHILE						
%token AFD						
%token AFN						
%token ER						
%token TO					
%token SHOW					
%token VAZIO					
%token Q							
%token E							
%token D							
%token F
%token ID
%token TYPE_INT
%token TYPE_VOID

%token WORD
%token STAR
%token UNION

%token AND
%token OR
%token DIV
%token EQUAL
%token N_EQUAL
%token L_EQUAL
%token G_EQUAL

%token DEF
%token SIMUL
	
%precedence algumacoisa
%precedence ELSE	
	
%right  '=' 
%left  '+'  '-'
%left  '*'  '/'
%nonassoc '>' '<' L_EQUAL G_EQUAL	
	

%start inicio

%%

function
	: ID TO convert
	| ID '=' operation
	| WORD simulation
	//| formalism definition AFD 
	| show			
	;

show
	: SHOW formalism ID ';' 
	;

convert
	:  formalism ID ';'
	;

operation
	: ID op_regular ';'
	;
	
op_regular
	: STAR
	| ':' ID 
	| UNION ID  
	;

simulation
	: SIMUL ID ';'	//Definir um tipo WORD para especificar qualquer sequencia de caracteres? OK
	;
	
formalism // posso tratar isso no semantico?
	: AFD
	| AFN
	| ER
	;
	
// não está sendo reconhecido pelo parser, sera arrumado para a proxima entrega
/*definition
	:  Q '=' '{'states'}'','E '=' '{'symbols'}'','D '=' '{'ftrans'}'','F '=' '{'states'}'';'
	;

states_definition 
	: Q '=' '{'states'}'','
	;

alpha_definition 
	: E '=' '{'symbols'}'','
	;

ftrans_definition 
	: D '=' '{'ftrans'}'','
	;

accept_definition
	: F '=' '{'states'}'';'
	;

ftrans
	: '{''('state symbols ',' state')' ftrans '}' 
	| '{''('state symbols ',' state')''}'
	;
	
states 
	: states','state 
	;
	
state
	: ID
	| VAZIO
	;
	
symbols 
	: symbols ',' symbole
	| symbole
	;

symbole
	: ID
	| VAZIO	
	; */

//SUPORTE PARA OPERAOES ARITMETICAS

factor
	: INT 
	| '('expr')'
	;

mult_expr
	: factor
	| mult_expr '*' factor
	| mult_expr '/' factor
	;

add_expr
	: mult_expr
	| add_expr '+' mult_expr // mult_expr '+' add_expr add_expr '+' mult_expr 
	| add_expr '-' mult_expr
	;

relational_expr
	: add_expr
	| relational_expr '<' add_expr// add_expr '<' relational_expr relational_expr '<' add_expr
	| relational_expr '>' add_expr
	| relational_expr L_EQUAL add_expr
	| relational_expr G_EQUAL add_expr 
	;

equal_expr
	: relational_expr
	| equal_expr EQUAL relational_expr
	| equal_expr N_EQUAL relational_expr 
	;

logical_and_expr
	: equal_expr
	| logical_and_expr AND equal_expr
	;

logical_or_expr
	: logical_and_expr
	| logical_or_expr OR logical_and_expr  
	;
	
a_operator
	: '='
	| N_EQUAL
	| L_EQUAL
	| G_EQUAL
	;

a_expression
	: logical_or_expr
	| direct_declarator a_operator a_expression 
	;

expr
	: a_expression
	| expr ',' a_expression
	;

expr_stmt
	: ';'
	| expr ';'
	;

it_stmt
	: WHILE '(' expr ')' stmt
	;

if_stmt
	: IF '(' expr ')' stmt %prec algumacoisa
	| IF '(' expr ')' stmt ELSE stmt
	;

stmt
	: compound_statement
	| expr_stmt
	| if_stmt
	| it_stmt
	;

type_specifier
	: TYPE_INT
	| TYPE_VOID
	;
	
statement_list
	: stmt
	| statement_list stmt
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;

initializer
	: a_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: direct_declarator
	| direct_declarator '=' initializer
	;

declaration_specifiers
	: type_specifier
	| type_specifier declaration_specifiers

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	;


compound_statement
	: '{' '}'
	| '{' statement_list '}'
	| '{' declaration_list '}'
	| '{' declaration_list statement_list '}'
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

direct_declarator
	: ID
	| '(' direct_declarator ')'
	;

inicio
	: external_declaration
	| inicio external_declaration
	;
	
external_declaration
	: function_definition
	| declaration
	| function
	;

function_definition
	: declaration_specifiers direct_declarator declaration_list compound_statement
	| declaration_specifiers direct_declarator compound_statement
	| direct_declarator declaration_list compound_statement
	| direct_declarator compound_statement
	;

%%

Arvore * nova_arvore(const char* name, char* val) {
    int i;
    Arvore *node;

    node = (Arvore *) malloc (sizeof(Arvore));

    if (name != NULL){
        node->name = strdup(name);
    } else
        node->name = NULL;

    if(val != NULL){
    	node->value = strdup(val);
    } else
    	node->value = NULL;

    node->pai = NULL;
    node->filho = NULL;
	node->irmao = NULL;
	node->contaFilho = 0;
 
    return node;
}

void adiciona_filho(Arvore *self, Arvore *child) {
    Arvore *temp;

    if (child == NULL)
        return;

    if (self->filho == NULL) {
        child->pai = self;
        self->filho = child;
    } else {
        adiciona_irmao(self->filho, child);
    }
    self->contaFilho = self->contaFilho + 1;
    for (temp = child; temp != NULL; temp = temp->irmao)
        temp->pai = self;
}

void adiciona_irmao(Arvore *self, Arvore *sibling) {
    Arvore *temp;

    if (sibling == NULL)
        return;

    if (self->irmao == NULL) {
        self->irmao = sibling;
    } else {
        for (temp = self->irmao; temp->irmao != NULL; temp = temp->irmao)
            ;
        temp->irmao = sibling;
    }
}


void mostra_arvore(Arvore* root, int level) {
    Arvore* next = NULL;
    int i = 0;

    if (level == 1){
		printf("\n********************************\n");
		printf("\nArvore gerada no arquivo arvore.txt\n");
		printf("\n********************************\n");
		fp = fopen("arvore.txt","w+");
   	}
    if (root != NULL) {
        for(i = 0; i < level; i++) {
            fprintf(fp,"  ");
			//printf("  ");
        }
        if (root->value != NULL) {
            fprintf(fp,"%s\n", root->value);
			//printf("%s  ", root->value);
        }else {
            fprintf(fp,"%s\n", root->name);
			//printf("%s\n", root->name);
        }
        next = root->filho;
        while (next != NULL) {
            mostra_arvore(next, level + 1);
            next = next->irmao;
        }

    }
}
	

void yyerror (char const *s) { 
	 
	printf("\nERRO sintatico! Linha %d: %s\n", linha, s);
	
}
	
int main( argc, argv) 
	int argc;
	char **argv;
		{
		++argv, --argc;  /* skip over program name */
		if ( argc > 0 )
				yyin = fopen( argv[0], "r" );
		else
				yyin = stdin;
			
	return yyparse () ;

		
	
}
