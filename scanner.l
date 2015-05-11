/* SCANNER by GFLL
 * 
 * DESCRICAO: SCANNER DE LINGUAGEM QUE PERMITE MANIPULACAO DE AUTOMATOS
 * DATA 1ª VERSAO: 15/04/2015
 * 1ª VERSAO: 1.0
 * VERSAO ATUAL: 1.3
 * 
 * HISTORICO DE VERSOES:
 * 
 */

%{

#include <stdio.h>
#include "parser.tab.h"
#include "definicoes.h"	

int linha = 1;
int coluna = 1;
int retorno;
void conta ();




%}

%option noyywrap
%option bison-bridge


D  [0-9]					
L  [a-zA-Z_]				

%x BL_COMENT
%x LN_COMENT
%x REGULAR_EXPRESSION

%%

"/*" 						{BEGIN (BL_COMENT);}
<BL_COMENT>"*/" 			{BEGIN (INITIAL);}
<BL_COMENT>"\n" 			{linha++; }
<BL_COMENT>.				{coluna++;}

"//" 						{BEGIN (LN_COMENT);}
<LN_COMENT>"\n"				{linha++; BEGIN(INITIAL);}
<LN_COMENT>. 				{ }

"'" 						{BEGIN (REGULAR_EXPRESSION);}
<REGULAR_EXPRESSION>"'"		{coluna++; BEGIN(INITIAL);}
<REGULAR_EXPRESSION>.		{return WORD;}


"int"						{conta();return (TYPE_INT);}
"void"						{conta();return (TYPE_VOID);}
"if"						{conta();return (IF);}
"else"						{conta();return (ELSE);}

"while"						{conta();return (WHILE);}
"AFD"						{conta();return (AFD);}
"AFN"						{conta();return (AFN);}
"ER"						{conta();return (ER);}
"to"						{conta();return (TO);}
"show"						{conta();return (SHOW);}
"VAZIO"						{conta();return (VAZIO);}
"Q"							{conta();return (Q);}
"E"							{conta();return (E);}
"D"							{conta();return (D);}
"F"							{conta();return (F);}


"&&"						{conta();return (AND);} 
"||"						{conta();return (OR);} 
"+"							{conta();return ('+');} 
"-"							{conta();return ('-');} 
"*"							{conta();return ('*');} 
"/"	  						{conta();return ('/');} 
"=="						{conta();return (EQUAL);}
"!="						{conta();return (N_EQUAL);}
"<="						{conta();return (L_EQUAL);}
">="						{conta();return (G_EQUAL);}


"++"						{conta();return (UNION);} 
"<-"						{conta();return (DEF);}
"**"						{conta();return (STAR);} 
":"							{conta();return (':');} 
"S=S"						{conta();return (SIMUL);} 

"="							{conta();return ('=');} 
"<"							{conta();return ('<');}
">"							{conta();return ('>');}
"("							{conta();return ('(');}
")"							{conta();return (')');}
";"							{conta();return (';');}
","							{conta();return (',');}
"{"							{conta();return ('{');}
"}"							{conta();return ('}');}
 
{D}+						{conta();return (INT);}
"."{D}|{D}"."|{D}"."{D}		{conta();
							printf("\n\nWARNING (linha %d, coluna %d) %s : nao ha a necessidade de usar numeros nao inteiros nessa linguagem por enquanto\n\n\n", linha, coluna, yytext);
							return (WARNING);
							BEGIN (INITIAL);}

{L}+({D}|{L})*				{conta();return (ID);}

{D}+[^D]					{conta();
							printf("\n\nERRO lexico(linha %d, coluna %d) %s :	voce nao pode comecar um identificador com um numero\n\n\n", linha, coluna, yytext);
							return ERRO;
							BEGIN (INITIAL);}

({D}{L})+					{conta(); return (WORD);}

[ \t\n]+

.           				{printf( "\n\nERRO lexico(linha %d, coluna %d) %s :	Caracter nao aceito pela linguagem\n\n\n",linha, coluna, yytext );
							return ERRO;
							BEGIN (INITIAL);}


%%

void conta() {
		
	int i;
	for (i = 0; yytext[i] != '\0'; i++){
		if(yytext[i] == '\n') {
			linha ++;
			coluna = 1;
		}
		else { 
			if (yytext[i] == '\t')
				coluna += 8 - (coluna % 8);
			else
				coluna++;
			}
		}	
	
	}
	



		

    

	
	
  
