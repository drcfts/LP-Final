%% Linguagem com suporte a funcoes de alta.
%
% Esse modulo Prolog ilustra a implementacao de um
% interpretador para uma linguagem de programacao funcional
% com suporte a expressoes lambda, expressoes do tipo let
% e escopo dinamico e estrategia innermost evaluation.
%
%
%

% Implementacao de verificacao de tipos
%
% Implementar, em prolog, verificacao de tipos vista nos caps 24 e 25
% com as expressoes ja implementadas na linguagem LFC
% O que foi feito:
% - Avaliacao e verificacao de tipos para Subtracao, Divisao, Multiplicacao
% - Verificacao de tipos para let
% - Verificacao de tipos para Ref (variavel)
% - Verificacao de tipos para lambda
% - Verificacao de tipos para aplicacao de funcao

%Tipo Inteiro, booleano e funcao
tipo(Gamma,Vx,tipo_int) :- avaliar(Gamma, Vx, valor_inteiro(_)).
tipo(Gamma,Vx,tipo_bool) :- avaliar(Gamma, Vx, valor_booleano(_)).
tipo(_,lambda((_,Tipo1), Tipo2, _),tipo_funcao(Tipo1,Tipo2)).

%Tipo de uma variavel
verificarTipos(Gamma, var(Var), variavel_nao_declarada) :-
    pesquisarTipos(var(Var), Gamma, variavel_nao_encontrada),!.
verificarTipos(Gamma, var(Var), Res) :-
  pesquisarTipos(var(Var), Gamma, Res).
% Verificar tipo do let
verificarTipos(Gamma, let(X, Exp1, Exp2), Res) :-
  tipo(Gamma, Exp1, TipoNomeada),
  tipo([(X, TipoNomeada)|Gamma], Exp2, Res).

% Verificar tipo de aplicacao de funcao
verificarTipos(Gamma, aplicacao(Exp1, Exp2), Res) :-
  tipo(Gamma, Exp1, tipo_funcao(X,Y)), !,
  Res = Y,
  tipo(Gamma, Exp2, TipoE2),
  TipoE2 == X,!,
  Res is Y.
verificarTipos(_, aplicacao(_, _), erro_tipo_lambda).
verificarTipos(_, _, erro_tipo).

pesquisarTipos(_, [], variavel_nao_encontrada).
pesquisarTipos(Var, [(Var, Tipo)|_], Tipo) :- !.
pesquisarTipos(Var, [_|Tail], Res) :- pesquisarTipos(Var, Tail, Res).
%%%%%%%% Avaliar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Definicao de avaliacao de valores inteiros
avaliar(_, valor_inteiro(X), valor_inteiro(X)) :- integer(X).

%Definicao de avaliacao de booleano
avaliar(_, valor_booleano(true), valor_booleano(true)) :- !.
avaliar(_, valor_booleano(false), valor_booleano(false)) :- !.
avaliar(_, valor_booleano(_), erro_tipo).

% Logica de avaliacao e verificacao de tipos para Soma
avaliar(Gamma, soma(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Soma is X + Y,
    Res = valor_inteiro(Soma).

avaliar(_, soma(_, _), erro_tipo).

% Logica de avaliacao e verificacao de tipos para Subtracao
avaliar(Gamma, subtracao(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Sub is X - Y,
    Res = valor_inteiro(Sub).

avaliar(_, subtracao(_, _), erro_tipo).

% Logica de avaliacao e verificacao de tipos para Divisao
avaliar(Gamma, divisao(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Div is X / Y,
    Res = valor_inteiro(Div).

avaliar(_, divisao(_, _), erro_tipo).

% Logica de avaliacao e verificacao de tipos para Multiplicacao
avaliar(Gamma, multiplicacao(LHS, RHS), Res) :-
    avaliar(Gamma, LHS, valor_inteiro(X)),
    avaliar(Gamma, RHS, valor_inteiro(Y)),!,
    Mult is X * Y,
    Res = valor_inteiro(Mult).

avaliar(_, multiplicacao(_, _), erro_tipo).


avaliar(Gamma, var(Var), variavel_nao_declarada) :-
    pesquisarAmbiente(var(Var), Gamma, variavel_nao_declarada),!.

avaliar(Gamma, var(Var), Res) :-
    pesquisarAmbiente(var(Var), Gamma, Exp),
    avaliar(Gamma, Exp, Res).

% Logica de avaliacao e verificacao de tipos para let
avaliar(Gamma, let(X, Exp1, Exp2), Res) :-
    % Deve-se avaliar a expressao nomeada antes (inntermost)
    avaliar(Gamma, Exp1, AvalE1),
    avaliar([(X, AvalE1)|Gamma], Exp2, Res).

avaliar(_, lambda((Arg,Tipo1),Tipo2 ,Exp), lambda((Arg,Tipo1),Tipo2 ,Exp)).

avaliar(Gamma, aplicacao(Exp1, Exp2), Res) :-
    avaliar(Gamma, Exp1, lambda((Arg,_),_ ,Exp)),!,
    avaliar(Gamma, Exp2, Valor),
    avaliar([(Arg, Valor)|Gamma] , Exp, Res).

avaliar(_, aplicacao(_, _), aplicacao_requer_exp_lambda).

pesquisarAmbiente(_, [], variavel_nao_encontrada).
pesquisarAmbiente(Var, [(Var, Exp)|_], Exp) :- !.
pesquisarAmbiente(Var, [_|Tail], Res) :- pesquisarAmbiente(Var, Tail, Res).
