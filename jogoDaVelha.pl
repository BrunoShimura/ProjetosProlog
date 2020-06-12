% ==============================================================================
% Grupo
% ==============================================================================

% Bruno Anthony Shimura 	RA:577928.
% André Vaz Miyagi			RA:577715.
% Gabriel Martinez			RA:581771.
% Enzo Dal Evedove			RA:579424.

% ==============================================================================
% declara bibliotecas 
% ==============================================================================
use_module(library(apply)).
use_module(library(random)).
use_module(library(tabling), [tabela/1]).

% ==============================================================================
% declara jogadores
% ==============================================================================
jogador(x).
jogador(o).
outro_jogador(x, o).
outro_jogador(o, x).

% ==============================================================================
% define as posicoes validas
% ==============================================================================
posicoes_validas(P) :- P >= 0, 2 >= P, !.
posicoes_validas(P) :-
    format("Posição Válida: ~w", [P]),
    nl,
    fail.

cada(0).
cada(1).
cada(2).

% ==============================================================================
% define quem ganha
% ==============================================================================
ganhador(X, Estado) :- tres_em_uma_linha(X, Estado), X \= empty.
ganhador(X, Estado) :- tres_em_uma_coluna(X, Estado), X \= empty.
ganhador(X, Estado) :- tres_em_uma_diagonal(X, Estado), X \= empty.

% ==============================================================================
%  linhas
% ==============================================================================
laco(Estado) :-
    flatten(Estado, FlatEstado),
    not(ganhador(_, Estado)),
    not(member(empty, FlatEstado)).

tres_em_uma_linha(X, Estado) :- linhas(Estado, [X, X, X]).

linhas(Estado, R) :- nth0(0, Estado, R).
linhas(Estado, R) :- nth0(1, Estado, R).
linhas(Estado, R) :- nth0(2, Estado, R).

% ==============================================================================
% diagonal
% ==============================================================================
tres_em_uma_diagonal(X, Estado) :-
    nth0(0, Estado, R0),
    nth0(1, Estado, R1),
    nth0(2, Estado, R2),
    nth0(0, R0, X),
    nth0(1, R1, X),
    nth0(2, R2, X).

tres_em_uma_diagonal(X, Estado) :-
    nth0(0, Estado, R0),
    nth0(1, Estado, R1),
    nth0(2, Estado, R2),
    nth0(2, R0, X),
    nth0(1, R1, X),
    nth0(0, R2, X).

% ==============================================================================
% coluna
% ==============================================================================
tres_em_uma_coluna(X, Estado) :-
    nth0(0, Estado, R0),
    nth0(1, Estado, R1),
    nth0(2, Estado, R2),
    cada(I),
    nth0(I, R0, X),
    nth0(I, R1, X),
    nth0(I, R2, X).

% ==============================================================================
% movimentos
% ==============================================================================
lista_de_movimentos(_, 9, []).

lista_de_movimentos(Estado, I, T) :-
    R is I // 3,
    C is I mod 3,
    not(desocupado(Estado, R, C)),
    Proximo is I + 1,
    lista_de_movimentos(Estado, Proximo, T),
    !.

lista_de_movimentos(Estado, I, [H|T]) :-
    R is I // 3,
    C is I mod 3,
    H = [R,C],
    Proximo is I + 1,
    lista_de_movimentos(Estado, Proximo, T),
    !.

lista_de_movimentos(S, M) :-
    lista_de_movimentos(S, 0, M).

% ==============================================================================
% define os pontos
% ==============================================================================
ponto(Estado, Jogador, Pontos) :-
    jogador(OutroJogador),
    OutroJogador \= Jogador,
    ganhador(OutroJogador, Estado),
    Pontos is -1,
    !.

ponto(Estado, Jogador, Pontos) :-
    ganhador(Jogador, Estado),
    jogador(Jogador),
    Pontos is 1.

ponto(Estado, _, Pontos) :-
    laco(Estado),
    Pontos is 0.

ponto(Estado, Jogador, Pontos) :-
    not(ganhador(_, Estado)),
    not(laco(Estado)),
    lista_de_movimentos(Estado, Move),
    pontuacao_de_cada(Estado, Jogador, Marcados, Move),
    escolhe(Marcados, [Pontos, _, _]).

% ==============================================================================
% define as pontuações 
% ==============================================================================
pontuacao_de_cada(_, _, [], []).

pontuacao_de_cada(Estado, Jogador, [[S,R,C]|L], [[R,C]|T]) :-
    modifica(Estado, 0, R, C, Jogador, R0),
    modifica(Estado, 1, R, C, Jogador, R1),
    modifica(Estado, 2, R, C, Jogador, R2),
    N = [ R0, R1, R2],
    outro_jogador(Jogador, OutroJogador),
    ponto(N, OutroJogador, S_other),
    S is -S_other,
    pontuacao_de_cada(Estado, Jogador, L, T),
    !.

% ==============================================================================
% formata entrada de dados da posição
% ==============================================================================
formata(V, Formatado) :-
    V = x, Formatado = x.

formata(V, Formatado) :-
    V = o, Formatado = o.

formata(V, Formatado) :-
    V = empty, Formatado = ' '.

% ==============================================================================
% estado do jogo
% ==============================================================================
print_Estado(Estado) :-
    write("Estado do Jogo:"),
    nl,
    linhas(Estado, Linha),
    print_Linha(Linha),
    fail.

% ==============================================================================
% print do jogo da velha
% ==============================================================================
print_Linha(Linha) :-
    nth0(0, Linha, V0_raw), formata(V0_raw, V0),
    nth0(1, Linha, V1_raw), formata(V1_raw, V1),
    nth0(2, Linha, V2_raw), formata(V2_raw, V2),
    format("~a | ~a | ~a", [V0, V1, V2]),
    nl.

% ==============================================================================
% espaços desocupados
% ==============================================================================
desocupado(Estado, Linha, Col) :-
    nth0(Linha, Estado, LinhaValue),
    nth0(Col, LinhaValue, Spot),
    Spot = empty.

start_Estado(Estado) :-
    Estado = [[empty,empty,empty], [empty,empty,empty], [empty,empty,empty]].

% ==============================================================================
% print das posições validas
% ==============================================================================
print_cada_move(_, []) :- fail.

print_cada_move(Jogador, [[R,C]|T]) :-
    format("~a posicao vailida (Linha, Coluna): ~d, ~d", [Jogador, R, C]),
    nl,
    print_cada_move(Jogador, T).

% ==============================================================================
% escolhe quem joga 
% ==============================================================================
primeiro([], []).

primeiro([A|B], [A_primeiro|B_escolhido]) :-
    nth0(0, A, A_primeiro),
    primeiro(B, B_escolhido).

comeca_com(B, L) :- nth0(0, L, B).

% ==============================================================================
% escolhe posiçãos
% ==============================================================================
escolhe(Marcados, [S, R, C]) :-
    msort(Marcados, SortedMarcados),
    primeiro(SortedMarcados, ClasificaPontos),
    max_list(ClasificaPontos, MaxPontos),
    include(call(comeca_com, MaxPontos), Marcados, MoveFiltrado),
    random_member([S, R, C], MoveFiltrado).

% ==============================================================================
% vez de qual jogador
% ==============================================================================
revezar(Estado, _, _) :-
    print_Estado(Estado).

revezar(Estado, P, _) :-
    lista_de_movimentos(Estado, Move),
    print_cada_move(P, Move).

revezar(Estado, AtualJogador, ProximoEstado) :-
    AtualJogador = o,
    lista_de_movimentos(Estado, Move),
    pontuacao_de_cada(Estado, AtualJogador, Marcados, Move),
    escolhe(Marcados, [_, R,C]),
    revezar(Estado, AtualJogador, R, C, ProximoEstado).

revezar(Estado, AtualJogador, ProximoEstado) :-
    AtualJogador = x,
    format("Escolhe linha Jogador ~a: ", [AtualJogador]),
	read(Linha),
    format("Escolhe coluna Jogador ~a: ", [AtualJogador]),
    read(Col),
    posicoes_validas(Linha),
    posicoes_validas(Col),
    desocupado(Estado, Linha, Col),
    revezar(Estado, AtualJogador, Linha, Col, ProximoEstado).

revezar(Estado, AtualJogador, Linha, Col, ProximoEstado) :-
    modifica(Estado, 0, Linha, Col, AtualJogador, R0),
    modifica(Estado, 1, Linha, Col, AtualJogador, R1),
    modifica(Estado, 2, Linha, Col, AtualJogador, R2),
    ProximoEstado = [ R0, R1, R2].

% ==============================================================================
% entra valor
% ==============================================================================
modifica(Estado, Linha, DesiredChangeLinha, _, _, LinhaOut) :-
    Linha \= DesiredChangeLinha,
    nth0(Linha, Estado, LinhaOut).

modifica(Estado, Linha, DesiredChangeLinha, Col, Jogador, LinhaOut) :-
    Linha = DesiredChangeLinha,
    Col = 0,
    nth0(Linha, Estado, LinhaEstadoOut),
    nth0(1, LinhaEstadoOut, C1),
    nth0(2, LinhaEstadoOut, C2),
    LinhaOut = [Jogador, C1, C2].

modifica(Estado, Linha, DesiredChangeLinha, Col, Jogador, LinhaOut) :-
    Linha = DesiredChangeLinha,
    Col = 1,
    nth0(Linha, Estado, LinhaEstadoOut),
    nth0(0, LinhaEstadoOut, C0),
    nth0(2, LinhaEstadoOut, C2),
    LinhaOut = [C0, Jogador, C2].

modifica(Estado, Linha, DesiredChangeLinha, Col, Jogador, LinhaOut) :-
    Linha = DesiredChangeLinha,
    Col = 2,
    nth0(Linha, Estado, LinhaEstadoOut),
    nth0(0, LinhaEstadoOut, C0),
    nth0(1, LinhaEstadoOut, C1),
    LinhaOut = [C0, C1, Jogador].

% ==============================================================================
% finaliza programa
% ==============================================================================
die(X) :- write("Programa Finalizado"), nl, die_(X).
die_(X) :- die_(X).

% ==============================================================================
% loop
% ==============================================================================
faz_loop(Estado, _) :-
    ganhador(_, Estado),
    print_Estado(Estado).

faz_loop(Estado, _) :-
    ganhador(P, Estado),
    format("Jogador ~a Ganhou!", [P]),
    nl,
    die(_).

faz_loop(Estado, _) :-
    laco(Estado),
    format("Deu Velha!"),
    nl,
    die(_).

faz_loop(Estado, Jogador) :-
  Jogador = x,
  revezar(Estado, Jogador, ProximoEstado),
  faz_loop(ProximoEstado, o).

faz_loop(Estado, Jogador) :-
    Jogador = o,
    revezar(Estado, Jogador, ProximoEstado),
    faz_loop(ProximoEstado, x).

% ==============================================================================
% main
% ==============================================================================
main :-
    start_Estado(Estado),
    faz_loop(Estado, x).

tabela(ganhador/3).
tabela(ponto/3).
tabela(laco/1).
