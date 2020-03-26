genitor(pam, bob).
genitor(tom, bob).
genitor(tom, liz).
genitor(bob, ana).
genitor(bob, pat).
genitor(liz, bill).
genitor(pat, jim).

homem(tom).
homem(bob).
homem(bill).
homem(jim).

mulher(pam).
mulher(liz).
mulher(ana).
mulher(pat).

filho(X,Y) :- genitor(Y,X), homem(X).
filho(X,Y) :- genitor(Y,X), mulher(X).

avo(X,Y) :- genitor(X,Z), genitor(Z,Y), homem(X).
avoh(X,Y) :- genitor(X,Z), genitor(Z,Y), mulher(X).

pai(X,Y) :- genitor(X,Y), homem(X).
mae(X,Y) :- genitor(X,Y), mulher(X).

irmao(X,Y) :- genitor(Z,X), genitor(Z,Y), X\=Y, homem(X).
irma(X,Y) :- genitor(Z,X), genitor(Z,Y), X\=Y, mulher(X).

tio(X,Y) :- irmao(X,Z), genitor(Z,Y).
tia(X,Y) :- irma(X,Z), genitor(Z,Y).

primo(X,Y) :- genitor(Z,X), genitor(W,Y), (irmao(W,Z));irma(W,Z), homem(X).
prima(X,Y) :- genitor(Z,X), genitor(W,Y), (irmao(W,Z));irma(W,Z), mulher(X).

