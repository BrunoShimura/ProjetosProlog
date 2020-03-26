apelido(rodolfo,prof).
apelido(jose,ze).


%exercicio mostra apelido e se nao tiver mostrar o nome 
dialogo :- write('Nome: '), 
			read(X), apelido(X,Y), 
			write('Ola, '), write(Y), nl.
			
		
