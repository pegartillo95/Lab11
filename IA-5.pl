% ------------------------------------------------------------------
% Preguntas que devuelven un solo valor:
% ------------------------------------------------------------------

% El sistema deberia ser capaz de contestar a preguntas del tipo

%¿Cual es el/la (<atributo>) de (<nombre>).
% Dime el/la (<atributo>) de (<nombre>).
% Que (<atributo>) tiene (<nombre>).

%----------Preguntas con una respuesta singular---------------
%En estos tres primeros podemos separar las frases en 4 partes:
%-Introduccion {Cual es el/la, Dime el/la, Que}.
%-(<atributo>).
%-Link {de,tiene}.
%-(<nombre>).

pregunta:- 
			read(F),
			ejecucion(F,V),
			write(V).


ejecucion(F,V):- preg(V,F,[]).

preg(Value) --> introduccion(Genero), atributo(Atrib,Genero), unions_1, nombre(Atrib,Value).
preg(Value) --> introduccion(Genero), atributo(Atrib,Genero), unions_2, nombre(Atrib,Value).

% ------------------------------------------------------------------
% Reglas para las preguntas que devuelven un solo valor:
% ------------------------------------------------------------------

atributo(A,Genero) --> [A],
{
	atributo(A,Genero)
}.

nombre(Atrib,Value) --> [N],
{
	empleado(N,Atrib,Value)
}.



%----------------------------------------------------------------------------------------
%  LISTA DE UNIONES
%----------------------------------------------------------------------------------------
unions_1 --> [de].
unions_1 --> [tiene].

unions_2 --> [de].


%----------------------------------------------------------------------------------------
%  LISTA DE ATRIBUTOS
%----------------------------------------------------------------------------------------
atributo(salario,masc).
atributo(departamento,masc).
atributo(edad,fem).
atributo(factoria,fem).

%----------------------------------------------------------------------------------------
%  LISTA DE INTRODUCCIONES
%----------------------------------------------------------------------------------------
introduccion(masc) --> [que]. 
introduccion(fem) --> [que].
introduccion(masc) --> [dime,el].
introduccion(fem) --> [dime, la].
introduccion(masc) --> [cual,es,el].
introduccion(fem) --> [cual,es, la].


%----------------------------------------------------------------------------------------
%  BASE DE DATOS
%----------------------------------------------------------------------------------------

%---------García---------------
nombre --> [garcia].
empleado(garcia, salario, 1000).
empleado(garcia, departamento, biologia).
empleado(garcia, edad, 25).
empleado(garcia, factoria, madrid).

%--------Mendez----------------
nombre --> [mendez].
empleado(mendez, salario, 1300).
empleado(mendez, departamento, informatica).
empleado(mendez, edad, 32).
empleado(mendez, factoria, coruna).

%--------Perez----------------
nombre --> [perez].
empleado(perez, salario, 850).
empleado(perez, departamento, informatica).
empleado(perez, edad, 30).
empleado(perez, factoria, madrid).



%-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



% ------------------------------------------------------------------
% Preguntas que devuelven una lista de nombres:
% ------------------------------------------------------------------

% Dame los nombres de los empleados del departamento de (<departamento>).
% Quiero saber los nombres de los empleados de la factoria de (<ciudad>).

ejecucion(P,L):-setof(V, preg2(V,P,[]),L).

preg2(Nombre) --> comienzo, [los, nombres, de, los, empleados], union(Atributo), [de], busqueda(Atributo,Nombre).

% ------------------------------------------------------------------
% Reglas para las preguntas que devuelven varios valores:
% ------------------------------------------------------------------

busqueda(Atributo,Nombre) --> [V],
{
	empleado(Nombre,Atributo,V)
}.

%----------------------------------------------------------------------------------------
%  COMIENZOS Y UNIONES
%----------------------------------------------------------------------------------------
comienzo --> [dame];[quiero,saber].

union(departamento) --> [del,departamento].
union(factoria) --> [de,la,factoria].