```prolog
% Definición de las reglas para el sistema experto.

% regla_1(enfermedad, sintomas).
regla_1(gripe, tos).
regla_1(gripe, fiebre).
regla_1(gripe, dolor_de_garganta).

regla_1(resfriado, tos).
regla_1(resfriado, estornudos).
regla_1(resfriado, congestión_nasal).

regla_1(sinusitis, dolor_de_cabeza).
regla_1(sinusitis, congestión_nasal).
regla_1(sinusitis, fiebre).

regla_1(amigdalitis, dolor_de_garganta).
regla_1(amigdalitis, fiebre).
regla_1(amigdalitis, dificultad_para_tragar).

% regla_2(síntoma, enfermedad).
regla_2(tos, gripe).
regla_2(tos, resfriado).

regla_2(estornudos, resfriado).

regla_2(congestión_nasal, sinusitis).
regla_2(congestión_nasal, resfriado).

regla_2(fiebre, gripe).
regla_2(fiebre, sinusitis).
regla_2(fiebre, amigdalitis).

regla_2(dolor_de_garganta, amigdalitis).
regla_2(dolor_de_garganta, gripe).

regla_2(dolor_de_cabeza, sinusitis).

regla_2(dificultad_para_tragar, amigdalitis).

% regla_3(enfermedad, tratamiento).
regla_3(gripe, reposo).
regla_3(gripe, medicamentos_antivirales).

regla_3(resfriado, reposo).
regla_3(resfriado, descongestionantes).

regla_3(sinusitis, antibióticos).
regla_3(sinusitis, descongestionantes).

regla_3(amigdalitis, antibióticos).
regla_3(amigdalitis, reposo).

% Consulta al sistema experto.

% enfermedad(Enfermedad).
enfermedad(Enfermedad) :-
    sintoma(Sintoma),
    regla_2(Sintoma, Enfermedad).

% tratamiento(Tratamiento).
tratamiento(Tratamiento) :-
    enfermedad(Enfermedad),
    regla_3(Enfermedad, Tratamiento).

% Explicación del código.

% Las reglas del sistema experto se definen utilizando la sintaxis:

% regla_(Número, Premisa, Conclusión).

% Donde:

% Número es el número de la regla.
% Premisa es la condición que debe cumplirse para que se aplique la regla.
% Conclusión es la conclusión que se extrae si se cumple la premisa.

% Las consultas al sistema experto se realizan utilizando la sintaxis:

% consulta(Objetivo).

% Donde:

% Objetivo es el objetivo que se quiere alcanzar.

% El sistema experto utiliza las reglas para inferir nuevas conclusiones a partir de los hechos conocidos. El proceso de inferencia se realiza mediante un mecanismo llamado encadenamiento hacia atrás.

% El encadenamiento hacia atrás es un método de resolución de problemas que comienza con el objetivo final y trabaja hacia atrás hasta encontrar un conjunto de hechos que lo satisfacen.

% En el caso de nuestro sistema experto, el objetivo final es encontrar una enfermedad y un tratamiento para un paciente. El sistema experto utiliza las reglas para inferir la enfermedad y el tratamiento a partir de los síntomas del paciente.

```