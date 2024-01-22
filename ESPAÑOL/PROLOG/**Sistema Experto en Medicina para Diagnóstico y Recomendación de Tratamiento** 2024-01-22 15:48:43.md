```prolog
% Base de conocimiento de un sistema experto en medicina

% Definición de los síntomas
sintoma(fiebre, temperatura_alta).
sintoma(tos, expectoracion).
sintoma(dolor_de_garganta, picor_de_garganta).
sintoma(estornudos, mucosidad_nasal).
sintoma(goteo_nasal, mucosidad_nasal).
sintoma(fatiga, cansancio).
sintoma(malestar_general, malestar).

% Definición de las enfermedades
enfermedad(gripe, [fiebre, tos, dolor_de_garganta, estornudos, goteo_nasal]).
enfermedad(resfriado, [tos, estornudos, goteo_nasal]).
enfermedad(faringitis, [dolor_de_garganta, fiebre]).
enfermedad(rinitis, [estornudos, goteo_nasal]).
enfermedad(sinusitis, [dolor_de_garganta, fiebre, malestar_general]).

% Regla que permite diagnosticar una enfermedad a partir de los síntomas
enfermedad(Enfermedad, Sintomas) :-
    enfermedad(Enfermedad, Sintomas_Enfermedad),
    todos_los_sintomas(Sintomas, Sintomas_Enfermedad).

% Regla que comprueba si todos los síntomas de una enfermedad están presentes
todos_los_sintomas([], []).
todos_los_sintomas([Sintoma|Resto], [Sintoma|Resto_Sintomas]) :-
    sintoma(Sintoma, _),
    todos_los_sintomas(Resto, Resto_Sintomas).

% Regla que permite recomendar un tratamiento para una enfermedad
tratamiento(Enfermedad, Tratamiento) :-
    enfermedad(Enfermedad, _),
    tratamiento(Enfermedad, Tratamiento).

% Definición de los tratamientos
tratamiento(gripe, paracetamol).
tratamiento(resfriado, vitamina_C).
tratamiento(faringitis, antibioticos).
tratamiento(rinitis, antihistaminicos).
tratamiento(sinusitis, antibioticos, descongestivos).

% Regla que permite generar una consulta para obtener los síntomas del paciente
consulta :-
    write('¿Tiene fiebre? '),
    read(Fiebre),
    write('¿Tiene tos? '),
    read(Tos),
    write('¿Tiene dolor de garganta? '),
    read(Dolor_de_garganta),
    write('¿Tiene estornudos? '),
    read(Estornudos),
    write('¿Tiene goteo nasal? '),
    read(Goteo_nasal),
    write('¿Tiene fatiga? '),
    read(Fatiga),
    write('¿Tiene malestar general? '),
    read(Malestar_general),

    % Generamos una lista con los síntomas del paciente
    Sintomas = [Fiebre, Tos, Dolor_de_garganta, Estornudos, Goteo_nasal, Fatiga, Malestar_general],

    % Eliminamos los síntomas que no están presentes
    Sintomas_Filtrados = [Sintoma | Resto] :-
        sintoma(Sintoma, _),
        member(Sintoma, Sintomas),

    % Diagnosticamos la enfermedad del paciente
    enfermedad(Enfermedad, Sintomas_Filtrados),

    % Recomendamos un tratamiento para la enfermedad del paciente
    tratamiento(Enfermedad, Tratamiento),

    % Mostramos el diagnóstico y el tratamiento al paciente
    write('Usted tiene '),
    write(Enfermedad),
    write('. El tratamiento recomendado es '),
    write(Tratamiento).
```

Este código es un sistema experto en medicina que permite diagnosticar y recomendar un tratamiento para una enfermedad a partir de los síntomas del paciente. El código está escrito en PROLOG, un lenguaje de programación lógico.

El código consta de varias partes:

* **Base de conocimiento:** Esta parte contiene la información sobre los síntomas, las enfermedades y los tratamientos.
* **Reglas:** Esta parte contiene las reglas que permiten diagnosticar una enfermedad a partir de los síntomas y recomendar un tratamiento para una enfermedad.
* **Consulta:** Esta parte contiene el código que permite generar una consulta para obtener los síntomas del paciente.

Para utilizar el sistema experto, el usuario debe seguir los siguientes pasos:

1. Ejecutar el código en PROLOG.
2. Responder a las preguntas de la consulta para proporcionar los síntomas del paciente.
3. El sistema experto diagnosticará la enfermedad del paciente y recomendará un tratamiento.

El sistema experto puede ser utilizado por médicos y otros profesionales de la salud para ayudar a diagnosticar y tratar enfermedades.