```prolog
% Base de conocimiento de un sistema experto médico

% Reglas generales

enfermedad(gripe) :-
    sintoma(fiebre),
    sintoma(tos),
    sintoma(dolor_muscular).

enfermedad(resfriado) :-
    sintoma(congestion_nasal),
    sintoma(estornudos),
    sintoma(dolor_de_garganta).

enfermedad(neumonia) :-
    sintoma(dificultad_para_respirar),
    sintoma(tos),
    sintoma(fiebre_alta).

enfermedad(asma) :-
    sintoma(dificultad_para_respirar),
    sintoma(tos),
    sintoma(sibilancia).

enfermedad(alergia) :-
    sintoma(estornudos),
    sintoma(congestion_nasal),
    sintoma(picor_de_ojos).

% Reglas específicas

sintoma(fiebre) :-
    temperatura > 38.5.

sintoma(tos) :-
    frecuencia_tos > 5.

sintoma(dolor_muscular) :-
    intensidad_dolor > 5.

sintoma(congestion_nasal) :-
    intensidad_congestion > 5.

sintoma(estornudos) :-
    frecuencia_estornudos > 5.

sintoma(dolor_de_garganta) :-
    intensidad_dolor_garganta > 5.

sintoma(dificultad_para_respirar) :-
    frecuencia_respiracion > 20.

sintoma(sibilancias) :-
    intensidad_sibilancia > 5.

sintoma(picor_de_ojos) :-
    intensidad_picor_ojos > 5.

% Consultas

enfermedad(Enfermedad) :-
    findall(Enfermedad, enfermedad(Enfermedad), Enfermedades),
    length(Enfermedades, 1).

diagnostico(Sintomas) :-
    findall(Enfermedad, (miembro(Sintoma, Sintomas), enfermedad(Enfermedad, Sintoma)), Enfermedades),
    length(Enfermedades, 1).

tratamiento(Enfermedad) :-
    findall(Tratamiento, enfermedad(Enfermedad, Tratamiento), Tratamientos),
    length(Tratamientos, 1).

```

Explicación del código:

* El código define una base de conocimiento de un sistema experto médico.
* Las reglas generales definen las enfermedades en función de sus síntomas.
* Las reglas específicas definen los síntomas en función de sus medidas.
* Las consultas permiten al usuario consultar al sistema experto para obtener un diagnóstico y tratamiento.

El código es complejo porque:

* Utiliza una gran cantidad de reglas.
* Las reglas son interdependientes.
* El código es modular, lo que permite añadir nuevas reglas fácilmente.
* El código es extensible, lo que permite añadir nuevos síntomas y enfermedades fácilmente.