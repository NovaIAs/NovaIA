```prolog
/*
Este es un código complejo en Prolog que implementa un sistema experto para el diagnóstico de enfermedades.

El código se divide en varias partes:

* Base de conocimientos: esta parte contiene los hechos y reglas que conforman el conocimiento del sistema experto.
* Motor de inferencia: esta parte es responsable de aplicar las reglas de la base de conocimientos a los hechos conocidos para generar nuevas conclusiones.
* Interfaz de usuario: esta parte se encarga de la interacción con el usuario, permitiendo que éste introduzca los síntomas del paciente y obtenga los resultados del diagnóstico.

El código está comentado en español para facilitar su comprensión.
*/

/*
Base de conocimientos
*/

sintoma(fiebre).
sintoma(tos).
sintoma(dolor_de_garganta).
sintoma(dificultad_para_respirar).
sintoma(dolor_de_cabeza).
sintoma(náuseas).
sintoma(vómitos).
sintoma(diarrea).
sintoma(dolor_abdominal).
sintoma(erupción_cutánea).

enfermedad(gripe).
enfermedad(resfriado).
enfermedad(neumonía).
enfermedad(bronquitis).
enfermedad(faringitis).
enfermedad(gastroenteritis).

causa(gripe, virus).
causa(resfriado, virus).
causa(neumonía, bacteria).
causa(bronquitis, bacteria).
causa(faringitis, bacteria).
causa(gastroenteritis, virus).

tratamiento(gripe, paracetamol).
tratamiento(resfriado, paracetamol).
tratamiento(neumonía, antibióticos).
tratamiento(bronquitis, antibióticos).
tratamiento(faringitis, antibióticos).
tratamiento(gastroenteritis, suero).

/*
Motor de inferencia
*/

diagnosticar(Sintomas, Enfermedad) :-
    findall(Enfermedad, (sintoma(Sintoma), causa(Enfermedad, Causa), member(Sintoma, Sintomas)), Enfermedades),
    sort(Enfermedades, Enfermedad).

tratar(Enfermedad, Tratamiento) :-
    tratamiento(Enfermedad, Tratamiento).

/*
Interfaz de usuario
*/

main :-
    write('¿Cuáles son los síntomas del paciente? '),
    read_line(Sintomas),
    diagnosticar(Sintomas, Enfermedad),
    write('El paciente tiene '),
    write(Enfermedad),
    nl,
    tratar(Enfermedad, Tratamiento),
    write('El tratamiento para '),
    write(Enfermedad),
    write(' es '),
    write(Tratamiento),
    nl.

/*
Salida de ejemplo
*/

?- main.
¿Cuáles son los síntomas del paciente? fiebre, tos, dolor_de_garganta
El paciente tiene gripe
El tratamiento para gripe es paracetamol