```prolog
/* Este programa en Prolog implementa un sistema experto de diagnóstico de enfermedades. */

/* Se definen las reglas del sistema experto. */

enfermedad(gripe, [tos, fiebre, dolor_de_garganta]).
enfermedad(resfriado, [tos, estornudos, congestión_nasal]).
enfermedad(neumonía, [tos, fiebre, dificultad_para_respirar]).
enfermedad(bronquitis, [tos, sibilancias, expectoración]).
enfermedad(asma, [sibilancias, dificultad_para_respirar, opresión_en_el_pecho]).

/* Se define la regla para consultar al usuario sobre los síntomas de la enfermedad. */

consultar_sintomas(Sintomas) :-
  write('¿Tiene tos? (s/n) '),
  read(Respuesta_tos),
  (Respuesta_tos = s -> Sintomas = [tos | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene fiebre? (s/n) '),
  read(Respuesta_fiebre),
  (Respuesta_fiebre = s -> Sintomas = [fiebre | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene dolor de garganta? (s/n) '),
  read(Respuesta_dolor_de_garganta),
  (Respuesta_dolor_de_garganta = s -> Sintomas = [dolor_de_garganta | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene estornudos? (s/n) '),
  read(Respuesta_estornudos),
  (Respuesta_estornudos = s -> Sintomas = [estornudos | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene congestión nasal? (s/n) '),
  read(Respuesta_congestion_nasal),
  (Respuesta_congestion_nasal = s -> Sintomas = [congestion_nasal | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene dificultad para respirar? (s/n) '),
  read(Respuesta_dificultad_para_respirar),
  (Respuesta_dificultad_para_respirar = s -> Sintomas = [dificultad_para_respirar | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene sibilancias? (s/n) '),
  read(Respuesta_sibilancias),
  (Respuesta_sibilancias = s -> Sintomas = [sibilancias | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene expectoración? (s/n) '),
  read(Respuesta_expectoracion),
  (Respuesta_expectoracion = s -> Sintomas = [expectoracion | Sintomas] ; Sintomas = Sintomas),
  write('¿Tiene opresión en el pecho? (s/n) '),
  read(Respuesta_opresion_en_el_pecho),
  (Respuesta_opresion_en_el_pecho = s -> Sintomas = [opresion_en_el_pecho | Sintomas] ; Sintomas = Sintomas).

/* Se define la regla para diagnosticar la enfermedad. */

diagnosticar(Sintomas, Enfermedad) :-
  enfermedad(Enfermedad, Sintomas).

/* Se define la regla para mostrar el diagnóstico al usuario. */

mostrar_diagnostico(Enfermedad) :-
  write('Su diagnóstico es: '),
  write(Enfermedad),
  nl.

/* Se define la regla principal del programa. */

main :-
  consultar_sintomas(Sintomas),
  diagnosticar(Sintomas, Enfermedad),
  mostrar_diagnostico(Enfermedad).
```

Explicación del código:

* El programa comienza con la definición de las reglas del sistema experto. Las reglas se definen utilizando el predicado `enfermedad/2`, que toma dos argumentos: el nombre de la enfermedad y la lista de síntomas asociados a esa enfermedad.
* A continuación, se define la regla para consultar al usuario sobre los síntomas de la enfermedad. La regla `consultar_sintomas/1` toma un argumento: una lista de síntomas. La regla escribe en la pantalla una serie de preguntas sobre los síntomas de la enfermedad y lee las respuestas del usuario. Las respuestas del usuario se utilizan para actualizar la lista de síntomas.
* La regla `diagnosticar/2` toma dos argumentos: una lista de síntomas y el nombre de una enfermedad. La regla comprueba si existe alguna regla que relacione la lista de síntomas con el nombre de la enfermedad. Si existe una regla de este tipo, entonces la regla concluye que el usuario tiene la enfermedad.
* La regla `mostrar_diagnostico/1` toma un argumento: el nombre de una enfermedad. La regla escribe en la pantalla el diagnóstico del usuario.
* Finalmente, se define la regla principal del programa, `main/0`. La regla `main/0` llama a las reglas `consultar_sintomas/1`, `diagnosticar/2` y `mostrar_diagnostico/1` para consultar al usuario sobre los síntomas de la enfermedad, diagnosticar la enfermedad y mostrar el diagnóstico al usuario.