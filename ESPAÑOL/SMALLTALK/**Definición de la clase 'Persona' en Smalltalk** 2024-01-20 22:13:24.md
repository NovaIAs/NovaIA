```smalltalk
;; Definir una clase llamada 'Persona' con cuatro variables de instancia: 'nombre', 'edad',
;; 'ocupación' y 'hobbies'.

Persona clase [
    nombre: String.
    edad: Natural.
    ocupación: String.
    aficiones: Sequence[String].

    ;; Definir un método de clase llamado 'crear' que toma un nombre, edad, ocupación y aficiones
    ;; y devuelve un nuevo objeto Persona con esas variables de instancia.

    clase crearNombre: unNombre unaEdad: unaEdad unaOcupación: unaOcupación unasAficiones: unasAficiones [
        (super crear) nombre: unNombre; edad: unaEdad; ocupación: unaOcupación;
        aficiones: unasAficiones
    ].

    ;; Definir un método de instancia llamado 'nombreCompleto' que devuelve el nombre completo de
    ;; la persona (nombre y apellidos).

    nombreCompleto [
        ^self nombre
    ].

    ;; Definir un método de instancia llamado 'edadAnos' que devuelve la edad de la persona en
    ;; años.

    edadAnos [
        ^self edad
    ].

    ;; Definir un método de instancia llamado 'ocupaciónActual' que devuelve la ocupación actual
    ;; de la persona.

    ocupaciónActual [
        ^self ocupación
    ].

    ;; Definir un método de instancia llamado 'aficionesFavoritas' que devuelve las aficiones
    ;; favoritas de la persona.

    aficionesFavoritas [
        ^self aficiones
    ].

    ;; Definir un método de instancia llamado 'mostrarInformación' que imprime la información
    ;; de la persona en la consola.

    mostrarInformación [
        'Nombre: ', self nombreCompleto println.
        'Edad: ', self edadAnos println.
        'Ocupación: ', self ocupaciónActual println.
        'Aficiones: ', self aficionesFavoritas println
    ].
].

;; Crear dos objetos Persona: 'persona1' y 'persona2'.

persona1 := Persona crearNombre: 'Juan Pérez' unaEdad: 25 unaOcupación: 'Ingeniero de Software'
            unasAficiones: #(Ciclismo Natación Viajes).

persona2 := Persona crearNombre: 'María Gómez' unaEdad: 30 unaOcupación: 'Médica' unasAficiones:
            #(Leer Cine Música).

;; Mostrar la información de cada persona en la consola.

persona1 mostrarInformación.
persona2 mostrarInformación.
```

Este código define una clase llamada `Persona` con cuatro variables de instancia: `nombre`, `edad`, `ocupación` y `aficiones`. También define varios métodos de clase y de instancia, incluyendo un método `crear` que toma un nombre, edad, ocupación y aficiones y devuelve un nuevo objeto `Persona` con esas variables de instancia, un método `nombreCompleto` que devuelve el nombre completo de la persona, un método `edadAnos` que devuelve la edad de la persona en años, un método `ocupaciónActual` que devuelve la ocupación actual de la persona, un método `aficionesFavoritas` que devuelve las aficiones favoritas de la persona, y un método `mostrarInformación` que imprime la información de la persona en la consola.

El código también crea dos objetos `Persona`: `persona1` y `persona2`, y muestra la información de cada persona en la consola.