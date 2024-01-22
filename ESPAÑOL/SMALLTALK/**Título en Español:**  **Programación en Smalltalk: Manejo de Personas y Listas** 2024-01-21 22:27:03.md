```smalltalk

;; Estructura de datos para representar una persona
Persona clase
    nombre: String.
    edad: Number.
    cédula: String.
    dirección: String.

    inicializarNombre: nombre edad: edad cédula: cédula dirección: dirección
        nombre := nombre.
        edad := edad.
        cédula := cédula.
        dirección := dirección.

    nombre
        ^nombre.

    edad
        ^edad.

    cédula
        ^cédula.

    dirección
        ^dirección.

    toString
        ^nombre , " - edad: " , edad , " - cédula: " , cédula , " - dirección: " , dirección.
;; Fin de la clase Persona

;; Estructura de datos para representar una lista de personas
ListaDePersonas clase
    personas: Array ordenado.

    inicializar
        personas := Array nuevo.

    agregarPersona: persona
        personas añadir: persona.

    removerPersona: persona
        personas remover: persona.

    obtenerPersonaPorÍndice: índice
        ^personas en: índice.

    obtenerPersonaPorNombre: nombre
        | i |
        i := 1.
        [personas en: i nombre = nombre]
            siVerdadero: [^personas en: i]
            siFalso: [i := i + 1].

    obtenerPersonasPorEdad: edad
        | resultado |
        resultado := Array nuevo.
        1 a: personas tamaño hacer: [:i |
            (personas en: i edad = edad)
                siVerdadero: [resultado añadir: personas en: i]
        ].
        ^resultado.

    toString
        | i |
        i := 1.
        ["Lista de Personas:" , saltoDeLínea] concatenar:
            [personas tamaño a: 1 por: -1 hacer: [:j |
                (personas en: j toString) concatenar: saltoDeLínea
            ]].
;; Fin de la clase ListaDePersonas

"Personas" objeto
    lista := ListaDePersonas nuevo.

    "Agregar algunas personas a la lista"
    lista agregarPersona: Persona nuevo inicializarNombre: "Juan" edad: 25 cédula: "123456789" dirección: "Calle 123".
    lista agregarPersona: Persona nuevo inicializarNombre: "María" edad: 30 cédula: "987654321" dirección: "Calle 456".
    lista agregarPersona: Persona nuevo inicializarNombre: "Pedro" edad: 18 cédula: "112233445" dirección: "Calle 789".

    "Imprimir la lista de personas"
    lista toString show.

    "Obtener la persona con el nombre "Juan""
    (lista obtenerPersonaPorNombre: "Juan") toString show.

    "Obtener las personas con edad 25"
    (lista obtenerPersonasPorEdad: 25) toString show.
```

**Explicación del código:**

1. **Clase `Persona`:** Se define una clase llamada `Persona` que representa una persona con los siguientes atributos: `nombre`, `edad`, `cédula` y `dirección`. La clase tiene métodos para inicializar los atributos, obtener los valores de los atributos y convertir la persona a una cadena de texto.

2. **Clase `ListaDePersonas`:** Se define una clase llamada `ListaDePersonas` que representa una lista de personas. La clase tiene un atributo llamado `personas` que es una colección de objetos `Persona`. La clase tiene métodos para agregar personas a la lista, remover personas de la lista, obtener una persona por índice o por nombre, obtener todas las personas con una edad determinada y convertir la lista a una cadena de texto.

3. **Objeto `Personas`:** Se crea un objeto llamado `Personas` que es una instancia de la clase `ListaDePersonas`. Se agregan algunas personas a la lista usando el método `agregarPersona`.

4. **Imprimir la lista de personas:** Se utiliza el método `toString` del objeto `Personas` para convertir la lista de personas a una cadena de texto y luego se muestra la cadena de texto en la consola.

5. **Obtener la persona con el nombre "Juan":** Se utiliza el método `obtenerPersonaPorNombre` del objeto `Personas` para obtener la persona con el nombre "Juan" y luego se muestra la cadena de texto de la persona en la consola.

6. **Obtener las personas con edad 25:** Se utiliza el método `obtenerPersonasPorEdad` del objeto `Personas` para obtener todas las personas con edad 25 y luego se muestra la cadena de texto de las personas en la consola.