```smalltalk

(
    "Clase Persona"

    Persona clase

    "Atributos"
    nombre: String,
    edad: Número.

    "Constructor"
    nuevo [:nombre :edad]
        [super nuevo]
        nombre: nombre,
        edad: edad.

    "Métodos"
    nombre [&bloque]
        [bloque valor: nombre].

    edad [&bloque]
        [bloque valor: edad].

    presentarse
        [
            'Hola, mi nombre es ',
            nombre,
            ' y tengo ',
            edad,
            ' años.'
        ] print.
) ejecutado.

(
    "Clase Estudiante"

    Estudiante clase

    "Atributos Heredados"
    nombre: String,
    edad: Número,

    "Atributos Propios"
    notaPromedio: Número.

    "Constructor"
    nuevo [:nombre :edad :notaPromedio]
        [super nuevo nombre: nombre edad: edad]
        notaPromedio: notaPromedio.

    "Métodos Propios"
    notaPromedio [&bloque]
        [bloque valor: notaPromedio].

    estudiar
        [
            'Estoy estudiando.',
            (notaPromedio + 1) notaPromedio:.
        ] print.
) ejecutado.

(
    "Clase Profesor"

    Profesor clase

    "Atributos Heredados"
    nombre: String,
    edad: Número,

    "Atributos Propios"
    materia: String.

    "Constructor"
    nuevo [:nombre :edad :materia]
        [super nuevo nombre: nombre edad: edad]
        materia: materia.

    "Métodos Propios"
    materia [&bloque]
        [bloque valor: materia].

    enseñar
        [
            'Estoy enseñando ',
            materia,
            '.'
        ] print.
) ejecutado.

(
    "Creación de Objetos"

    persona1: Persona nuevo nombre: 'Juan' edad: 20,
    estudiante1: Estudiante nuevo nombre: 'María' edad: 22 notaPromedio: 8,
    profesor1: Profesor nuevo nombre: 'Pedro' edad: 30 materia: 'Matemáticas'.
) ejecutado.

(
    "Uso de Métodos"

    persona1 nombre: [nombre print],
    persona1 edad: [edad print].

    estudiante1 nombre: [nombre print],
    estudiante1 edad: [edad print],
    estudiante1 notaPromedio: [notaPromedio print].

    profesor1 nombre: [nombre print],
    profesor1 edad: [edad print],
    profesor1 materia: [materia print].

    estudiante1 estudiar.
    profesor1 enseñar.
) ejecutado.

```

Explicación del Código:

* **Clase Persona:** Define la clase Persona con dos atributos, nombre y edad. Incluye métodos para obtener y establecer estos atributos, así como un método llamado `presentarse` que imprime un mensaje de presentación.


* **Clase Estudiante:** Define la clase Estudiante que hereda de Persona. Agrega un atributo adicional, `notaPromedio`, y métodos para obtener y establecer este atributo. También incluye un método llamado `estudiar` que imprime un mensaje y aumenta la nota promedio en 1.


* **Clase Profesor:** Define la clase Profesor que hereda de Persona. Agrega un atributo adicional, `materia`, y métodos para obtener y establecer este atributo. También incluye un método llamado `enseñar` que imprime un mensaje y muestra la materia que el profesor está enseñando.


* **Creación de Objetos:** Se crean tres objetos: `persona1`, `estudiante1` y `profesor1`, utilizando los constructores correspondientes.


* **Uso de Métodos:** Se utilizan los métodos definidos en las clases para obtener y establecer los atributos de los objetos, así como para llamar a los métodos `estudiar` y `enseñar`.


Este código muestra cómo crear clases y objetos en Smalltalk, así como cómo utilizar los métodos definidos en las clases para interactuar con los objetos. El código también demuestra el uso de herencia y polimorfismo, ya que las clases Estudiante y Profesor heredan de la clase Persona y comparten algunos métodos y atributos.