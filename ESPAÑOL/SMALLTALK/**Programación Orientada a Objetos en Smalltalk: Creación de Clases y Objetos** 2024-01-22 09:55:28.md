```smalltalk
**Definición de la clase Persona**

Clase Persona

    **Propiedades**

    nombre: ''
    edad: 0
    peso: 0

    **Métodos**

    inicializar: unNombre unaEdad unPeso
        "Crea una nueva instancia de Persona con los valores de las variables dadas."

        nombre := unNombre.
        edad := unaEdad.
        peso := unPeso.

    nombre
        "Devuelve el nombre de la persona."

        ^nombre.

    edad
        "Devuelve la edad de la persona."

        ^edad.

    peso
        "Devuelve el peso de la persona."

        ^peso.

    saludar
        "Imprime un saludo de la persona."

        Transcript show: '¡Hola! Mi nombre es ', nombre,
            ' y tengo ', edad, ' años y ', peso, ' kilos.'.
        Transcript cr.

**Definición de la clase Trabajador**

Clase Trabajador extiende: Persona

    **Propiedades**

    ocupacion: ''
    salario: 0

    **Métodos**

    inicializar: unNombre unaEdad unPeso unaOcupacion unSalario
        "Crea una nueva instancia de Trabajador con los valores de las variables dadas."

        super inicializar: unNombre unaEdad unPeso.
        ocupacion := unaOcupacion.
        salario := unSalario.

    ocupacion
        "Devuelve la ocupacion del trabajador."

        ^ocupacion.

    salario
        "Devuelve el salario del trabajador."

        ^salario.

    presentarse
        "Imprime una presentación formal del trabajador."

        Transcript show: 'Hola, mi nombre es ', nombre, ' y soy un ', ocupacion, '.
            Gano ', salario, ' al mes..'
        Transcript cr.

**Definición de la clase Estudiante**

Clase Estudiante extiende: Persona

    **Propiedades**

    universidad: ''
    carrera: ''

    **Métodos**

    inicializar: unNombre unaEdad unPeso unaUniversidad unaCarrera
        "Crea una nueva instancia de Estudiante con los valores de las variables dadas."

        super inicializar: unNombre unaEdad unPeso.
        universidad := unaUniversidad.
        carrera := unaCarrera.

    universidad
        "Devuelve la universidad del estudiante."

        ^universidad.

    carrera
        "Devuelve la carrera del estudiante."

        ^carrera.

    presentarse
        "Imprime una presentación formal del estudiante."

        Transcript show: 'Hola, mi nombre es ', nombre,
            ' y soy estudiante de ', carrera, ' en ', universidad, '.'
        Transcript cr.

**Ejemplo de uso**

unaPersona := Persona nueva.
unaPersona inicializar: 'Juan' 20 70.
unaPersona saludar.

unTrabajador := Trabajador nuevo.
unTrabajador inicializar: 'María' 30 90 'Doctora' 50000.
unTrabajador presentarse.

unaEstudiante := Estudiante nueva.
unaEstudiante inicializar: 'Ana' 22 60 'UNED' 'Ingeniería Informática'.
unaEstudiante presentarse.
```

Explicación del código:

* El código define tres clases: Persona, Trabajador y Estudiante.
* La clase Persona tiene tres propiedades (nombre, edad y peso) y tres métodos (inicializar, nombre, edad y peso).
* La clase Trabajador extiende la clase Persona y agrega dos propiedades (ocupación y salario) y dos métodos (inicializar, ocupación, salario y presentarse).
* La clase Estudiante extiende la clase Persona y agrega dos propiedades (universidad y carrera) y dos métodos (inicializar, universidad, carrera y presentarse).
* El código crea tres instancias de estas clases y llama a sus respectivos métodos para imprimir un saludo o presentación.

Este código es un ejemplo de cómo crear clases y objetos en Smalltalk.