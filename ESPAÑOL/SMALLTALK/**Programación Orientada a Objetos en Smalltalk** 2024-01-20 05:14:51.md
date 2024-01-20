```smalltalk
**Definición de clases**

Clase Persona
  variables de instancia
    nombre: String
    edad: Número
  métodos
    inicializar: (unNombre unaEdad)
      nombre := unNombre.
      edad := unaEdad.

    imprimir
      Transcript show: 'Nombre:', nombre, 'Edad:', edad, '.\n'.

Clase Estudiante
  variables de instancia
    calificación: Número
  métodos
    inicializar: (unNombre unaEdad unaCalificación)
      super inicializar: unNombre unaEdad.
      calificación := unaCalificación.

    imprimir
      super imprimir.
      Transcript show: 'Calificación:', calificación, '.\n'.

**Creación de objetos**

persona1 := Persona nueva inicializar: 'Juan' 20.
persona2 := Estudiante nuevo inicializar: 'María' 22 8.

**Envío de mensajes**

persona1 imprimir.
persona2 imprimir.

**Salida:**

Nombre: Juan Edad: 20.
Nombre: María Edad: 22 Calificación: 8.
```

**Explicación:**

* La clase `Persona` define dos variables de instancia, `nombre` y `edad`, y dos métodos, `inicializar` e `imprimir`. El método `inicializar` se utiliza para crear nuevos objetos `Persona` y el método `imprimir` se utiliza para imprimir la información de un objeto `Persona`.
* La clase `Estudiante` hereda de la clase `Persona`. Esto significa que los objetos `Estudiante` tienen todas las variables de instancia y métodos de los objetos `Persona`, además de sus propias variables de instancia y métodos. En este caso, la clase `Estudiante` define una variable de instancia, `calificación`, y un método, `imprimir`. El método `imprimir` se utiliza para imprimir la información de un objeto `Estudiante`.
* Los objetos `persona1` y `persona2` se crean utilizando el mensaje `nueva` seguido del nombre de la clase. El mensaje `inicializar` se envía a cada objeto para inicializar sus variables de instancia.
* El mensaje `imprimir` se envía a cada objeto para imprimir su información.

**Nota:** El código anterior utiliza la sintaxis de Smalltalk.