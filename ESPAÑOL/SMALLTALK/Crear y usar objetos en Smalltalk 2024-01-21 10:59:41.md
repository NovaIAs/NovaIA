```smalltalk

**Definir una clase**

```smalltalk
Object subclass: Persona [ | nombre edad | ]
```

* `Object` es la clase predefinida que todos los objetos heredan.
* `subclass:` crea una nueva subclase de una clase dada.
* `Persona` es el nombre de la nueva clase.
* `| nombre edad |` son las variables de instancia de la clase.

**Agregar métodos a una clase**

```smalltalk
Persona

    inicializar :unNombre :unaEdad [
        nombre := unNombre.
        edad := unaEdad.
    ]

    nombre [
        ^nombre
    ]

    edad [
        ^edad
    ]
```

* `inicializar:` es el método del constructor. Se llama cuando se crea un nuevo objeto.
* `nombre` y `edad` son métodos de acceso. Se utilizan para obtener y establecer los valores de las variables de instancia.

**Crear un objeto**

```smalltalk
persona1 := Persona new inicializar: 'Juan' 25.
```

* `Persona new` crea un nuevo objeto de la clase `Persona`.
* `inicializar: 'Juan' 25` llama al método del constructor con los argumentos dados.

**Enviar mensajes a un objeto**

```smalltalk
persona1 nombre.
```

* `persona1` es el objeto al que se envía el mensaje.
* `nombre` es el mensaje que se envía al objeto.
* El objeto responde al mensaje enviando de vuelta el valor de la variable de instancia `nombre`.

**#### Ejemplo completo####**

```smalltalk
Object subclass: Persona [ | nombre edad | ]

Persona

    inicializar :unNombre :unaEdad [
        nombre := unNombre.
        edad := unaEdad.
    ]

    nombre [
        ^nombre
    ]

    edad [
        ^edad
    ]

persona1 := Persona new inicializar: 'Juan' 25.
persona2 := Persona new inicializar: 'María' 30.

persona1 nombre.
"Juan"

persona2 edad.
"30"
```

Este ejemplo crea dos objetos de la clase `Persona` y luego envía mensajes a los objetos para obtener sus nombres y edades.

**#### Explicación del código####**

El código anterior es un ejemplo de cómo crear y usar objetos en Smalltalk.

* Primero, se define una clase llamada `Persona`. Esta clase tiene dos variables de instancia, `nombre` y `edad`.
* A continuación, se agregan tres métodos a la clase `Persona`: `inicializar`, `nombre` y `edad`. El método `inicializar` se utiliza para crear un nuevo objeto de la clase `Persona`. Los métodos `nombre` y `edad` se utilizan para obtener y establecer los valores de las variables de instancia `nombre` y `edad`, respectivamente.
* A continuación, se crean dos objetos de la clase `Persona`. El primer objeto se llama `persona1` y tiene el nombre "Juan" y la edad 25. El segundo objeto se llama `persona2` y tiene el nombre "María" y la edad 30.
* Por último, se envían mensajes a los objetos `persona1` y `persona2` para obtener sus nombres y edades. Los mensajes `nombre` y `edad` se envían a los objetos utilizando el operador de punto (`.`). Los objetos responden a los mensajes enviando de vuelta los valores de las variables de instancia `nombre` y `edad`, respectivamente.

El código anterior es un ejemplo sencillo de cómo crear y usar objetos en Smalltalk. Smalltalk es un lenguaje de programación orientado a objetos, lo que significa que se basa en la idea de que los programas están compuestos por objetos que interactúan entre sí.