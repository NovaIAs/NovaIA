```smalltalk
Hola Mundo

"Este es un sencillo programa Smalltalk que imprime 'Hola Mundo' en la consola."

Transcript show: 'Hola Mundo'.
```

Explicación:

* Hola Mundo: Es una locución latina que es equivalente a la expresión inglesa Hello World. Se utiliza como una forma sencilla de probar un nuevo entorno de programación.
* Transcript show: Es un método que se utiliza para imprimir texto en la consola.
* 'Hola Mundo': Es la cadena de texto que se imprime en la consola.

Ahora vamos a crear un programa más complejo que permita al usuario introducir su nombre y luego lo salude por su nombre.

```smalltalk
Saludar

"Este programa permite al usuario introducir su nombre y luego lo saluda por su nombre."

| nombre |

"Mostramos un mensaje pidiendo al usuario que introduzca su nombre"
Transcript show: 'Introduce tu nombre: '.

"Leemos el nombre del usuario desde la consola"
nombre := Transcript readLine.

"Mostramos un mensaje saludando al usuario por su nombre"
Transcript show: 'Hola, ' , nombre , '!' .
```

Explicación:

* Saludar: Es el nombre de nuestro programa.
* | nombre |: Es una variable que se utiliza para almacenar el nombre del usuario.
* Transcript show: 'Introduce tu nombre: ': Es un mensaje que se muestra en la consola pidiendo al usuario que introduzca su nombre.
* Transcript readLine: Es un método que se utiliza para leer una línea de texto de la consola.
* 'Hola, ' , nombre , '!' : Es una cadena de texto que se utiliza para saludar al usuario por su nombre.

Este programa es más complejo que el anterior porque utiliza variables y métodos para interactuar con el usuario.