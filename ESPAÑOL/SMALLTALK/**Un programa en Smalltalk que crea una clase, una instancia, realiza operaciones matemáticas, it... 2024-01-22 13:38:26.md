```smalltalk
¡Hola mundo!

"Este es un comentario en Smalltalk.
Todo lo que está entre comillas dobles se ignora al ejecutar el código."

"Definimos una clase llamada 'Persona' que tiene dos variables de instancia, 'nombre' y 'edad'."
Persona clase
    nombre: String.
    edad: Número.
fin

"Creamos una instancia de la clase 'Persona' llamada 'juan' y establecemos el nombre y la edad."
juan := Persona nueva.
juan nombre: 'Juan'.
juan edad: 25.

"Mostramos el nombre y la edad de 'juan'."
Display writeString: 'Nombre: ', juan nombre.
Display writeString: 'Edad: ', (juan edad comoString).

"Definimos una función llamada 'suma' que toma dos números y devuelve su suma."
suma: (a: Número, b: Número) -> a + b.

"Calculamos la suma de 5 y 10 y la almacenamos en 'resultado'."
resultado := suma: 5, 10.

"Mostramos el resultado."
Display writeString: 'Resultado: ', resultado comoString.

"Definimos una lista de números."
números := (1 2 3 4 5).

"Iteramos sobre la lista de números y mostramos cada uno de ellos."
números do: [:número | Display writeString: número comoString, ' '].

"Definimos un bloque de código que se ejecutará cuando se haga clic en el botón 'Aceptar'."
Aceptar-evento-botón: [:event | Display writeString: 'Aceptar pulsado'].

"Creamos un botón con el texto 'Aceptar' y lo añadimos a la ventana principal."
botón-aceptar := Botón conTítulo: 'Aceptar'.
botón-aceptar añadidos A: Ventana principal.

"Establecemos el evento de clic del botón 'Aceptar' al bloque definido anteriormente."
botón-aceptar eventoClic: Aceptar-evento-botón.
```

Este código crea una clase llamada 'Persona' con dos variables de instancia, 'nombre' y 'edad'. Luego crea una instancia de la clase 'Persona' llamada 'juan' y establece el nombre y la edad. Después muestra el nombre y la edad de 'juan'.

También define una función llamada 'suma' que toma dos números y devuelve su suma. Calcula la suma de 5 y 10 y la almacena en 'resultado'. Luego muestra el resultado.

Define una lista de números e itera sobre ella mostrando cada uno de los números.

Finalmente, define un bloque de código que se ejecutará cuando se haga clic en el botón 'Aceptar'. Crea un botón con el texto 'Aceptar' y lo añade a la ventana principal. Establece el evento de clic del botón 'Aceptar' al bloque definido anteriormente.