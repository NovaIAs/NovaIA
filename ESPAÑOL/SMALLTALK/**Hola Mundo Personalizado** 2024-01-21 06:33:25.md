```smalltalk
"Mensaje de bienvenida"
Transcript show: '¡Hola, mundo!'.

"Definición de variables"
| saludo nombre |

"Asignación de valores a variables"
nombre := 'Pedro'.
saludo := '¡Hola, '.

"Imprime el saludo y el nombre"
Transcript show: saludo , nombre , !'.
```

Este código SMALLTALK realiza lo siguiente:

1. Muestra el mensaje de bienvenida "¡Hola, mundo!" en la consola usando el objeto `Transcript`.

2. Define dos variables: `saludo` y `nombre`.

3. Asigna el valor "Pedro" a la variable `nombre` y el valor "¡Hola, " a la variable `saludo`.

4. Imprime el saludo, el nombre y un signo de exclamación en la consola, concatenando las cadenas de texto usando el operador `, ` y el punto de exclamación `!`.

Este código muestra el saludo personalizado "¡Hola, Pedro!" en la consola.