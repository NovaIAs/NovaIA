```visual basic
Option Explicit

'Constantes
Const PI = 3.1415926535897932384626433832795
Const GRAVEDAD = 9.80665 m/s^2

'Variables
Dim masa, radio, velocidad, altura, tiempo, angulo

'Obtener los valores del usuario
masa = InputBox("Introduce la masa del objeto (kg): ")
radio = InputBox("Introduce el radio del objeto (m): ")
velocidad = InputBox("Introduce la velocidad inicial del objeto (m/s): ")
angulo = InputBox("Introduce el ángulo de lanzamiento (grados): ")

'Convertir el ángulo de grados a radianes
angulo = angulo * PI / 180

'Calcular la altura máxima alcanzada por el objeto
altura = (velocidad^2 * Sin(angulo)^2) / (2 * GRAVEDAD)

'Calcular el tiempo que tarda el objeto en alcanzar la altura máxima
tiempo = velocidad * Sin(angulo) / GRAVEDAD

'Calcular la distancia horizontal recorrida por el objeto
distancia = velocidad * Cos(angulo) * tiempo

'Mostrar los resultados
MsgBox "La altura máxima alcanzada por el objeto es: " & altura & " metros."
MsgBox "El tiempo que tarda el objeto en alcanzar la altura máxima es: " & tiempo & " segundos."
MsgBox "La distancia horizontal recorrida por el objeto es: " & distancia & " metros."
```

Este código calcula la altura máxima alcanzada por un objeto lanzado con una velocidad inicial y un ángulo determinados. También calcula el tiempo que tarda el objeto en alcanzar la altura máxima y la distancia horizontal recorrida por el objeto.

El código utiliza las siguientes funciones:

* `Sin()` calcula el seno de un ángulo.
* `Cos()` calcula el coseno de un ángulo.
* `InputBox()` muestra un cuadro de diálogo que solicita al usuario que introduzca un valor.
* `MsgBox()` muestra un cuadro de diálogo que muestra un mensaje.