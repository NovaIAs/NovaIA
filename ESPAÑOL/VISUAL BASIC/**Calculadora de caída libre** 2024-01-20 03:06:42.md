```visual basic
' Definir constantes
Const PI = 3.141592653589793
Const GRAVEDAD = 9.80665

' Definir variables
Dim masa As Double
Dim altura As Double
Dim velocidadInicial As Double
Dim tiempoDeCaída As Double

' Obtener valores del usuario
masa = InputBox("Introduzca la masa del objeto (en kilogramos):")
altura = InputBox("Introduzca la altura inicial del objeto (en metros):")
velocidadInicial = InputBox("Introduzca la velocidad inicial del objeto (en metros por segundo):")

' Calcular el tiempo de caída
tiempoDeCaída = Sqrt((2 * altura) / GRAVEDAD)

' Calcular la velocidad terminal
velocidadTerminal = Sqrt(2 * GRAVEDAD * altura)

' Mostrar los resultados
MsgBox "El tiempo de caída del objeto es:" & tiempoDeCaída & " segundos."
MsgBox "La velocidad terminal del objeto es:" & velocidadTerminal & " metros por segundo."

' Simular el movimiento del objeto
For t = 0 To tiempoDeCaída
    y = altura - 0.5 * GRAVEDAD * t^2
    v = velocidadInicial - GRAVEDAD * t
    Print "Tiempo:" & t & " segundos"
    Print "Altura:" & y & " metros"
    Print "Velocidad:" & v & " metros por segundo"
    Print ""
Next t
```

Este código calcula el tiempo de caída y la velocidad terminal de un objeto que se deja caer desde una altura determinada con una velocidad inicial determinada. El código también simula el movimiento del objeto a medida que cae.

El código comienza definiendo las constantes PI y GRAVEDAD. A continuación, define las variables masa, altura, velocidadInicial y tiempoDeCaída.

El código luego obtiene los valores del usuario para la masa, la altura y la velocidad inicial. A continuación, calcula el tiempo de caída utilizando la ecuación:

```
tiempoDeCaída = Sqrt((2 * altura) / GRAVEDAD)
```

El código luego calcula la velocidad terminal utilizando la ecuación:

```
velocidadTerminal = Sqrt(2 * GRAVEDAD * altura)
```

El código entonces muestra los resultados al usuario.

El código finalmente simula el movimiento del objeto a medida que cae. Para ello, utiliza un bucle for para recorrer cada segundo de tiempo de caída. Para cada segundo, calcula la altura y la velocidad del objeto utilizando las ecuaciones:

```
y = altura - 0.5 * GRAVEDAD * t^2
v = velocidadInicial - GRAVEDAD * t
```

El código luego muestra la altura y la velocidad del objeto para ese segundo.