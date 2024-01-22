```smalltalk
(Objetivo: Crear una clase Punto2D que representa un punto en un plano de dos dimensiones y proporciona métodos para manipular y obtener información sobre el punto.)

Clase Punto2D

atributos
    x: número
    y: número

inicializarConX: unNúmero
         y: otroNúmero

    "Inicializa el punto con las coordenadas dadas."

    x := unNúmero.
    y := otroNúmero.

trasladarEnX: unNúmero
           y: otroNúmero

    "Traslada el punto unNúmero unidades en el eje x y otroNúmero unidades en el eje y."

    x := x + unNúmero.
    y := y + otroNúmero.

distanciaA: otroPunto

    "Devuelve la distancia entre este punto y otroPunto."

    ((x - otroPunto x) ^ 2 + (y - otroPunto y) ^ 2) raizCuadrada.

reflejarEn: unEje

    "Refleja el punto en el eje dado."

    (unEje = :x)
        ifTrue: [x := -x]
        ifFalse: [y := -y].

toString

    "Devuelve una representación de cadena del punto."

    "(" , x , ", ", y , ")"

[Punto2D crear] trasladarEnX: 10 y: 20.
[Punto2D crear] trasladarEnX: -5 y: 15.
[Punto2D crear] trasladarEnX: 0 y: -10.

distancia := [(10, 20) distanciaA: (-5, 15)] imprimir.

[(-5, 15) reflejarEn: :x] trasladarEnX: 10 y: 5.
[(-5, 15) reflejarEn: :y] trasladarEnX: 15 y: 10.
```

Explicación:

* La clase `Punto2D` representa un punto en un plano de dos dimensiones. Tiene dos atributos, `x` e `y`, que representan las coordenadas del punto en los ejes x e y, respectivamente.
* El método `inicializarConX:y:` es el constructor de la clase. Inicializa el punto con las coordenadas dadas.
* El método `trasladarEnX:y:` traslada el punto unNúmero unidades en el eje x y otroNúmero unidades en el eje y.
* El método `distanciaA:` devuelve la distancia entre este punto y otroPunto.
* El método `reflejarEn:` refleja el punto en el eje dado.
* El método `toString` devuelve una representación de cadena del punto.

Ejemplo de uso:

* Se crean tres objetos de la clase `Punto2D` y se les asignan coordenadas.
* Se calcula la distancia entre dos de los puntos y se imprime el resultado.
* Se reflejan dos de los puntos en los ejes x e y, respectivamente, y se les asignan nuevas coordenadas.