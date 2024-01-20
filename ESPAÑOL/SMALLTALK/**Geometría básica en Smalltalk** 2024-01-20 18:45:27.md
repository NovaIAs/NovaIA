```smalltalk

"Clase Punto"

Objeto
    nombre: 'Punto'
    categoría: 'Geometría'
    instanciaVariable: {
        x: 0.0.
        y: 0.0.
    }
    método: {
        "_nueva"
            "Crea un nuevo punto con las coordenadas especificadas."
            [ :x :y |
                self new setX: x setY: y
            ]

        "getX"
            "Devuelve la coordenada x del punto."
            [ self x ]

        "setX:"
            "Establece la coordenada x del punto."
            [ :x | self x: x ]

        "getY"
            "Devuelve la coordenada y del punto."
            [ self y ]

        "setY:"
            "Establece la coordenada y del punto."
            [ :y | self y: y ]

        "toString"
            "Devuelve una representación en cadena del punto."
            [ 'Punto(' , self x printString , ', ', self y printString , ')' ]

        "trasladar:"
            "Traslada el punto por el vector especificado."
            [ :v |
                self setX: (self x + v x) setY: (self y + v y)
            ]

        "distanciaA:"
            "Calcula la distancia euclidiana entre este punto y otro punto."
            [ :p |
                (self x - p x) squared + (self y - p y) squared
            ] ^ Math sqrt
    }
end

"Clase Línea"

Objeto
    nombre: 'Línea'
    categoría: 'Geometría'
    instanciaVariable: {
        p1: Punto new
        p2: Punto new
    }
    método: {
        "_nueva"
            "Crea una nueva línea con los puntos especificados."
            [ :p1 :p2 |
                self new setP1: p1 setP2: p2
            ]

        "getP1"
            "Devuelve el primer punto de la línea."
            [ self p1 ]

        "setP1:"
            "Establece el primer punto de la línea."
            [ :p | self p1: p ]

        "getP2"
            "Devuelve el segundo punto de la línea."
            [ self p2 ]

        "setP2:"
            "Establece el segundo punto de la línea."
            [ :p | self p2: p ]

        "toString"
            "Devuelve una representación en cadena de la línea."
            [ 'Línea(', self p1 toString , ', ', self p2 toString , ')' ]

        "longitud"
            "Calcula la longitud de la línea."
            [ self p1 distanciaA: self p2 ]
    }
end

"Clase Triángulo"

Objeto
    nombre: 'Triángulo'
    categoría: 'Geometría'
    instanciaVariable: {
        p1: Punto new
        p2: Punto new
        p3: Punto new
    }
    método: {
        "_nueva"
            "Crea un nuevo triángulo con los puntos especificados."
            [ :p1 :p2 :p3 |
                self new setP1: p1 setP2: p2 setP3: p3
            ]

        "getP1"
            "Devuelve el primer punto del triángulo."
            [ self p1 ]

        "setP1:"
            "Establece el primer punto del triángulo."
            [ :p | self p1: p ]

        "getP2"
            "Devuelve el segundo punto del triángulo."
            [ self p2 ]

        "setP2:"
            "Establece el segundo punto del triángulo."
            [ :p | self p2: p ]

        "getP3"
            "Devuelve el tercer punto del triángulo."
            [ self p3 ]

        "setP3:"
            "Establece el tercer punto del triángulo."
            [ :p | self p3: p ]

        "toString"
            "Devuelve una representación en cadena del triángulo."
            [ 'Triángulo(', self p1 toString , ', ', self p2 toString , ', ', self p3 toString , ')' ]

        "perímetro"
            "Calcula el perímetro del triángulo."
            [ self p1 distanciaA: self p2 + self p2 distanciaA: self p3 + self p3 distanciaA: self p1 ]

        "área"
            "Calcula el área del triángulo."
            [
                (self p1 x - self p3 x) * (self p2 y - self p3 y) -
                (self p1 y - self p3 y) * (self p2 x - self p3 x)
            ] abs / 2
    }
end

"Clase Círculo"

Objeto
    nombre: 'Círculo'
    categoría: 'Geometría'
    instanciaVariable: {
        centro: Punto new
        radio: 0.0
    }
    método: {
        "_nueva"
            "Crea un nuevo círculo con el centro y el radio especificados."
            [ :centro :radio |
                self new setCentro: centro setRadio: radio
            ]

        "getCentro"
            "Devuelve el centro del círculo."
            [ self centro ]

        "setCentro:"
            "Establece el centro del círculo."
            [ :centro | self centro: centro ]

        "getRadio"
            "Devuelve el radio del círculo."
            [ self radio ]

        "setRadio:"
            "Establece el radio del círculo."
            [ :radio | self radio: radio ]

        "toString"
            "Devuelve una representación en cadena del círculo."
            [ 'Círculo(', self centro toString , ', ', self radio printString , ')' ]

        "área"
            "Calcula el área del círculo."
            [ Math PI * self radio squared ]

        "circunferencia"
            "Calcula la circunferencia del círculo."
            [ 2.0 * Math PI * self radio ]
    }
end

```

Explicación del código:

* La clase `Punto` define un punto en el espacio bidimensional. Tiene dos instancias variables, `x` e `y`, que representan las coordenadas del punto. También tiene métodos para establecer y obtener estas coordenadas, así como para calcular la distancia a otro punto.
* La clase `Línea` define una línea en el espacio bidimensional. Tiene dos instancias variables, `p1` y `p2`, que representan los dos puntos que definen la línea. También tiene métodos para establecer y obtener estos puntos, así como para calcular la longitud de la línea.
* La clase `Triángulo` define un triángulo en el espacio bidimensional. Tiene tres instancias variables, `p1`, `p2` y `p3`, que representan los tres puntos que definen el triángulo. También tiene métodos para establecer y obtener estos puntos, así como para calcular el perímetro y el área del triángulo.
* La clase `Círculo` define un círculo en el espacio bidimensional. Tiene dos instancias variables, `centro` y `radio`, que representan el centro y el radio del círculo. También tiene métodos para establecer y obtener estos valores, así como para calcular el área y la circunferencia del círculo.

El código utiliza el patrón de diseño de objetos en el que cada clase define sus propias instancias variables y métodos, y las clases derivadas heredan de las clases base. Esto permite compartir código entre clases relacionadas y facilita la creación de nuevas clases.