```f#
// Definición de una interfaz para representar una forma geométrica
interfaz IForma {
    propiedad Area : float
    propiedad Perímetro : float
}

// Clase que implementa la interfaz IForma para representar un círculo
class Círculo implements IForma {
    propiedad Radio : float

    constructor(radio: float) {
        Radio = radio
    }

    propiedad Area : float {
        get {
            return Math.PI * Radio * Radio
        }
    }

    propiedad Perímetro : float {
        get {
            return 2 * Math.PI * Radio
        }
    }
}

// Clase que implementa la interfaz IForma para representar un rectángulo
class Rectángulo implements IForma {
    propiedad Ancho : float
    propiedad Alto : float

    constructor(ancho: float, alto: float) {
        Ancho = ancho
        Alto = alto
    }

    propiedad Area : float {
        get {
            return Ancho * Alto
        }
    }

    propiedad Perímetro : float {
        get {
            return 2 * (Ancho + Alto)
        }
    }
}

// Función para calcular el área total de una lista de formas geométricas
fun calcularAreaTotal(formas: IForma[]): float =
    formas.Sum(forma => forma.Area)

// Función para calcular el perímetro total de una lista de formas geométricas
fun calcularPerímetroTotal(formas: IForma[]): float =
    formas.Sum(forma => forma.Perímetro)

// Lista de formas geométricas
var formas: IForma[] = [
    new Círculo(5.0),
    new Rectángulo(10.0, 20.0),
    new Círculo(3.0),
    new Rectángulo(15.0, 25.0)
]

// Calcular el área total y el perímetro total de la lista de formas geométricas
var areaTotal: float = calcularAreaTotal(formas)
var perímetroTotal: float = calcularPerímetroTotal(formas)

// Mostrar los resultados
Console.WriteLine("El área total es {0}", areaTotal)
Console.WriteLine("El perímetro total es {0}", perímetroTotal)
```

Explicación del código:

* La interfaz `IForma` define las propiedades `Area` y `Perímetro` que deben ser implementadas por las clases que representen formas geométricas.
* La clase `Círculo` implementa la interfaz `IForma` y representa un círculo. La propiedad `Radio` almacena el radio del círculo. Las propiedades `Area` y `Perímetro` calculan el área y el perímetro del círculo, respectivamente.
* La clase `Rectángulo` implementa la interfaz `IForma` y representa un rectángulo. Las propiedades `Ancho` y `Alto` almacenan el ancho y el alto del rectángulo. Las propiedades `Area` y `Perímetro` calculan el área y el perímetro del rectángulo, respectivamente.
* La función `calcularAreaTotal` calcula el área total de una lista de formas geométricas. La función utiliza la función `Sum` para sumar las áreas de todas las formas en la lista.
* La función `calcularPerímetroTotal` calcula el perímetro total de una lista de formas geométricas. La función utiliza la función `Sum` para sumar los perímetros de todas las formas en la lista.
* La lista `formas` contiene una colección de objetos `Círculo` y `Rectángulo`.
* Las variables `areaTotal` y `perímetroTotal` almacenan el área total y el perímetro total de la lista de formas geométricas, respectivamente.
* Se utilizan las funciones `Console.WriteLine` para mostrar los resultados en la consola.