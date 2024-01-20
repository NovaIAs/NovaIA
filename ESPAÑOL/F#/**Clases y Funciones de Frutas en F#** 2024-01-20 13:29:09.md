```f#
// Definición de las clases y sus propiedades
class Fruta {
  public Nombre: string
  public Color: string
  public Sabor: string
  public Tamaño: float
  public Precio: float

  public constructor(nombre: string, color: string, sabor: string, tamaño: float, precio: float) {
    this.Nombre = nombre
    this.Color = color
    this.Sabor = sabor
    this.Tamaño = tamaño
    this.Precio = precio
  }
}

class Manzana : Fruta {
  public Variedad: string

  public constructor(nombre: string, color: string, sabor: string, tamaño: float, precio: float, variedad: string) {
    super(nombre, color, sabor, tamaño, precio)
    this.Variedad = variedad
  }
}

class Naranja : Fruta {
  public VitaminaC: int

  public constructor(nombre: string, color: string, sabor: string, tamaño: float, precio: float, vitaminaC: int) {
    super(nombre, color, sabor, tamaño, precio)
    this.VitaminaC = vitaminaC
  }
}

// Definición de las funciones de utilidad
function ObtenerFrutasPorColor(frutas: Fruta[], color: string): Fruta[] {
  let frutasFiltradas =
    frutas |> Array.filter(fun fruta -> fruta.Color = color)
  return frutasFiltradas
}

function ObtenerPrecioTotal(frutas: Fruta[]): float {
  let precioTotal = 0.0
  frutas |> Array.iter(fun fruta -> precioTotal += fruta.Precio)
  return precioTotal
}

// Creación de una lista de frutas
let frutas = [
  new Manzana("Manzana roja", "rojo", "dulce", 0.2, 1.0, "Red Delicious"),
  new Manzana("Manzana verde", "verde", "ácido", 0.25, 1.2, "Granny Smith"),
  new Naranja("Naranja", "naranja", "cítrico", 0.3, 1.5, 100),
  new Naranja("Limón", "amarillo", "ácido", 0.1, 0.8, 50),
  new Fruta("Plátano", "amarillo", "dulce", 0.2, 1.0),
  new Fruta("Pera", "verde", "dulce", 0.3, 1.2)
]

// Uso de las funciones de utilidad
let frutas Rojas = ObtenerFrutasPorColor(frutas, "rojo")
let precioTotal = ObtenerPrecioTotal(frutas)

// Impresión de resultados
printfn "Frutas rojas:"
frutas Rojas |> Array.iter(fun fruta -> printfn "  %s" fruta.Nombre)

printfn "Precio total de todas las frutas: %f" precioTotal
```

Este código define dos clases de frutas, `Manzana` y `Naranja`, que heredan de la clase base `Fruta`. Cada clase tiene sus propias propiedades específicas, además de las propiedades comunes que tiene la clase base.

El código también define dos funciones de utilidad: `ObtenerFrutasPorColor` y `ObtenerPrecioTotal`. La función `ObtenerFrutasPorColor` filtra una lista de frutas y devuelve las frutas que tienen un color específico. La función `ObtenerPrecioTotal` calcula el precio total de una lista de frutas.

El código crea una lista de frutas y luego usa las funciones de utilidad para filtrar las frutas rojas y calcular el precio total de todas las frutas. Finalmente, imprime los resultados.