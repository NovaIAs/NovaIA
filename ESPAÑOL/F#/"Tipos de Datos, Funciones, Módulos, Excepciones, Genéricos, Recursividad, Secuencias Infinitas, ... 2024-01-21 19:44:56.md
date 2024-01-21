```f#
// Tipos de datos personalizados
type Persona = { Nombre: string; Edad: int }
type Dirección = { Calle: string; Ciudad: string; País: string }

// Funciones
let saludar persona = printf "Hola, %s!\n" persona.Nombre
let obtenerDirección persona = persona.Dirección

// Patrones de coincidencia
let edadPersona persona =
    match persona with
    | { Edad = e } -> e

// Funciones reutilizables
let aplicar función lista =
    List.iter función lista

// Módulos
module Matemáticas =
    let sumar a b = a + b

// Excepciones
let dividir a b =
    if b = 0 then raise DivideByZeroException() else a / b

// Manejo de excepciones
try
    let resultado = dividir 10 0
    printf "El resultado es %d\n" resultado
with
| DivideByZeroException -> printf "No se puede dividir por cero\n"

// Genéricos
type Lista<'T> =
    | Vacía
    | Cons de 'T * Lista<'T>

// Funciones genéricas
let longitud lista =
    match lista with
    | Vacía -> 0
    | Cons(_, cola) -> 1 + longitud cola

// Tipos recursivos
type Árbol =
    | Hoja de int
    | Rama de Árbol * Árbol

// Funciones recursivas
let altura árbol =
    match árbol with
    | Hoja(_) -> 1
    | Rama(izq, der) -> 1 + max (altura izq) (altura der)

// Secuencias infinitas
let serieFibonacci =
    let rec fib n =
        if n < 2 then n else fib (n - 1) + fib (n - 2)
    seq { fib i | i <- 0 .. Int32.MaxValue }

// Tipos dinámicos
dynamic x = 10
printf "%d\n" (x + 5)

// Interfaz con código nativo
open System
open System.Drawing

let dibujarCirculo centro radio =
    let gfx = Graphics.FromImage(Bitmap(100, 100))
    gfx.DrawEllipse(Pens.Black, centro.X - radio, centro.Y - radio, 2 * radio, 2 * radio)

// Interfaz con otros lenguajes
let sumarCSharp a b =
    let csharp = [|Microsoft.CSharp.RuntimeBinder.Binder.InvokeMember(null, "Call", null, typeof<Func<int, int, int>>, null, new object[] { a, b })|]
    csharp.[0]

// Programa principal
let personas = [Persona("Juan", 20), Persona("María", 25)]
aplicar saludar personas
```

Explicación del código:

* Se definen tipos de datos personalizados para representar personas y direcciones.
* Se definen funciones para saludar a una persona y obtener su dirección.
* Se utiliza un patrón de coincidencia para obtener la edad de una persona.
* Se define una función reutilizable para aplicar una función a una lista.
* Se crea un módulo para encapsular la funcionalidad matemática.
* Se define una función para dividir dos números, con manejo de excepciones.
* Se define un tipo de datos genérico para representar listas.
* Se define una función genérica para obtener la longitud de una lista.
* Se define un tipo de datos recursivo para representar árboles binarios.
* Se define una función recursiva para obtener la altura de un árbol binario.
* Se define una secuencia infinita para generar la serie Fibonacci.
* Se utiliza la funcionalidad dinámica para interactuar con objetos .NET.
* Se define una función para dibujar un círculo en una imagen.
* Se utiliza la interoperabilidad con otros lenguajes para llamar a una función de C#.
* Se crea una lista de personas y se imprime un saludo para cada una.