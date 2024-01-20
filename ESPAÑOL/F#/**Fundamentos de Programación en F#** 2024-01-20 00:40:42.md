```f#
// Definición de tipos y estructuras de datos

type Person = { Name: string; Age: int }

// Funciones básicas

let saludar persona =
    printfn "Hola, %s! Tienes %d años." persona.Name persona.Age


// Funciones de orden superior (HOF)

let map f xs =
    List.map f xs

let filter f xs =
    List.filter f xs

let fold f xs acc =
    List.fold f xs acc

// Funciones recursivas

let factorial n =
    if n <= 1 then 1 else n * factorial (n - 1)


// Módulos y espacios de nombres

module Math =
    let pi = 3.14
    let sumar a b = a + b

// Uso de módulos y espacios de nombres

let _ = printf "El valor de pi es %f" Math.pi
let _ = printf "La suma de 1 y 2 es %d" (Math.sumar 1 2)


// Excepciones

try
    raise (DivideByZeroException())
with
| DivideByZeroException() -> printfn "No se puede dividir por cero"


// Patrones y coincidencia de patrones

let clasificar persona =
    match persona with
    | { Name = "Juan"; Age = 20 } -> "Juan es un joven"
    | { Name = "María"; Age = 30 } -> "María es una mujer adulta"
    | { Name = _; Age = _ } -> "Se desconoce la clasificación de la persona"

// Uso de patrones y coincidencia de patrones

let _ = printf "%s" (clasificar { Name = "Juan"; Age = 20 })


// Tipos genéricos

type List<T> =
    | Nil
    | Cons(head: T; tail: List<T>)

let cabeza lst =
    match lst with
    | Cons(head, _) -> head
    | _ -> failwith "La lista está vacía"

let cola lst =
    match lst with
    | Cons(_, tail) -> tail
    | _ -> failwith "La lista está vacía"

let añadir elem lst =
    Cons(elem, lst)


// Uso de tipos genéricos

let listaNumeros = añadir 1 (añadir 2 (añadir 3 Nil))
let _ = printf "%d" (cabeza listaNumeros)
let _ = printf "%d" (cabeza (cola listaNumeros))


// Funciones anónimas (lambda)

let cuadrado x = fun x -> x * x

let _ = printf "%d" (cuadrado 5)


// Cierres

let contador =
    let cont = ref 0
    fun () ->
        cont := !cont + 1
        !cont

let _ = printf "%d" (contador())
let _ = printf "%d" (contador())
```

Explicación del código:

* El código define tipos y estructuras de datos personalizados, como el tipo `Person` que representa una persona con un nombre y una edad.
* También se definen funciones básicas, como la función `saludar` que saluda a una persona.
* Se utilizan funciones de orden superior (HOF) como `map`, `filter` y `fold` para trabajar con colecciones de datos.
* Se definen funciones recursivas, como la función `factorial` que calcula el factorial de un número.
* Se utilizan módulos y espacios de nombres para organizar el código y evitar conflictos de nombres.
* Se utiliza el manejo de excepciones para controlar errores.
* Se utilizan patrones y coincidencia de patrones para clasificar objetos.
* Se definen tipos genéricos, como el tipo `List<T>` que representa una lista de elementos de un tipo genérico `T`.
* Se utilizan funciones anónimas (lambda) y cierres para crear funciones ad hoc y encapsular estado.