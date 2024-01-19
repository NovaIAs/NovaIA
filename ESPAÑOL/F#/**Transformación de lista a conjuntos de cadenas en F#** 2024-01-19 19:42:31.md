```f#

// Definición de alias para simplificar el código
type StringMap<T> = Map<string, T>

// Implementación del tipo `StringSet` usando `StringMap`
type StringSet = StringMap<unit>

// Interfaz abstracta para operaciones básicas de conjuntos
interface ISet<T> =
    abstract member Add: T -> unit
    abstract member Remove: T -> unit
    abstract member Contains: T -> bool

// Implementación del tipo `StringSet` utilizando la interfaz `ISet`
type StringSetImpl =
    inherit StringSet
    inherit ISet<string>

    // Añade un elemento al conjunto
    override member Add(item: string) = Map.add item () this
    
    // Elimina un elemento del conjunto
    override member Remove(item: string) = Map.remove item this

    // Comprueba si un elemento está en el conjunto
    override member Contains(item: string) = Map.containsKey item this

// Función para crear un conjunto de cadenas vacío
let createSet = fun () -> StringSetImpl()

// Función para añadir un elemento a un conjunto de cadenas
let addSet = fun (set: StringSetImpl) (item: string) -> set.Add(item)

// Función para eliminar un elemento de un conjunto de cadenas
let removeSet = fun (set: StringSetImpl) (item: string) -> set.Remove(item)

// Función que transforma una lista de cadenas en una lista de conjuntos de cadenas
let transformToSets (list: string list) =
    let result = list.Map(createSet)
    let result = result.Map(fun set -> addSet set "A")
    result.Map(fun set -> addSet set "B")

// Uso de las funciones definidas anteriormente
let items = ["Uno"; "Dos"; "Tres"; "Cuatro"]
let transformedItems = transformToSets items
let containsThree = transformedItems.[1].Contains("Tres")

// Mostrar el resultado
printfn "Resultado: %b" containsThree

```