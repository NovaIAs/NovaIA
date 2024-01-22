```f#
type Contenedor =
    | Nada
    | UnElemento de object
    | MuchosElementos de object list

let crearContenedorElemento valor =
    UnElemento valor

let crearContenedorElementos valores =
    MuchosElementos valores

let obtenerElemento contenedor =
    match contenedor with
    | UnElemento valor -> valor
    | _ -> failwith "El contenedor no contiene un solo elemento"

let obtenerElementos contenedor =
    match contenedor with
    | MuchosElementos valores -> valores
    | _ -> failwith "El contenedor no contiene varios elementos"

let agregarElemento contenedor elemento =
    match contenedor with
    | Nada -> UnElemento elemento
    | UnElemento valor -> MuchosElementos [valor; elemento]
    | MuchosElementos valores -> MuchosElementos (elemento :: valores)

let eliminarElemento contenedor elemento =
    match contenedor with
    | Nada -> Nada
    | UnElemento valor when valor = elemento -> Nada
    | UnElemento valor -> UnElemento valor
    | MuchosElementos valores -> MuchosElementos (List.filter (fun x -> x <> elemento) valores)

let contarElementos contenedor =
    match contenedor with
    | Nada -> 0
    | UnElemento _ -> 1
    | MuchosElementos valores -> List.length valores

let iterarContenedor (f : object -> unit) contenedor =
    match contenedor with
    | Nada -> ()
    | UnElemento valor -> f valor
    | MuchosElementos valores -> List.iter f valores

let imprimirContenedor contenedor =
    match contenedor with
    | Nada -> printfn "El contenedor está vacío"
    | UnElemento valor -> printfn "El contenedor contiene el elemento %A" valor
    | MuchosElementos valores -> printfn "El contenedor contiene los elementos %A" valores

let main () =
    let contenedor = crearContenedorElemento 10
    imprimirContenedor contenedor
    contenedor |> agregarElemento 20 |> imprimirContenedor
    contenedor |> agregarElemento 30 |> imprimirContenedor
    contenedor |> eliminarElemento 20 |> imprimirContenedor
    printfn "El contenedor contiene %d elementos" (contarElementos contenedor)
    iterarContenedor (fun x -> printfn "Elemento: %A" x) contenedor
    ()

main ()

```

Explicación del código:

1. Definición de tipos:

   - `Contenedor`: Este es el tipo que representa el contenedor. Puede estar vacío (`Nada`), contener un solo elemento (`UnElemento`) o contener varios elementos (`MuchosElementos`).
   - `object`: Este es el tipo de los elementos que se pueden almacenar en el contenedor. En este caso, puede ser cualquier tipo de objeto.

2. Funciones para crear contenedores:

   - `crearContenedorElemento`: Esta función crea un contenedor que contiene un solo elemento.
   - `crearContenedorElementos`: Esta función crea un contenedor que contiene varios elementos.

3. Funciones para obtener elementos del contenedor:

   - `obtenerElemento`: Esta función obtiene el elemento del contenedor si contiene un solo elemento.
   - `obtenerElementos`: Esta función obtiene todos los elementos del contenedor si contiene varios elementos.

4. Funciones para agregar y eliminar elementos del contenedor:

   - `agregarElemento`: Esta función agrega un elemento al contenedor. Si el contenedor está vacío, se crea un nuevo contenedor con el elemento. Si el contenedor contiene un solo elemento, se crea un nuevo contenedor con el elemento existente y el nuevo elemento. Si el contenedor contiene varios elementos, se agrega el nuevo elemento a la lista de elementos existentes.
   - `eliminarElemento`: Esta función elimina un elemento del contenedor. Si el contenedor está vacío, no hace nada. Si el contenedor contiene un solo elemento y el elemento coincide con el elemento a eliminar, se crea un nuevo contenedor vacío. Si el contenedor contiene varios elementos, se elimina el elemento de la lista de elementos existentes.

5. Funciones para contar los elementos del contenedor:

   - `contarElementos`: Esta función cuenta el número de elementos del contenedor.

6. Funciones para iterar sobre los elementos del contenedor:

   - `iterarContenedor`: Esta función itera sobre los elementos del contenedor y aplica una función a cada elemento.

7. Función `imprimirContenedor`:

   - Esta función imprime el contenido del contenedor. Si el contenedor está vacío, imprime un mensaje que indica que está vacío. Si el contenedor contiene un solo elemento, imprime el elemento. Si el contenedor contiene varios elementos, imprime todos los elementos.

8. Función `main`:

   - Esta es la función principal del programa. Crea un contenedor con un solo elemento, imprime el contenedor, agrega dos elementos más al contenedor, imprime el contenedor, elimina un elemento del contenedor, imprime el contenedor, cuenta el número de elementos del contenedor, itera sobre los elementos del contenedor e imprime cada elemento.