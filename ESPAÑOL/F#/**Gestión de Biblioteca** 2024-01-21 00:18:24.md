```f#

// Definición de tipos de datos
type Libro = { ISBN: string; Autor: string; Titulo: string; Precio: float }
type Usuario = { Nombre: string; LibrosPrestados: Libro list }

// Definición de funciones
// Función para crear un nuevo libro
let nuevoLibro (isbn, autor, titulo, precio) = Libro(isbn, autor, titulo, precio)

// Función para crear un nuevo usuario
let nuevoUsuario (nombre) = Usuario(nombre, [])

// Función para prestar un libro a un usuario
let prestarLibro (libro, usuario) =
    let nuevosLibrosPrestados = usuario.LibrosPrestados @ [libro]
    { usuario with LibrosPrestados = nuevosLibrosPrestados }

// Función para devolver un libro de un usuario
let devolverLibro (libro, usuario) =
    let nuevosLibrosPrestados = usuario.LibrosPrestados |> List.filter (fun l -> l.ISBN <> libro.ISBN)
    { usuario with LibrosPrestados = nuevosLibrosPrestados }

// Crear algunos libros y usuarios
let libros = [
    nuevoLibro("1234567890", "Juan Pérez", "El Quijote", 10.0),
    nuevoLibro("9876543210", "Ana López", "Cien años de soledad", 15.0),
    nuevoLibro("0123456789", "Pedro García", "El señor de los anillos", 20.0)
]

let usuarios = [
    nuevoUsuario("María González"),
    nuevoUsuario("José Rodríguez"),
    nuevoUsuario("Luis Fernández")
]

// Prestar algunos libros a algunos usuarios
usuarios |> List.iter (fun u -> usuarios <- prestarLibro(libros[0], u))
usuarios |> List.iter (fun u -> usuarios <- prestarLibro(libros[1], u))

// Devolver algunos libros de algunos usuarios
usuarios |> List.iter (fun u -> usuarios <- devolverLibro(libros[0], u))
usuarios |> List.iter (fun u -> usuarios <- devolverLibro(libros[1], u))

// Mostrar los libros prestados de cada usuario
usuarios |> List.iter (fun u -> printf "%s: %A" u.Nombre (u.LibrosPrestados |> List.map (fun l -> l.Titulo)))

```

Este código define dos tipos de datos: `Libro` y `Usuario`. El tipo `Libro` contiene información sobre un libro, como su ISBN, autor, título y precio. El tipo `Usuario` contiene información sobre un usuario, como su nombre y los libros que tiene prestados.

También define varias funciones para trabajar con libros y usuarios. La función `nuevoLibro` crea un nuevo libro, la función `nuevoUsuario` crea un nuevo usuario, la función `prestarLibro` presta un libro a un usuario y la función `devolverLibro` devuelve un libro de un usuario.

Por último, el código crea algunos libros y usuarios, presta algunos libros a algunos usuarios y luego devuelve algunos libros de algunos usuarios. Finalmente, muestra los libros prestados de cada usuario.

Explicación del código:

* El código utiliza el sistema de tipos de F#, que es un sistema de tipos estático y fuertemente tipado. Esto significa que los tipos de datos y las funciones deben ser declarados explícitamente, y el compilador comprobará los tipos de los datos y las funciones para asegurarse de que son correctos.
* El código utiliza la sintaxis de F#, que es una sintaxis concisa y expresiva. Por ejemplo, la función `nuevoLibro` se define utilizando la sintaxis de tuplas, que permite crear un nuevo valor de tipo `Libro` proporcionando los valores de los campos del tipo en el orden correcto.
* El código utiliza la biblioteca estándar de F#, que proporciona una amplia gama de funciones y tipos de datos útiles. Por ejemplo, la función `List.iter` se utiliza para iterar sobre una lista y aplicar una función a cada elemento de la lista.
* El código utiliza la función de printf para mostrar los libros prestados de cada usuario. La función de printf toma una cadena de formato y una lista de valores como argumentos, y formatea la cadena de formato utilizando los valores especificados.

Este código es un ejemplo de cómo utilizar F# para trabajar con datos y funciones de forma segura y expresiva.