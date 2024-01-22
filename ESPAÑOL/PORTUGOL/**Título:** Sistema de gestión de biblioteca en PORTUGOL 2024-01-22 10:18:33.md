**Nombre del código:** Sistema de gestión de biblioteca

**Objetivo:** Desarrollar un código en PORTUGOL que gestione una biblioteca, permitiendo registrar libros, prestarlos a los usuarios y mantener un control de los mismos.

**Código:**

```portuol
// Definición de constantes
Constante MAX_USUARIOS = 100
Constante MAX_LIBROS = 500
Constante MAX_PRESTAMOS = 1000

// Definición de tipos de datos
Tipo Libro = Registro
    Título: String
    Autor: String
    Año: Entero
    Género: String
    Disponibilidad: Booleano
Fin Registro

Tipo Usuario = Registro
    Nombre: String
    Apellido: String
    Dirección: String
    Teléfono: String
    Préstamos: [MAX_PRESTAMOS] de Libro
Fin Registro

Tipo Biblioteca = Registro
    Libros: [MAX_LIBROS] de Libro
    Usuarios: [MAX_USUARIOS] de Usuario
    Préstamos: [MAX_PRESTAMOS] de Libro
Fin Registro

// Declaración de variables
Biblioteca biblioteca

// Inicialización de la biblioteca
biblioteca.Libros = []
biblioteca.Usuarios = []
biblioteca.Préstamos = []

// Función para registrar un libro
Función RegistrarLibro(Título: String; Autor: String; Año: Entero; Género: String)
    Libro libro = CrearLibro(Título, Autor, Año, Género)
    biblioteca.Libros.Añadir(libro)
Fin Función

// Función para registrar un usuario
Función RegistrarUsuario(Nombre: String; Apellido: String; Dirección: String; Teléfono: String)
    Usuario usuario = CrearUsuario(Nombre, Apellido, Dirección, Teléfono)
    biblioteca.Usuarios.Añadir(usuario)
Fin Función

// Función para prestar un libro
Función PrestarLibro(Usuario: Usuario; Libro: Libro)
    si Libro.Disponibilidad es Verdadero Entonces
        Libro.Disponibilidad = Falso
        Préstamo préstamo = CrearPréstamo(Libro, Usuario)
        biblioteca.Préstamos.Añadir(préstamo)
    Fin Si
Fin Función

// Función para devolver un libro
Función DevolverLibro(Usuario: Usuario; Libro: Libro)
    si Libro.Disponibilidad es Falso Entonces
        Libro.Disponibilidad = Verdadero
        Préstamo préstamo = BuscarPréstamo(Libro, Usuario)
        biblioteca.Préstamos.Eliminar(préstamo)
    Fin Si
Fin Función

// Función para buscar un préstamo
Función BuscarPréstamo(Libro: Libro; Usuario: Usuario): Préstamo
    Para Cada Préstamo en biblioteca.Préstamos Hacer
        si Préstamo.Libro es Libro y Préstamo.Usuario es Usuario Entonces
            Devolver Préstamo
        Fin Si
    Fin Para
    Devolver Ninguno
Fin Función

// Función para crear un libro
Función CrearLibro(Título: String; Autor: String; Año: Entero; Género: String): Libro
    Libro libro = Nuevo Libro
    libro.Título = Título
    libro.Autor = Autor
    libro.Año = Año
    libro.Género = Género
    libro.Disponibilidad = Verdadero
    Devolver libro
Fin Función

// Función para crear un usuario
Función CrearUsuario(Nombre: String; Apellido: String; Dirección: String; Teléfono: String): Usuario
    Usuario usuario = Nuevo Usuario
    usuario.Nombre = Nombre
    usuario.Apellido = Apellido
    usuario.Dirección = Dirección
    usuario.Teléfono = Teléfono
    usuario.Préstamos = []
    Devolver usuario
Fin Función

// Función para crear un préstamo
Función CrearPréstamo(Libro: Libro; Usuario: Usuario): Préstamo
    Préstamo préstamo = Nuevo Préstamo
    préstamo.Libro = Libro
    préstamo.Usuario = Usuario
    préstamo.FechaPréstamo = FechaActual()
    préstamo.FechaDevolución = FechaActual() + 30
    Devolver préstamo
Fin Función


// Función principal
Inicio
    RegistrarLibro("El Quijote", "Miguel de Cervantes", 1605, "Novela")
    RegistrarLibro("La metamorfosis", "Franz Kafka", 1915, "Novela corta")
    RegistrarLibro("Cien años de soledad", "Gabriel García Márquez", 1967, "Novela")

    RegistrarUsuario("Juan", "Pérez", "Calle 123", "12345678")
    RegistrarUsuario("María", "González", "Calle 456", "87654321")

    PrestarLibro(biblioteca.Usuarios[0], biblioteca.Libros[0])
    PrestarLibro(biblioteca.Usuarios[1], biblioteca.Libros[1])

    DevolverLibro(biblioteca.Usuarios[0], biblioteca.Libros[0])

    Para Cada Libro en biblioteca.Libros Hacer
        Escribir Libro.Título, " por ", Libro.Autor
    Fin Para

    Para Cada Usuario en biblioteca.Usuarios Hacer
        Escribir Usuario.Nombre, " ", Usuario.Apellido, " ha prestado los siguientes libros:"
        Para Cada Préstamo en Usuario.Préstamos Hacer
            Escribir " - ", Préstamo.Libro.Título
        Fin Para
    Fin Para
Fin
```

Explicación del código:

* El código define primero una serie de constantes, tipos de datos y variables. Las constantes se utilizan para definir el tamaño máximo de los arrays que se utilizan para almacenar los libros, los usuarios y los préstamos. Los tipos de datos se utilizan para definir las estructuras de los libros, los usuarios y la biblioteca. La variable `biblioteca` se utiliza para almacenar la información de la biblioteca.
* Las funciones `RegistrarLibro()`, `RegistrarUsuario()`, `PrestarLibro()`, `DevolverLibro()`, `BuscarPréstamo()`, `CrearLibro()`, `CrearUsuario()` y `CrearPréstamo()` se utilizan para realizar las operaciones CRUD (Crear, Leer, Actualizar y Eliminar) sobre los libros, los usuarios y los préstamos.
* La función `main()` se utiliza para inicializar la biblioteca y ejecutar las operaciones CRUD sobre los libros, los usuarios y los préstamos.

El código también utiliza los siguientes conceptos:

* Arrays: Los arrays se utilizan para almacenar los libros, los usuarios y los préstamos.
* Estructuras: Las estructuras se utilizan para definir los libros, los usuarios y la biblioteca.
* Funciones: Las funciones se utilizan para realizar las operaciones CRUD sobre los libros, los usuarios y los préstamos.
* Variables: Las variables se utilizan para almacenar los datos de la biblioteca.

El código es complejo y difícil de repetir, ya que utiliza una serie de conceptos avanzados de PORTUGOL. Sin embargo, es un ejemplo de cómo se puede utilizar PORTUGOL para desarrollar aplicaciones complejas y funcionales.