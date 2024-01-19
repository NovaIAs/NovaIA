```
// Este programa es un sistema de gestión de una biblioteca.
// Tiene las siguientes características:
// * Permite agregar, modificar y eliminar libros.
// * Permite buscar libros por título, autor o ISBN.
// * Permite prestar y devolver libros.
// * Lleva un registro de los usuarios que han prestado libros.

// Declaración de las clases necesarias.

public class Libro
{
    public int ISBN { get; set; }
    public string Titulo { get; set; }
    public string Autor { get; set; }
    public int AñoPublicacion { get; set; }
    public bool Prestado { get; set; }
}

public class Usuario
{
    public int Id { get; set; }
    public string Nombre { get; set; }
    public string Apellidos { get; set; }
    public string Dirección { get; set; }
    public string Teléfono { get; set; }
}

public class Prestamo
{
    public int Id { get; set; }
    public Libro Libro { get; set; }
    public Usuario Usuario { get; set; }
    public DateTime FechaPrestamo { get; set; }
    public DateTime FechaDevolución { get; set; }
}

// Declaración de la clase principal del programa.

public class Biblioteca
{
    // Lista de libros.

    private List<Libro> libros = new List<Libro>();

    // Lista de usuarios.

    private List<Usuario> usuarios = new List<Usuario>();

    // Lista de préstamos.

    private List<Prestamo> prestamos = new List<Prestamo>();

    // Método para agregar un libro.

    public void AgregarLibro(Libro libro)
    {
        libros.Add(libro);
    }

    // Método para modificar un libro.

    public void ModificarLibro(Libro libro)
    {
        int index = libros.IndexOf(libro);
        libros[index] = libro;
    }

    // Método para eliminar un libro.

    public void EliminarLibro(Libro libro)
    {
        libros.Remove(libro);
    }

    // Método para buscar un libro por título.

    public List<Libro> BuscarLibrosPorTítulo(string título)
    {
        return libros.Where(l => l.Título.Contains(título)).ToList();
    }

    // Método para buscar un libro por autor.

    public List<Libro> BuscarLibrosPorAutor(string autor)
    {
        return libros.Where(l => l.Autor.Contains(autor)).ToList();
    }

    // Método para buscar un libro por ISBN.

    public Libro BuscarLibroPorISBN(int isbn)
    {
        return libros.FirstOrDefault(l => l.ISBN == isbn);
    }

    // Método para prestar un libro.

    public void PrestarLibro(Libro libro, Usuario usuario)
    {
        Prestamo prestamo = new Prestamo
        {
            Libro = libro,
            Usuario = usuario,
            FechaPrestamo = DateTime.Now,
            FechaDevolución = DateTime.Now.AddDays(14)
        };

        prestamos.Add(prestamo);
        libro.Prestado = true;
    }

    // Método para devolver un libro.

    public void DevolverLibro(Libro libro)
    {
        Prestamo prestamo = prestamos.FirstOrDefault(p => p.Libro == libro);
        prestamos.Remove(prestamo);
        libro.Prestado = false;
    }

    // Método para obtener el historial de préstamos de un usuario.

    public List<Prestamo> ObtenerHistorialPrestamos(Usuario usuario)
    {
        return prestamos.Where(p => p.Usuario == usuario).ToList();
    }

    // Método para obtener la lista de libros prestados por un usuario.

    public List<Libro> ObtenerLibrosPrestados(Usuario usuario)
    {
        return prestamos.Where(p => p.Usuario == usuario).Select(p => p.Libro).ToList();
    }

    // Método para obtener la lista de usuarios que han prestado un libro.

    public List<Usuario> ObtenerUsuariosQueHanPrestadoUnLibro(Libro libro)
    {
        return prestamos.Where(p => p.Libro == libro).Select(p => p.Usuario).ToList();
    }
}

// Programa principal.

class Program
{
    static void