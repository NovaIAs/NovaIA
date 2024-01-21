```typescript
// Definición de la interfaz de un libro
interface Libro {
  titulo: string;
  autor: string;
  fechaPublicacion: Date;
  paginas: number;
  genero: string;
}

// Definición de la clase Biblioteca
class Biblioteca {
  // Atributos de la biblioteca
  libros: Libro[];

  constructor() {
    // Inicialización de la lista de libros
    this.libros = [];
  }

  // Método para añadir un libro a la biblioteca
  agregarLibro(libro: Libro) {
    this.libros.push(libro);
  }

  // Método para eliminar un libro de la biblioteca
  eliminarLibro(libro: Libro) {
    const indice = this.libros.indexOf(libro);
    if (indice >= 0) {
      this.libros.splice(indice, 1);
    }
  }

  // Método para buscar un libro por su título
  buscarLibroPorTitulo(titulo: string): Libro | undefined {
    return this.libros.find((libro) => libro.titulo === titulo);
  }

  // Método para buscar un libro por su autor
  buscarLibroPorAutor(autor: string): Libro[] {
    return this.libros.filter((libro) => libro.autor === autor);
  }

  // Método para obtener todos los libros de un género específico
  obtenerLibrosPorGenero(genero: string): Libro[] {
    return this.libros.filter((libro) => libro.genero === genero);
  }

  // Método para obtener todos los libros publicados en un año específico
  obtenerLibrosPorAñoPublicacion(año: number): Libro[] {
    return this.libros.filter((libro) => libro.fechaPublicacion.getFullYear() === año);
  }

  // Método para ordenar los libros por título
  ordenarLibrosPorTitulo() {
    this.libros.sort((a, b) => a.titulo.localeCompare(b.titulo));
  }

  // Método para obtener la lista de todos los libros en la biblioteca
  obtenerListaDeLibros(): string[] {
    return this.libros.map((libro) => `${libro.titulo} - ${libro.autor}`);
  }
}

// Crear una instancia de la biblioteca
const biblioteca = new Biblioteca();

// Agregar algunos libros a la biblioteca
biblioteca.agregarLibro({
  titulo: "El Quijote",
  autor: "Miguel de Cervantes",
  fechaPublicacion: new Date(1605, 0, 16),
  paginas: 1023,
  genero: "Novela",
});
biblioteca.agregarLibro({
  titulo: "Cien años de soledad",
  autor: "Gabriel García Márquez",
  fechaPublicacion: new Date(1967, 5, 5),
  paginas: 417,
  genero: "Novela",
});
biblioteca.agregarLibro({
  titulo: "El principito",
  autor: "Antoine de Saint-Exupéry",
  fechaPublicacion: new Date(1943, 3, 6),
  paginas: 98,
  genero: "Novela corta",
});

// Buscar un libro por su título
const libroBuscadoPorTitulo = biblioteca.buscarLibroPorTitulo("El Quijote");
console.log(`Libro encontrado por título: ${libroBuscadoPorTitulo?.titulo}`);

// Buscar un libro por su autor
const librosBuscadosPorAutor = biblioteca.buscarLibroPorAutor("Gabriel García Márquez");
console.log(`Libros encontrados por autor: ${librosBuscadosPorAutor.map((libro) => libro.titulo)}`);

// Obtener todos los libros de un género específico
const librosPorGenero = biblioteca.obtenerLibrosPorGenero("Novela");
console.log(`Libros encontrados por género: ${librosPorGenero.map((libro) => libro.titulo)}`);

// Obtener todos los libros publicados en un año específico
const librosPorAñoPublicacion = biblioteca.obtenerLibrosPorAñoPublicacion(1967);
console.log(`Libros encontrados por año de publicación: ${librosPorAñoPublicacion.map((libro) => libro.titulo)}`);

// Ordenar los libros por título
biblioteca.ordenarLibrosPorTitulo();

// Obtener la lista de todos los libros en la biblioteca
const listaDeLibros = biblioteca.obtenerListaDeLibros();
console.log("Lista de todos los libros:");
listaDeLibros.forEach((libro) => console.log(libro));
```

Explicación del código:

* Se define una interfaz `Libro` para representar la información de un libro.
* Se define una clase `Biblioteca` que contiene una lista de libros y proporciona métodos para agregar, eliminar, buscar y ordenar libros.
* Se crea una instancia de la biblioteca y se agregan algunos libros a ella.
* Se utilizan los métodos de la biblioteca para buscar libros por título, autor, género y año de publicación.
* Se ordena la lista de libros por título y se obtiene una lista de todos los libros en la biblioteca.
* Se muestran los resultados en la consola.