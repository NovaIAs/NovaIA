```d
// Perspectiva sobre manejo de memoria manual.
//
// Cuando se trata de manejar memoria existen solamente dos posibles
// perspectivas:
// - El compilador se encarga de todo, asigna y desasigna la memoria por
//   nosotros.
// - El programador tiene control total de la memoria y es responsable de
//   asignaciones y desasignaciones de memoria.
//
// D es un lenguaje que permite ambas perspectivas. Por defecto D utiliza
// manejo de memoria automático (garbage collection), sin embargo permite
// utilizar manejo de memoria manual (resource management) con RAII si se
// desea.
//
// En este script se explicará cómo se maneja la memoria de forma manual
// utilizando RAII en D.
//
// ¿Qué es RAII?
//
// RAII son las siglas de Resource Acquisition Is Initialization. RAII es una
// técnica de programación que se utiliza para asegurar que los recursos
// utilizados por un objeto sean liberados automáticamente cuando el objeto
// se destruye.
//
// ¿Cómo funciona RAII en D?
//
// En D, RAII se implementa utilizando el concepto de "scope". Un scope es
// una región del código donde se declaran las variables. Cuando se sale de
// un scope, todas las variables declaradas en ese scope se destruyen.
//
// D tiene un tipo de dato especial llamado "resource handle". Un resource
// handle es una variable que encapsula un recurso. Cuando se declara una
// variable de tipo resource handle, el recurso encapsulado se adquiere
// automáticamente. Cuando se destruye la variable, el recurso encapsulado
// se libera automáticamente.
//
// Ejemplo de uso de RAII en D
//
// El siguiente código muestra cómo se utiliza RAII en D para gestionar la
// memoria de un archivo:
```d
import std.stdio, std.io;

class File {
    private immutable FileHandle handle;

    // El constructor adquiere el recurso.
    this(in string filename) {
        handle = FileHandle.open(filename, FileMode.ReadOnly);
    }

    // El destructor libera el recurso.
    ~this() {
        handle.close();
    }

    // Métodos para leer del archivo.
    immutable string readLine() {
        return handle.readLine();
    }

    void readLines(Action!string line) {
        while (!handle.eof) {
            line(handle.readLine());
        }
    }
}

void main() {
    File file = File.new("myfile.txt");

    // El recurso es liberado automáticamente cuando
    // se sale del scope.
    file.readLines!print;
}
```
```
// Explicación del código:

// Se importan las librerías necesarias.

// Se define una clase llamada `File` que encapsula el recurso del archivo.

// El constructor de la clase `File` adquiere el recurso del archivo
// utilizando la función `FileHandle.open()`.

// El destructor de la clase `File` libera el recurso del archivo
// utilizando la función `FileHandle.close()`.

// La clase `File` define dos métodos para leer del archivo: `readLine()`
// y `readLines()`.

// En la función `main()`, se crea una instancia de la clase `File` y se
// llama al método `readLines()` para leer las líneas del archivo y
// mostrarlas por consola.

// El recurso del archivo se libera automáticamente cuando se sale del scope
// donde se creó la instancia de la clase `File`.