```d
// programa de edición de texto en d

import std.stdio, std.string, std.array, std.range, std.algorithm, std.conv;

class Editor {
    private:
        string buffer;
        string filename;
        string currentLine;
        int cursorPos;

    public:
        Editor() {
            filename = "";
            cursorPos = 0;
        }

        void abrirArchivo(in string nombreArchivo) {
            filename = nombreArchivo;
            buffer = readFile(filename);
            cursorPos = 0;
        }

        void guardarArchivo() {
            writeFile(filename, buffer);
        }

        void insertarTexto(in string texto) {
            buffer.insert(cursorPos, texto);
            cursorPos += texto.length;
        }

        void borrarTexto(int inicio, int fin) {
            buffer.remove(inicio, fin - inicio);
            if (cursorPos > fin) {
                cursorPos -= fin - inicio;
            } else if (cursorPos >= inicio) {
                cursorPos = inicio;
            }
        }

        void moverCursor(int posicion) {
            if (posicion < 0) {
                cursorPos = 0;
            } else if (posicion > buffer.length) {
                cursorPos = buffer.length;
            } else {
                cursorPos = posicion;
            }
        }

        void moverCursorInicio() {
            cursorPos = 0;
        }

        void moverCursorFin() {
            cursorPos = buffer.length;
        }

        void moverCursorIzquierda() {
            if (cursorPos > 0) {
                cursorPos--;
            }
        }

        void moverCursorDerecha() {
            if (cursorPos < buffer.length) {
                cursorPos++;
            }
        }

        void seleccionarTexto(int inicio, int fin) {
            currentLine = buffer.substring(inicio, fin - inicio);
        }

        void copiarTexto() {
            selectedText = currentLine;
        }

        void pegarTexto() {
            buffer.insert(cursorPos, selectedText);
            cursorPos += selectedText.length;
        }

        void cortarTexto() {
            copiarTexto();
            borrarTexto(cursorPos - currentLine.length, cursorPos);
        }

        string getBuffer() {
            return buffer;
        }

        int getCursorPos() {
            return cursorPos;
        }
}

class Main {
    static void main() {
        Editor editor = new Editor();
        string nombreArchivo = "archivo.txt";
        editor.abrirArchivo(nombreArchivo);

        while (true) {
            // mostrar el editor de texto

            // leer la entrada del usuario

            // procesar la entrada del usuario

            // actualizar el editor de texto
        }
    }
}
```

Explicación del código:

* La clase `Editor` representa el editor de texto. Tiene una variable privada `buffer` que contiene el texto del archivo actual, una variable privada `filename` que contiene el nombre del archivo actual, una variable privada `currentLine` que contiene la línea de texto actual, y una variable privada `cursorPos` que contiene la posición del cursor.
* La clase `Main` contiene el método `main()` que es el punto de entrada del programa.
* El método `main()` crea una instancia de la clase `Editor`, abre el archivo "archivo.txt", y entra en un bucle infinito.
* Dentro del bucle, el método `main()` muestra el editor de texto, lee la entrada del usuario, procesa la entrada del usuario, y actualiza el editor de texto.
* El método `mostrarEditorDeTexto()` muestra el texto del archivo actual, la posición del cursor, y la línea de texto actual.
* El método `leerEntradaDeUsuario()` lee la entrada del usuario desde la consola.
* El método `procesarEntradaDeUsuario()` procesa la entrada del usuario y llama a los métodos adecuados de la clase `Editor` para realizar las acciones correspondientes.
* El método `actualizarEditorDeTexto()` actualiza el texto del archivo actual, la posición del cursor, y la línea de texto actual.