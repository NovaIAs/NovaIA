```go
// Creación de un Servidor Web en GO que permita subir archivos y mostrarlos.

// Importación de los paquetes necesarios.
package main

import (
    "fmt"
    "io"
    "log"
    "net/http"
    "os"

    "github.com/gorilla/mux"
)

// Función principal.
func main() {
    // Creación de un nuevo servidor mux.
    r := mux.NewRouter()

    // Definición de la ruta para subir archivos.
    r.HandleFunc("/upload", UploadFile)

    // Definición de la ruta para mostrar los archivos.
    r.HandleFunc("/show", ShowFiles)

    // Inicio del servidor en el puerto 8080.
    log.Println("Iniciando el servidor en el puerto 8080...")
    http.ListenAndServe(":8080", r)
}

// Función para subir archivos.
func UploadFile(w http.ResponseWriter, r *http.Request) {
    // Validación del método de la petición.
    if r.Method != "POST" {
        http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
        return
    }

    // Obtención del archivo del formulario.
    file, _, err := r.FormFile("file")
    if err != nil {
        http.Error(w, "Error al obtener el archivo", http.StatusInternalServerError)
        return
    }
    defer file.Close()

    // Creación del directorio de subida si no existe.
    err = os.MkdirAll("./uploads", os.ModePerm)
    if err != nil {
        http.Error(w, "Error al crear el directorio de subida", http.StatusInternalServerError)
        return
    }

    // Generación del nombre del archivo.
    fileName := fmt.Sprintf("./uploads/%s", file.Filename)

    // Creación del archivo en el directorio de subida.
    f, err := os.Create(fileName)
    if err != nil {
        http.Error(w, "Error al crear el archivo", http.StatusInternalServerError)
        return
    }

    // Copia del contenido del archivo en el nuevo archivo.
    _, err = io.Copy(f, file)
    if err != nil {
        http.Error(w, "Error al copiar el archivo", http.StatusInternalServerError)
        return
    }

    // Cierre del nuevo archivo.
    f.Close()

    // Envío de una respuesta de éxito.
    fmt.Fprintln(w, "Archivo subido correctamente")
}

// Función para mostrar los archivos.
func ShowFiles(w http.ResponseWriter, r *http.Request) {
    // Obtención del directorio de subida.
    files, err := os.ReadDir("./uploads")
    if err != nil {
        http.Error(w, "Error al obtener el directorio de subida", http.StatusInternalServerError)
        return
    }

    // Generación de la lista de archivos.
    var fileList []string
    for _, file := range files {
        fileList = append(fileList, file.Name())
    }

    // Envío de la lista de archivos en formato JSON.
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(fileList)
}
```

Explicación:

* El código anterior crea un servidor web en GO que nos permitirá subir archivos y mostrarlos.
* El servidor web se crea utilizando la biblioteca `gorilla/mux`.
* Tenemos dos rutas definidas:
    * La ruta `/upload` es utilizada para subir archivos.
    * La ruta `/show` es utilizada para mostrar los archivos subidos.
* La función `UploadFile` se encarga de gestionar la subida de archivos.
* La función `ShowFiles` se encarga de mostrar los archivos subidos.
* El servidor web se inicia en el puerto 8080.

Este código es bastante complejo y tiene muchas partes móviles. Sin embargo, es un ejemplo muy útil de cómo crear un servidor web en GO que pueda subir y mostrar archivos.