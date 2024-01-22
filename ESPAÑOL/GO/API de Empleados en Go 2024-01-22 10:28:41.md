```
// Importación de las bibliotecas necesarias
package main

import (
    "fmt"
    "log"
    "net/http"
    "os"
    "context"
    "io"
    "encoding/json"
)

// Definición de la estructura que representa los datos de un empleado
type Empleado struct {
    ID              int
    Nombre          string
    Apellido        string
    Departamento    string
    Salario         float64
}

// Definición del slice que contendrá los datos de los empleados
var empleados []Empleado

// Función principal del programa
func main() {
    // Inicialización del slice de empleados
    empleados = []Empleado{
        {1, "Juan", "García", "Ventas", 20000},
        {2, "María", "Pérez", "Marketing", 25000},
        {3, "Pedro", "López", "Contabilidad", 30000},
    }

    // Creación del servidor HTTP
    http.HandleFunc("/", IndexHandler)
    http.HandleFunc("/empleados", EmpleadosHandler)
    http.HandleFunc("/empleados/", EmpleadoHandler)
    http.HandleFunc("/empleados/", EmpleadoHandlerPost)
    http.HandleFunc("/empleados/", EmpleadoHandlerPut)
    http.HandleFunc("/empleados/", EmpleadoHandlerDelete)

    // Inicio del servidor HTTP en el puerto 8080
    log.Fatal(http.ListenAndServe(":8080", nil))
}

// Manejador de la ruta raíz ("/")
func IndexHandler(w http.ResponseWriter, r *http.Request) {
    fmt.Fprintf(w, "Bienvenido a la API de empleados")
}

// Manejador de la ruta "/empleados"
func EmpleadosHandler(w http.ResponseWriter, r *http.Request) {
    // Verificación del método HTTP
    if r.Method != "GET" {
        http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
        return
    }

    // Codificación de los datos de los empleados en formato JSON
    data, err := json.Marshal(empleados)
    if err != nil {
        http.Error(w, "Error al codificar los datos", http.StatusInternalServerError)
        return
    }

    // Envío de los datos en formato JSON al cliente
    w.Header().Set("Content-Type", "application/json")
    w.Write(data)
}

// Manejador de la ruta "/empleados/{id}"
func EmpleadoHandler(w http.ResponseWriter, r *http.Request) {
    // Obtención del ID del empleado de la URL
    id := r.URL.Path[len("/empleados/"):]

    // Búsqueda del empleado con el ID especificado
    empleado, err := FindEmpleado(id)
    if err != nil {
        http.Error(w, "Empleado no encontrado", http.StatusNotFound)
        return
    }

    // Codificación de los datos del empleado en formato JSON
    data, err := json.Marshal(empleado)
    if err != nil {
        http.Error(w, "Error al codificar los datos", http.StatusInternalServerError)
        return
    }

    // Envío de los datos en formato JSON al cliente
    w.Header().Set("Content-Type", "application/json")
    w.Write(data)
}

// Manejador de la ruta "/empleados/{id}" para el método POST
func EmpleadoHandlerPost(w http.ResponseWriter, r *http.Request) {
    // Verificación del método HTTP
    if r.Method != "POST" {
        http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
        return
    }

    // Lectura del cuerpo de la petición HTTP
    body, err := io.ReadAll(r.Body)
    if err != nil {
        http.Error(w, "Error al leer el cuerpo de la petición", http.StatusBadRequest)
        return
    }

    // Decodificación de los datos del empleado en formato JSON
    var empleado Empleado
    if err := json.Unmarshal(body, &empleado); err != nil {
        http.Error(w, "Error al decodificar los datos del empleado", http.StatusBadRequest)
        return
    }

    // Creación del nuevo empleado
    nuevoEmpleado, err := CreateEmpleado(empleado)
    if err != nil {
        http.Error(w, "Error al crear el empleado", http.StatusInternalServerError)
        return
    }

    // Codificación de los datos del nuevo empleado en formato JSON
    data, err := json.Marshal(nuevoEmpleado)
    if err != nil {
        http.Error(w, "Error al codificar los datos del empleado", http.StatusInternalServerError)
        return
    }

    // Envío de los datos en formato JSON al cliente
    w.Header().Set("Content-Type", "application/json")
    w.Write(data)
}

// Manejador de la ruta "/empleados/{id}" para el método PUT
func EmpleadoHandlerPut(w http.ResponseWriter, r *http.Request) {
    // Verificación del método HTTP
    if r.Method != "PUT" {
        http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
        return
    }

    // Obtención del ID del empleado de la URL
    id := r.URL.Path[len("/empleados/"):]

    // Lectura del cuerpo de la petición HTTP
    body, err := io.ReadAll(r.Body)
    if err != nil {
        http.Error(w, "Error al leer el cuerpo de la petición", http.StatusBadRequest)
        return
    }

    // Decodificación de los datos del empleado en formato JSON
    var empleado Empleado
    if err := json.Unmarshal(body, &empleado); err != nil {
        http.Error(w, "Error al decodificar los datos del empleado", http.StatusBadRequest)
        return
    }

    // Actualización del empleado
    empleadoActualizado, err := UpdateEmpleado(id, empleado)
    if err != nil {
        http.Error(w, "Error al actualizar el empleado", http.StatusInternalServerError)
        return
    }

    // Codificación de los datos del empleado actualizado en formato JSON
    data, err := json.Marshal(empleadoActualizado)
    if err != nil {
        http.Error(w, "Error al codificar los datos del empleado", http.StatusInternalServerError)
        return
    }

    // Envío de los datos en formato JSON al cliente
    w.Header().Set("Content-Type", "application/json")
    w.Write(data)
}

// Manejador de la ruta "/empleados/{id}" para el método DELETE
func EmpleadoHandlerDelete(w http.ResponseWriter, r *http.Request) {
    // Verificación del método HTTP
    if r.Method != "DELETE" {
        http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
        return
    }

    // Obtención del ID del empleado de la URL
    id := r.URL.Path[len("/empleados/"):]

    // Eliminación del empleado
    if err := DeleteEmpleado(id); err != nil {
        http.Error(w, "Error al eliminar el empleado", http.StatusInternalServerError)
        return
    }

    // Envío de una respuesta vacía al cliente
    w.WriteHeader(http.StatusNoContent)
}

// Función para buscar un empleado por su ID
func FindEmpleado(id string) (Empleado, error) {
    for _, empleado := range empleados {
        if empleado.ID == id {
            return empleado, nil
        }
    }
    return Empleado{}, fmt.Errorf("Empleado no encontrado")
}

// Función para crear un nuevo empleado
func CreateEmpleado(empleado Empleado) (Empleado, error) {
    // Asignación del nuevo ID
    empleado.ID = len(empleados) + 1

    // Adición del nuevo empleado al slice
    empleados = append(empleados, empleado)

    return empleado, nil
}

// Función para actualizar un empleado
func UpdateEmpleado(id string, empleado Empleado) (Empleado, error) {
    for i, empleado := range empleados {
        if empleado.ID == id {
            empleados[i] = empleado
            return empleado, nil
        }
    }
    return Empleado{}, fmt.Errorf("Empleado no encontrado")
}

// Función para eliminar un empleado
func DeleteEmpleado(id string) error {
    for i, empleado := range empleados {
        if empleado.ID == id {
            empleados = append(empleados[:i], empleados[i+1:]...)
            return nil
        }
    }
    return fmt.Errorf("Empleado no encontrado")
}
```