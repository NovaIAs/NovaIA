```go
package main

import (
    "fmt"
    "log"
    "net/http"
    "sync"

    "golang.org/x/text/language"
    "golang.org/x/text/message"
)

// Crear un servidor web que responda con un mensaje personalizado en diferentes idiomas.

// Mapeo de códigos de idioma a mensajes personalizados.
var mensajes = map[string]string{
    "es": "Hola, mundo!",
    "en": "Hello, world!",
    "fr": "Bonjour, le monde!",
    "de": "Hallo, Welt!",
    "it": "Ciao, mondo!",
}

// Crear un grupo de espera para sincronizar el acceso al mapa de mensajes.
var mutex sync.Mutex

// Función para obtener el mensaje personalizado en función del código de idioma.
func obtenerMensaje(códigoIdioma string) string {
    // Bloquear el acceso al mapa de mensajes.
    mutex.Lock()
    defer mutex.Unlock()

    // Obtener el mensaje personalizado del mapa.
    mensaje, ok := mensajes[códigoIdioma]
    if !ok {
        // Si el código de idioma no existe en el mapa, devolver un mensaje genérico.
        return "Hello, world!"
    }

    // Devolver el mensaje personalizado.
    return mensaje
}

// Función para manejar las solicitudes HTTP.
func controlador(w http.ResponseWriter, r *http.Request) {
    // Obtener el código de idioma de la solicitud.
    códigoIdioma := r.Header.Get("Accept-Language")

    // Obtener el mensaje personalizado en función del código de idioma.
    mensaje := obtenerMensaje(códigoIdioma)

    // Escribir el mensaje personalizado en la respuesta HTTP.
    fmt.Fprintf(w, mensaje)
}

// Función principal.
func main() {
    // Crear un servidor HTTP.
    http.HandleFunc("/", controlador)

    // Iniciar el servidor HTTP.
    log.Fatal(http.ListenAndServe(":8080", nil))
}

```

Este código crea un servidor web que responde con un mensaje personalizado en diferentes idiomas. El servidor utiliza el encabezado `Accept-Language` de la solicitud HTTP para determinar el idioma del usuario y luego utiliza un mapa para obtener el mensaje personalizado correspondiente. El servidor también utiliza un grupo de espera para sincronizar el acceso al mapa de mensajes, lo que garantiza que el mapa no se modifique mientras se está usando.

Para utilizar el servidor, primero debe instalarlo en su sistema. Puede hacerlo utilizando el siguiente comando:

```
go install main.go
```

Una vez que el servidor esté instalado, puede iniciarlo ejecutando el siguiente comando:

```
main
```

El servidor se iniciará en el puerto 8080. Puede acceder al servidor abriendo un navegador web y escribiendo la siguiente URL:

```
http://localhost:8080
```

El servidor responderá con un mensaje personalizado en función del idioma de su navegador.