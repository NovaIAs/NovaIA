```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import java.net.URL

// Definimos la URL de la API que vamos a consumir
String url = "https://jsonplaceholder.typicode.com/posts"

// Creamos una instancia de la clase URL para obtener el contenido de la API
URL urlObject = new URL(url)

// Abrimos una conexión con la URL y obtenemos el contenido en formato JSON
URLConnection connection = urlObject.openConnection()
String json = connection.getInputStream().text

// Creamos una instancia de la clase JsonSlurper para parsear el JSON
JsonSlurper slurper = new JsonSlurper()

// Parseamos el JSON y lo convertimos en una lista de objetos
List<Map> posts = slurper.parseText(json)

// Iteramos sobre la lista de objetos e imprimimos los títulos de los posts
for (Map post in posts) {
    println post.title
}

// Creamos una nueva lista para almacenar los títulos de los posts que contienen la palabra "dolorem"
List<String> dolorPosts = []

// Iteramos sobre la lista de objetos y añadimos los títulos de los posts que contienen la palabra "dolorem" a la nueva lista
for (Map post in posts) {
    if (post.title.contains("dolorem")) {
        dolorPosts.add(post.title)
    }
}

// Imprimimos la lista de títulos de los posts que contienen la palabra "dolorem"
println dolorPosts

// Creamos un mapa para almacenar los títulos de los posts y sus autores
Map<String, String> postAuthors = [:]

// Iteramos sobre la lista de objetos y añadimos los títulos de los posts y sus autores al mapa
for (Map post in posts) {
    postAuthors[post.title] = post.author
}

// Imprimimos el mapa de títulos de los posts y sus autores
println postAuthors
```

Este código consume la API de JSONPlaceholder y obtiene una lista de posts. Luego, itera sobre la lista de posts e imprime los títulos de los posts. A continuación, crea una nueva lista para almacenar los títulos de los posts que contienen la palabra "dolorem" e itera sobre la lista de posts para añadir los títulos de los posts que contienen la palabra "dolorem" a la nueva lista. Finalmente, imprime la lista de títulos de los posts que contienen la palabra "dolorem". Por último, crea un mapa para almacenar los títulos de los posts y sus autores e itera sobre la lista de posts para añadir los títulos de los posts y sus autores al mapa. Por último, imprime el mapa de títulos de los posts y sus autores.