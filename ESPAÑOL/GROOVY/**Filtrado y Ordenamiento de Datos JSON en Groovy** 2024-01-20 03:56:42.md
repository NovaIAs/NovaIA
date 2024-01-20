```groovy
import groovy.json.JsonSlurper

// Leer un archivo JSON y almacenar los datos en un mapa
def jsonSlurper = new JsonSlurper()
def datos = jsonSlurper.parseText(new File('datos.json').text)

// Crear una lista de objetos con los datos del archivo JSON
def listaObjetos = []
datos.each { objeto ->
  listaObjetos.add(new Objeto(objeto))
}

// Filtrar la lista de objetos para obtener sólo los objetos de un determinado tipo
def objetosFiltrados = listaObjetos.findAll { objeto ->
  objeto.tipo == 'tipo1'
}

// Ordenar la lista de objetos filtrados por una propiedad
def objetosOrdenados = objetosFiltrados.sort { objeto ->
  objeto.nombre
}

// Crear un nuevo archivo JSON con los datos de los objetos ordenados
def nuevoArchivoJSON = new File('nuevos_datos.json')
nuevoArchivoJSON.write(jsonSlurper.encode(objetosOrdenados))

// Definir una clase para representar los objetos
class Objeto {
  String nombre
  String tipo

  Objeto(Map<String, Object> datos) {
    nombre = datos['nombre']
    tipo = datos['tipo']
  }
}
```

Explicación del código:

* El código comienza importando la biblioteca `groovy.json.JsonSlurper`, que se utiliza para leer y escribir archivos JSON.
* A continuación, se lee el archivo `datos.json` y se almacena el contenido en un mapa.
* Se crea una lista de objetos con los datos del archivo JSON.
* Se filtra la lista de objetos para obtener sólo los objetos de un determinado tipo.
* Se ordena la lista de objetos filtrados por una propiedad.
* Se crea un nuevo archivo JSON con los datos de los objetos ordenados.
* Finalmente, se define una clase para representar los objetos.