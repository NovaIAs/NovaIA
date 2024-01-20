```groovy
// Importamos las clases necesarias
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import groovy.json.JsonSlurper;
import groovy.json.JsonBuilder;

// Definimos una función para obtener los datos de una URL
def obtenerDatos(url) {
  def jsonSlurper = new JsonSlurper()
  return jsonSlurper.parse(url)
}

// Definimos una función para agrupar los objetos en función de una clave determinada
def agruparPorClave(objetos, clave) {
  def grupos = new HashMap<String, List<Map<String, Object>>>()
  objetos.each { objeto ->
    def key = objeto[clave]
    if (!grupos.containsKey(key)) {
      grupos[key] = new ArrayList<Map<String, Object>>()
    }
    grupos[key].add(objeto)
  }
  return grupos
}

// Definimos una función para filtrar los objetos que cumplan una determinada condición
def filtrar(objetos, condicion) {
  return objetos.findAll(condicion)
}

// Definimos una función para obtener la propiedad id
def obtenerId(objeto) {
  return objeto["id"]
}


// Definimos una función para recuperar los objetos de su nombre
def buscar(nombre, lista) {
  return lista.findAll{ it.nombre == nombre }
}


// Obtenemos los datos de la URL
def datos = obtenerDatos("https://example.com/api/datos")

// Agrupamos los datos por la clave "tipo"
def grupos = agruparPorClave(datos, "tipo")

// Obtenemos los datos del tipo "producto"
def productos = grupos["producto"]

// Filtramos los productos para obtener solo los que tengan un precio mayor a 10
def productosFiltrados = filtrar(productos) { it.precio > 10 }

// Obtenemos los identificadores de los productos filtrados
def idsProductosFiltrados = productosFiltrados.collect(obtenerId)

// Obtenemos los datos de los usuarios
def usuarios = obtenerDatos("https://example.com/api/usuarios")

// Buscamos los usuarios que hayan comprado productos filtrados
def usuariosConProductosFiltrados = usuarios.findAll{ it.compras.any{ idsProductosFiltrados.contains(it) } }

// Creamos un objeto con los datos de los usuarios y los productos filtrados
def resultado = new JsonBuilder()
resultado.usuarios = usuariosConProductosFiltrados
resultado.productos = productosFiltrados

// Mostramos el resultado
println resultado.toPrettyString()
```

Este código obtiene los datos de dos URL diferentes, los agrupa, los filtra y los procesa para obtener un resultado final que es un objeto JSON con los datos de los usuarios y los productos filtrados.

El código está dividido en varias funciones para hacerlo más legible y mantenible. Las funciones se utilizan para agrupar los datos, filtrarlos, obtener la propiedad id y buscar los objetos de su nombre.

El código utiliza la biblioteca Groovy JSON para analizar los datos JSON. También utiliza la biblioteca Groovy Streams para filtrar los datos.

El resultado final es un objeto JSON que contiene los datos de los usuarios y los productos filtrados.
`````