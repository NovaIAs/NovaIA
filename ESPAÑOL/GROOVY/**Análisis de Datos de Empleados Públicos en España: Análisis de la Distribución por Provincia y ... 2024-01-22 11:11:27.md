```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import java.net.URL

// Definimos la URL del API de datos
def url = new URL("https://api.datos.gob.es/apidata/catalog/dataset/empleo-publico/resource/empleados-publicos.json")

// Creamos un cliente HTTP para realizar la petición
def client = new URLConnection(url)

// Realizamos la petición y leemos la respuesta
def response = client.getInputStream().text

// Parseamos la respuesta a JSON
def json = new JsonSlurper().parseText(response)

// Definimos un mapa para almacenar los datos de los empleados públicos
def empleadosPublicos = [:]

// Iteramos sobre los datos JSON y los añadimos al mapa
json.result.records.each { empleado ->
    empleadosPublicos[empleado.nif] = empleado
}

// Imprimimos los datos de los empleados públicos
println empleadosPublicos

// Creamos un nuevo mapa para almacenar los empleados públicos por provincia
def empleadosPublicosPorProvincia = [:]

// Iteramos sobre los empleados públicos y los añadimos al mapa por provincia
empleadosPublicos.each { nif, empleado ->
    if (!empleadosPublicosPorProvincia.containsKey(empleado.provincia)) {
        empleadosPublicosPorProvincia[empleado.provincia] = []
    }
    empleadosPublicosPorProvincia[empleado.provincia].add(empleado)
}

// Imprimimos los datos de los empleados públicos por provincia
println empleadosPublicosPorProvincia

// Creamos un nuevo mapa para almacenar los empleados públicos por sexo
def empleadosPublicosPorSexo = [:]

// Iteramos sobre los empleados públicos y los añadimos al mapa por sexo
empleadosPublicos.each { nif, empleado ->
    if (!empleadosPublicosPorSexo.containsKey(empleado.sexo)) {
        empleadosPublicosPorSexo[empleado.sexo] = []
    }
    empleadosPublicosPorSexo[empleado.sexo].add(empleado)
}

// Imprimimos los datos de los empleados públicos por sexo
println empleadosPublicosPorSexo
```

Este código realiza una petición a la API de datos del gobierno español para obtener los datos de los empleados públicos.

Una vez obtenidos los datos, los parsea a JSON y los almacena en un mapa.

Después, crea nuevos mapas para almacenar los datos de los empleados públicos por provincia y por sexo.

Por último, imprime los datos de los empleados públicos por provincia y por sexo.

Este código es complejo porque realiza varias operaciones en cadena y utiliza múltiples mapas para almacenar los datos.