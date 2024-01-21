```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import groovy.sql.Sql
import groovy.time.TimeCategory

// Definimos las variables que vamos a utilizar
def url = "jdbc:postgresql://localhost:5432/postgres"
def usuario = "postgres"
def contraseña = "password"

// Creamos una conexión a la base de datos
def sql = Sql.newInstance(url, usuario, contraseña)

// Obtenemos los datos de la tabla "clientes"
def clientes = sql.rows("SELECT * FROM clientes")

// Creamos una lista de objetos JSON con los datos de cada cliente
def clientesJson = []
clientes.each { cliente ->
    def clienteJson = [:]
    clienteJson.id = cliente.id
    clienteJson.nombre = cliente.nombre
    clienteJson.apellido = cliente.apellido
    clienteJson.fechaNacimiento = cliente.fecha_nacimiento.format("yyyy-MM-dd")
    clientesJson.add(clienteJson)
}

// Creamos un objeto JSON con los datos de todos los clientes
def clientesJsonTotal = [:]
clientesJsonTotal.clientes = clientesJson

// Convertimos el objeto JSON a una cadena de texto
def clientesJsonString = clientesJsonTotal.toJson()

// Imprimimos la cadena de texto en la consola
println clientesJsonString

// Cerramos la conexión a la base de datos
sql.close()

```

Este código realiza las siguientes tareas:

* Importa las librerías necesarias.
* Define las variables que se van a utilizar.
* Crea una conexión a la base de datos.
* Obtiene los datos de la tabla "clientes".
* Crea una lista de objetos JSON con los datos de cada cliente.
* Crea un objeto JSON con los datos de todos los clientes.
* Convierte el objeto JSON a una cadena de texto.
* Imprime la cadena de texto en la consola.
* Cierra la conexión a la base de datos.

Este código es complejo porque realiza varias tareas diferentes, incluyendo la conexión a una base de datos, la recuperación de datos de una tabla, la conversión de los datos a un formato JSON y la impresión de los datos en la consola. También utiliza varias librerías diferentes, lo que puede hacer que sea difícil de entender. Sin embargo, el código está bien documentado y es fácil de seguir si se tiene un poco de experiencia en programación.