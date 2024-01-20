```groovy
// Importamos las librerías necesarias
import groovy.xml.MarkupBuilder
import java.sql.DriverManager

// Definimos el nombre de la base de datos y la conexión
def dbName = 'mi_base_de_datos'
def dbUrl = "jdbc:mysql://localhost:3306/$dbName"
def dbUser = 'root'
def dbPassword = 'password'

// Establecemos la conexión a la base de datos
def connection = DriverManager.getConnection(dbUrl, dbUser, dbPassword)

// Creamos una lista de nuevos datos para insertar en la base de datos
def newData = [
    ["Juan", "Pérez", 25],
    ["María", "Gómez", 30],
    ["Pedro", "López", 35]
]

// Creamos un script SQL para insertar los nuevos datos
def sqlInsert = "INSERT INTO clientes (nombre, apellido, edad) VALUES (?, ?, ?)"

// Preparamos la declaración SQL
def preparedStatement = connection.prepareStatement(sqlInsert)

// Iteramos sobre la lista de nuevos datos y los insertamos en la base de datos
newData.each { data ->
    preparedStatement.setString(1, data[0])
    preparedStatement.setString(2, data[1])
    preparedStatement.setInt(3, data[2])

    preparedStatement.executeUpdate()
}

// Cerramos la conexión a la base de datos
connection.close()

// Generamos un archivo XML con los datos de los nuevos clientes
def xmlBuilder = new MarkupBuilder()

xmlBuilder.clientes {
    newData.each { data ->
        cliente(nombre: data[0], apellido: data[1], edad: data[2])
    }
}

// Guardamos el archivo XML en el disco
def xmlFile = new File("clientes.xml")
xmlFile.write(xmlBuilder.toString())

// Imprimimos un mensaje de éxito
println "Los nuevos datos se han insertado correctamente en la base de datos y en el archivo XML."
```

Este código realiza las siguientes tareas:

- Primero, importa las librerías necesarias para trabajar con bases de datos MySQL y para generar archivos XML.

- Después, establece la conexión a la base de datos especificada en las variables `dbName`, `dbUrl`, `dbUser` y `dbPassword`.

- Se crea una lista de nuevos datos para insertar en la base de datos. Cada elemento de la lista es un array con tres valores: el nombre, el apellido y la edad.

- A continuación, se crea un script SQL para insertar los nuevos datos en la tabla `clientes` de la base de datos especificada.

- Se prepara la declaración SQL y se itera sobre la lista de nuevos datos para insertar cada uno de ellos en la base de datos.

- Se cierra la conexión a la base de datos.

- Se genera un archivo XML con los datos de los nuevos clientes utilizando la librería `groovy.xml.MarkupBuilder`.

- Por último, se guarda el archivo XML en el disco y se imprime un mensaje de éxito.