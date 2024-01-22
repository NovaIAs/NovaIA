```groovy
// Importamos las librerías necesarias
import groovy.json.JsonSlurper
import groovy.sql.Sql
import java.sql.Timestamp

// Creamos una conexión a la base de datos
def sql = Sql.connect('jdbc:mysql://localhost:3306/base_de_datos', 'usuario', 'contraseña')

// Creamos una tabla si no existe
sql.execute('''
CREATE TABLE IF NOT EXISTS tabla (
  id INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  apellido VARCHAR(255) NOT NULL,
  edad INT NOT NULL,
  fecha_creacion TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id)
)
''')

// Leemos un archivo JSON y convertimos el contenido a un objeto Groovy
def jsonSlurper = new JsonSlurper()
def personas = jsonSlurper.parse(new File('personas.json'))

// Insertamos los datos del archivo JSON en la base de datos
for (persona in personas) {
  sql.executeInsert('''
    INSERT INTO tabla (nombre, apellido, edad)
    VALUES (?, ?, ?)
  ''', [persona.nombre, persona.apellido, persona.edad])
}

// Recuperamos los datos de la tabla y los mostramos por consola
def resultado = sql.rows('SELECT * FROM tabla')
for (fila in resultado) {
  println("ID: ${fila.id} - Nombre: ${fila.nombre} - Apellido: ${fila.apellido} - Edad: ${fila.edad} - Fecha de creación: ${fila.fecha_creacion}")
}

// Cerramos la conexión a la base de datos
sql.close()
```

Este código crea una conexión a una base de datos MySQL, crea una tabla si no existe, inserta los datos de un archivo JSON en la tabla, recupera los datos de la tabla y los muestra por consola, y finalmente cierra la conexión a la base de datos.

El código está dividido en varias partes:

* **Importación de librerías:** Se importan las librerías necesarias para trabajar con Groovy, MySQL y JSON.
* **Creación de la conexión a la base de datos:** Se crea una conexión a la base de datos MySQL utilizando la clase `Sql`.
* **Creación de la tabla:** Se crea una tabla llamada `tabla` si no existe utilizando la sentencia `CREATE TABLE`.
* **Inserción de los datos del archivo JSON en la tabla:** Se lee el archivo JSON utilizando la clase `JsonSlurper` y se insertan los datos en la tabla utilizando la sentencia `INSERT INTO`.
* **Recuperación de los datos de la tabla:** Se recuperan los datos de la tabla utilizando la sentencia `SELECT` y se almacenan en una variable de tipo `List`.
* **Mostrar los datos por consola:** Se recorre la lista de datos y se muestran por consola utilizando la función `println()`.
* **Cierre de la conexión a la base de datos:** Se cierra la conexión a la base de datos utilizando el método `close()`.

Este código es un ejemplo de cómo trabajar con Groovy, MySQL y JSON para realizar operaciones básicas de base de datos.