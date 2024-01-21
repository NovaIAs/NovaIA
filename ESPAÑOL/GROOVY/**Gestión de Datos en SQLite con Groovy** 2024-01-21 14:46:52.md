```groovy
// Importamos la biblioteca de SQLite para trabajar con bases de datos SQLite
import groovy.sql.Sql

// Creamos una conexión a la base de datos
def db = Sql.connect('jdbc:sqlite:mi_base_de_datos.db')

// Creamos una tabla llamada "personas"
db.execute('''
CREATE TABLE IF NOT EXISTS personas (
    id INTEGER PRIMARY KEY,
    nombre TEXT NOT NULL,
    apellido TEXT NOT NULL,
    edad INTEGER NOT NULL
)''')

// Insertamos algunos datos en la tabla "personas"
db.execute('''
INSERT INTO personas (nombre, apellido, edad) VALUES
    ('Juan', 'García', 25),
    ('María', 'Pérez', 30),
    ('Pedro', 'López', 35)
''')

// Creamos una función para obtener todas las personas de la tabla "personas"
def getAllPersons() {
    db.rows('SELECT * FROM personas')
}

// Creamos una función para obtener una persona por su ID
def getPersonById(id) {
    db.firstRow('SELECT * FROM personas WHERE id = ?', [id])
}

// Creamos una función para insertar una nueva persona en la tabla "personas"
def insertPerson(nombre, apellido, edad) {
    db.execute('INSERT INTO personas (nombre, apellido, edad) VALUES (?, ?, ?)', [nombre, apellido, edad])
}

// Creamos una función para actualizar una persona en la tabla "personas"
def updatePerson(id, nombre, apellido, edad) {
    db.execute('UPDATE personas SET nombre = ?, apellido = ?, edad = ? WHERE id = ?', [nombre, apellido, edad, id])
}

// Creamos una función para eliminar una persona de la tabla "personas"
def deletePerson(id) {
    db.execute('DELETE FROM personas WHERE id = ?', [id])
}

// Cerramos la conexión a la base de datos
db.close()

// Ejemplo de uso de las funciones
def personas = getAllPersons()
println("Todas las personas:")
personas.each { persona ->
    println("ID: ${persona.id}, Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")
}

def persona = getPersonById(2)
println("Persona con ID 2:")
println("ID: ${persona.id}, Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")

insertPerson('Ana', 'Fernández', 28)
println("Nueva persona insertada:")
persona = getPersonById(4)
println("ID: ${persona.id}, Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")

updatePerson(3, 'Pedro', 'Gómez', 37)
println("Persona con ID 3 actualizada:")
persona = getPersonById(3)
println("ID: ${persona.id}, Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")

deletePerson(4)
println("Persona con ID 4 eliminada:")
personas = getAllPersons()
println("Todas las personas:")
personas.each { persona ->
    println("ID: ${persona.id}, Nombre: ${persona.nombre}, Apellido: ${persona.apellido}, Edad: ${persona.edad}")
}
```

Explicación del código:

* Importamos la biblioteca de SQLite para trabajar con bases de datos SQLite.
* Creamos una conexión a la base de datos.
* Creamos una tabla llamada "personas" en la base de datos.
* Insertamos algunos datos en la tabla "personas".
* Creamos funciones para obtener todas las personas de la tabla "personas", obtener una persona por su ID, insertar una nueva persona en la tabla "personas", actualizar una persona en la tabla "personas" y eliminar una persona de la tabla "personas".
* Cerramos la conexión a la base de datos.
* Creamos un ejemplo de uso de las funciones para mostrar todas las personas, obtener una persona por su ID, insertar una nueva persona, actualizar una persona y eliminar una persona.