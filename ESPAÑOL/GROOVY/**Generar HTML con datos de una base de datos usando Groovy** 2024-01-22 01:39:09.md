```groovy
// Importar las librerías necesarias
import groovy.sql.Sql
import groovy.text.SimpleTemplateEngine
import java.text.SimpleDateFormat

// Crear una conexión a la base de datos
def db = Sql.newInstance("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "", "org.h2.Driver")

// Crear una tabla con los campos necesarios
db.execute("""CREATE TABLE IF NOT EXISTS personas (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL,
    fecha_nacimiento DATE NOT NULL,
    PRIMARY KEY (id)
)""")

// Insertar los datos en la tabla
db.execute("INSERT INTO personas (nombre, apellido, email, fecha_nacimiento) VALUES ('Juan', 'Pérez', 'juan.perez@gmail.com', '1980-01-01')")
db.execute("INSERT INTO personas (nombre, apellido, email, fecha_nacimiento) VALUES ('María', 'García', 'maria.garcia@gmail.com', '1985-02-02')")
db.execute("INSERT INTO personas (nombre, apellido, email, fecha_nacimiento) VALUES ('Pedro', 'López', 'pedro.lopez@gmail.com', '1990-03-03')")

// Crear un template para generar el HTML
def template = new SimpleTemplateEngine().createTemplate("""
<!DOCTYPE html>
<html>
<head>
    <title>Personas</title>
</head>
<body>
    <h1>Personas</h1>
    <ul>
        <% personas.each { persona -> %>
        <li><%= persona.nombre %> <%= persona.apellido %> <%= persona.email %> <%= persona.fecha_nacimiento %></li>
        <% } %>
    </ul>
</body>
</html>
""")

// Obtener los datos de las personas de la base de datos
def personas = db.rows("SELECT * FROM personas")

// Generar el HTML con los datos de las personas
def html = template.make([personas: personas]).toString()

// Guardar el HTML en un archivo
new File("personas.html").write(html)

// Imprimir el HTML en la consola
println html
```

Este código crea una base de datos con la tabla "personas" y la llena con algunos datos. Luego, crea un template para generar un archivo HTML con los datos de las personas y finalmente genera el HTML y lo guarda en un archivo. También imprime el HTML en la consola para que se pueda ver.

Este código es complejo porque utiliza varias librerías y técnicas de programación, como la creación de una base de datos, la inserción de datos, la creación de plantillas, la generación de HTML y la impresión de datos en la consola. También utiliza el lenguaje de programación Groovy, que es un lenguaje de programación dinámico y orientado a objetos.