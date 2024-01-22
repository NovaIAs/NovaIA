```perl
# Crea un objeto de SQLite3
use DBIx::SQLite3;

# Conéctate a la base de datos o créala si no existe
$dbh = DBIx::SQLite3->connect("mi_base_de_datos.sqlite3");

# Crea una tabla llamada "usuarios"
$dbh->do(
  "CREATE TABLE usuarios (
    id INTEGER PRIMARY KEY,
    nombre VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    contraseña VARCHAR(255) NOT NULL
  )"
);

# Inserta algunos datos en la tabla "usuarios"
$dbh->do(
  "INSERT INTO usuarios (nombre, email, contraseña) VALUES (
    'Juan Pérez', 'juan.perez@ejemplo.com', 'contraseña123'
  )"
);
$dbh->do(
  "INSERT INTO usuarios (nombre, email, contraseña) VALUES (
    'María López', 'maria.lopez@ejemplo.com', 'contraseña456'
  )"
);
$dbh->do(
  "INSERT INTO usuarios (nombre, email, contraseña) VALUES (
    'Pedro García', 'pedro.garcia@ejemplo.com', 'contraseña789'
  )"
);

# Crea una tabla llamada "tareas"
$dbh->do(
  "CREATE TABLE tareas (
    id INTEGER PRIMARY KEY,
    nombre VARCHAR(255) NOT NULL,
    descripcion TEXT,
    fecha_creación DATETIME,
    fecha_vencimiento DATETIME,
    estado VARCHAR(255),
    usuario_id INTEGER REFERENCES usuarios(id)
  )"
);

# Inserta algunos datos en la tabla "tareas"
$dbh->do(
  "INSERT INTO tareas (nombre, descripcion, fecha_creación, fecha_vencimiento, estado, usuario_id) VALUES (
    'Comprar pan', 'Comprar una barra de pan integral', NOW(), DATE_ADD(NOW(), INTERVAL 1 DAY), 'Pendiente', 1
  )"
);
$dbh->do(
  "INSERT INTO tareas (nombre, descripcion, fecha_creación, fecha_vencimiento, estado, usuario_id) VALUES (
    'Hacer la cama', 'Hacer la cama de la habitación principal', NOW(), DATE_ADD(NOW(), INTERVAL 1 DAY), 'Pendiente', 2
  )"
);
$dbh->do(
  "INSERT INTO tareas (nombre, descripcion, fecha_creación, fecha_vencimiento, estado, usuario_id) VALUES (
    'Limpiar el baño', 'Limpiar el baño principal', NOW(), DATE_ADD(NOW(), INTERVAL 2 DAYS), 'En progreso', 3
  )"
);

# Cierra la conexión a la base de datos
$dbh->disconnect();

# Muestra los datos de la tabla "usuarios"
print "Usuarios:\n";
$sth = $dbh->prepare("SELECT * FROM usuarios");
$sth->execute();
while (my $row = $sth->fetchrow_hashref) {
  print "  - $row->{nombre} ($row->{email})\n";
}
$sth->finish();

# Muestra los datos de la tabla "tareas"
print "Tareas:\n";
$sth = $dbh->prepare("SELECT * FROM tareas");
$sth->execute();
while (my $row = $sth->fetchrow_hashref) {
  print "  - $row->{nombre} ($row->{estado}) asignada a $row->{usuario_id}\n";
}
$sth->finish();
```

Explicación del código:

* En primer lugar, se importa el módulo DBIx::SQLite3, que se utiliza para conectarse a bases de datos SQLite3.

* A continuación, se crea un objeto de SQLite3 llamado $dbh, que se utiliza para conectarse a la base de datos. La conexión se establece a través del método connect(), que recibe como argumento la ruta a la base de datos. En este caso, la base de datos se llama "mi_base_de_datos.sqlite3" y se encuentra en el mismo directorio que el script.

* Una vez establecida la conexión, se crean las tablas "usuarios" y "tareas" mediante el método do(). El método do() recibe como argumento una cadena de caracteres que contiene la sentencia SQL para crear la tabla.

* A continuación, se insertan algunos datos en las tablas "usuarios" y "tareas" mediante el mismo método do().

* Por último, se cierra la conexión a la base de datos mediante el método disconnect().

* Finalmente, se muestran los datos de las tablas "usuarios" y "tareas" utilizando el método fetchrow_hashref(), que devuelve una referencia a un hash con los datos de la fila actual.

Este código es un ejemplo complejo de cómo utilizar el módulo DBIx::SQLite3 para conectarse a una base de datos SQLite3, crear tablas, insertar datos y mostrar los datos de las tablas.