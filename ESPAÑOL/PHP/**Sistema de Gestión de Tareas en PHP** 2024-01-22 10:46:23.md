```php
// Este código implementa un sistema de gestión de tareas en PHP.
// Se utiliza una base de datos MySQL para almacenar las tareas.

// Conexión a la base de datos
$servername = "localhost";
$username = "root";
$password = "";
$dbname = "tareas";

// Crear conexión
$conn = new mysqli($servername, $username, $password, $dbname);

// Comprobar conexión
if ($conn->connect_error) {
  die("Error de conexión: " . $conn->connect_error);
}

// Definir la tabla de tareas
$sql = "CREATE TABLE IF NOT EXISTS tareas (
  id INT AUTO_INCREMENT PRIMARY KEY,
  titulo VARCHAR(255) NOT NULL,
  descripcion TEXT,
  fecha_creacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  fecha_vencimiento DATE,
  prioridad ENUM('alta', 'media', 'baja') DEFAULT 'media',
  estado ENUM('pendiente', 'en curso', 'completada') DEFAULT 'pendiente'
)";

// Crear la tabla
if ($conn->query($sql) === TRUE) {
  echo "Tabla tareas creada correctamente.";
} else {
  echo "Error al crear la tabla tareas: " . $conn->error;
}

// Función para obtener todas las tareas
function getAllTasks($conn) {
  $sql = "SELECT * FROM tareas";
  $result = $conn->query($sql);

  if ($result->num_rows > 0) {
    $tasks = array();

    while ($row = $result->fetch_assoc()) {
      $tasks[] = $row;
    }

    return $tasks;
  } else {
    return array();
  }
}

// Función para obtener una tarea por su ID
function getTaskById($conn, $id) {
  $sql = "SELECT * FROM tareas WHERE id = $id";
  $result = $conn->query($sql);

  if ($result->num_rows > 0) {
    return $result->fetch_assoc();
  } else {
    return null;
  }
}

// Función para crear una nueva tarea
function createTask($conn, $titulo, $descripcion, $fecha_vencimiento, $prioridad) {
  $sql = "INSERT INTO tareas (titulo, descripcion, fecha_vencimiento, prioridad)
    VALUES ('$titulo', '$descripcion', '$fecha_vencimiento', '$prioridad')";

  if ($conn->query($sql) === TRUE) {
    return $conn->insert_id;
  } else {
    return false;
  }
}

// Función para actualizar una tarea
function updateTask($conn, $id, $titulo, $descripcion, $fecha_vencimiento, $prioridad, $estado) {
  $sql = "UPDATE tareas SET
    titulo = '$titulo',
    descripcion = '$descripcion',
    fecha_vencimiento = '$fecha_vencimiento',
    prioridad = '$prioridad',
    estado = '$estado'
    WHERE id = $id";

  if ($conn->query($sql) === TRUE) {
    return true;
  } else {
    return false;
  }
}

// Función para eliminar una tarea
function deleteTask($conn, $id) {
  $sql = "DELETE FROM tareas WHERE id = $id";

  if ($conn->query($sql) === TRUE) {
    return true;
  } else {
    return false;
  }
}

// Cerrar la conexión
$conn->close();
```

Este código crea una tabla de tareas en una base de datos MySQL y define las funciones necesarias para gestionar las tareas, incluyendo la obtención de todas las tareas, la obtención de una tarea por su ID, la creación de una nueva tarea, la actualización de una tarea y la eliminación de una tarea.