```php
<?php

// Definición de clases

// Clase para gestionar las operaciones CRUD (Crear, Leer, Actualizar y Eliminar) sobre la base de datos
class CRUD {

    // Propiedades
    private $host = "localhost"; // Host de la base de datos
    private $user = "root"; // Usuario de la base de datos
    private $pass = ""; // Contraseña de la base de datos
    private $dbname = "test"; // Nombre de la base de datos

    // Constructor
    public function __construct() {
        // Se conecta a la base de datos
        $this->mysqli = new mysqli($this->host, $this->user, $this->pass, $this->dbname);
    }

    // Métodos

    // Crear
    public function crear($tabla, $datos) {
        // Se crea la sentencia SQL
        $sql = "INSERT INTO `$tabla` (`" . implode('`, `', array_keys($datos)) . "`) VALUES ('" . implode("', '", $datos) . "')";

        // Se ejecuta la sentencia SQL
        $this->mysqli->query($sql);

        // Se obtiene el ID del registro creado
        return $this->mysqli->insert_id;
    }

    // Leer
    public function leer($tabla, $condicion = "") {
        // Se crea la sentencia SQL
        $sql = "SELECT * FROM `$tabla`";
        if ($condicion != "") {
            $sql .= " WHERE $condicion";
        }

        // Se ejecuta la sentencia SQL
        $resultado = $this->mysqli->query($sql);

        // Se devuelven los resultados
        return $resultado;
    }

    // Actualizar
    public function actualizar($tabla, $datos, $condicion = "") {
        // Se crea la sentencia SQL
        $sql = "UPDATE `$tabla` SET ";
        foreach ($datos as $campo => $valor) {
            $sql .= "`$campo` = '$valor', ";
        }
        $sql = rtrim($sql, ', ');
        if ($condicion != "") {
            $sql .= " WHERE $condicion";
        }

        // Se ejecuta la sentencia SQL
        $this->mysqli->query($sql);
    }

    // Eliminar
    public function eliminar($tabla, $condicion = "") {
        // Se crea la sentencia SQL
        $sql = "DELETE FROM `$tabla`";
        if ($condicion != "") {
            $sql .= " WHERE $condicion";
        }

        // Se ejecuta la sentencia SQL
        $this->mysqli->query($sql);
    }

    // Cerrar la conexión
    public function cerrar() {
        // Se cierra la conexión a la base de datos
        $this->mysqli->close();
    }
}

// Ejemplo de uso

// Se crea una instancia de la clase CRUD
$crud = new CRUD();

// Se crea un nuevo registro en la tabla `usuarios`
$datos = array(
    'nombre' => 'Juan',
    'apellido' => 'García',
    'email' => 'juan.garcia@ejemplo.com',
    'password' => '123456'
);
$id = $crud->crear('usuarios', $datos);

// Se leen todos los registros de la tabla `usuarios`
$resultado = $crud->leer('usuarios');

// Se muestran los resultados
while ($fila = $resultado->fetch_assoc()) {
    echo $fila['nombre'] . ' ' . $fila['apellido'] . ' (' . $fila['email'] . ')' . '<br>';
}

// Se actualiza el registro con el ID 1
$datos = array(
    'nombre' => 'Pedro',
    'apellido' => 'Sánchez'
);
$crud->actualizar('usuarios', $datos, "id = 1");

// Se elimina el registro con el ID 2
$crud->eliminar('usuarios', "id = 2");

// Se cierra la conexión a la base de datos
$crud->cerrar();

?>
```

Explicación del código:

* Se define una clase llamada `CRUD` que proporciona métodos para realizar operaciones CRUD (Crear, Leer, Actualizar y Eliminar) sobre una base de datos.
* El constructor de la clase se utiliza para conectarse a la base de datos.
* Los métodos `crear()`, `leer()`, `actualizar()` y `eliminar()` se utilizan para realizar las operaciones CRUD correspondientes.
* El método `cerrar()` se utiliza para cerrar la conexión a la base de datos.
* Se crea una instancia de la clase `CRUD` y se utilizan los métodos para realizar operaciones CRUD sobre una tabla de usuarios.
* Los resultados de las operaciones se muestran en la salida.