```php
// Definir una clase de conexión a la base de datos.
class Database {
    private $host; // Nombre del host de la base de datos.
    private $user; // Nombre de usuario de la base de datos.
    private $pass; // Contraseña del usuario de la base de datos.
    private $name; // Nombre de la base de datos.
    private $conn; // Conexión a la base de datos.

    // Constructor de la clase.
    public function __construct(string $host, string $user, string $pass, string $name) {
        $this->host = $host;
        $this->user = $user;
        $this->pass = $pass;
        $this->name = $name;
    }

    // Método para establecer la conexión a la base de datos.
    public function connect(): bool {
        $this->conn = new mysqli($this->host, $this->user, $this->pass, $this->name);
        return $this->conn->connect_error ? false : true;
    }

    // Método para ejecutar una consulta a la base de datos.
    public function query(string $sql): mysqli_result {
        return $this->conn->query($sql);
    }

    // Método para obtener el último error de la conexión a la base de datos.
    public function error(): string {
        return $this->conn->error;
    }

    // Método para cerrar la conexión a la base de datos.
    public function close(): bool {
        return $this->conn->close();
    }
}

// Definir una clase de usuario.
class User {
    private $id; // Identificador del usuario.
    private $name; // Nombre del usuario.
    private $email; // Correo electrónico del usuario.
    private $password; // Contraseña del usuario.

    // Constructor de la clase.
    public function __construct(int $id, string $name, string $email, string $password) {
        $this->id = $id;
        $this->name = $name;
        $this->email = $email;
        $this->password = $password;
    }

    // Métodos para obtener los datos del usuario.
    public function getId(): int {
        return $this->id;
    }

    public function getName(): string {
        return $this->name;
    }

    public function getEmail(): string {
        return $this->email;
    }

    public function getPassword(): string {
        return $this->password;
    }

    // Método para actualizar los datos del usuario.
    public function update(string $name, string $email, string $password): bool {
        $sql = "UPDATE users SET name='$name', email='$email', password='$password' WHERE id=$this->id";
        $result = $this->db->query($sql);
        return $result ? true : false;
    }

    // Método para eliminar el usuario.
    public function delete(): bool {
        $sql = "DELETE FROM users WHERE id=$this->id";
        $result = $this->db->query($sql);
        return $result ? true : false;
    }
}

// Instanciar una conexión a la base de datos.
$db = new Database('localhost', 'root', '', 'users');

// Comprobar si la conexión se ha establecido correctamente.
if ($db->connect()) {
    // Crear un nuevo usuario.
    $user = new User(0, 'Juan', 'juan@gmail.com', '123456');

    // Insertar el nuevo usuario en la base de datos.
    $sql = "INSERT INTO users (name, email, password) VALUES ('$user->getName()', '$user->getEmail()', '$user->getPassword()')";
    $result = $db->query($sql);

    // Comprobar si el nuevo usuario se ha insertado correctamente.
    if ($result) {
        // Obtener el último identificador generado por la base de datos.
        $user->setId($db->insert_id);

        // Mostrar los datos del nuevo usuario.
        echo "El usuario $user->getName() con el correo electrónico $user->getEmail() y la contraseña $user->getPassword() se ha creado correctamente.";
    } else {
        // Mostrar un mensaje de error.
        echo "No se ha podido crear el usuario.";
    }

    // Cerrar la conexión a la base de datos.
    $db->close();
} else {
    // Mostrar un mensaje de error.
    echo "No se ha podido establecer la conexión a la base de datos.";
}
```

Explicación del código:

* La primera parte del código define una clase de conexión a la base de datos. Esta clase permite establecer una conexión a una base de datos MySQL y ejecutar consultas sobre ella.
* La segunda parte del código define una clase de usuario. Esta clase permite crear, modificar y eliminar usuarios en una base de datos MySQL.
* La tercera parte del código instancia una conexión a la base de datos y comprueba si se ha establecido correctamente.
* La cuarta parte del código crea un nuevo usuario y lo inserta en la base de datos.
* La quinta parte del código comprueba si el nuevo usuario se ha insertado correctamente y muestra un mensaje en consecuencia.
* La sexta parte del código cierra la conexión a la base de datos.