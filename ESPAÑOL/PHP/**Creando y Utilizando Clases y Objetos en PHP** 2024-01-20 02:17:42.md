```php
// Definición de las clases
class Persona {
  private $nombre;
  private $email;

  public function __construct($nombre, $email) {
    $this->nombre = $nombre;
    $this->email = $email;
  }

  public function getNombre() {
    return $this->nombre;
  }

  public function getEmail() {
    return $this->email;
  }
}

class Empleado extends Persona {
  private $salario;

  public function __construct($nombre, $email, $salario) {
    parent::__construct($nombre, $email);
    $this->salario = $salario;
  }

  public function getSalario() {
    return $this->salario;
  }
}

class Empresa {
  private $nombre;
  private $empleados = [];

  public function __construct($nombre) {
    $this->nombre = $nombre;
  }

  public function getNombre() {
    return $this->nombre;
  }

  public function agregarEmpleado(Empleado $empleado) {
    $this->empleados[] = $empleado;
  }

  public function getEmpleados() {
    return $this->empleados;
  }
}

// Creación de los objetos
$empresa = new Empresa("Acme Corporation");

$empleado1 = new Empleado("Juan Pérez", "juan.perez@acme.com", 1000);
$empleado2 = new Empleado("María Gómez", "maria.gomez@acme.com", 1200);
$empleado3 = new Empleado("Pedro Rodríguez", "pedro.rodriguez@acme.com", 1500);

// Añadir los empleados a la empresa
$empresa->agregarEmpleado($empleado1);
$empresa->agregarEmpleado($empleado2);
$empresa->agregarEmpleado($empleado3);

// Obtener los empleados de la empresa
$empleados = $empresa->getEmpleados();

// Mostrar los datos de los empleados
foreach ($empleados as $empleado) {
  echo "Nombre: " . $empleado->getNombre() . "<br>";
  echo "Email: " . $empleado->getEmail() . "<br>";
  echo "Salario: " . $empleado->getSalario() . "<br>";
  echo "<br>";
}
```

Explicación del código:

* Se definen las clases `Persona`, `Empleado` y `Empresa`.
* La clase `Persona` representa a una persona genérica, con los atributos `nombre` y `email`.
* La clase `Empleado` hereda de la clase `Persona` y añade el atributo `salario`.
* La clase `Empresa` representa a una empresa, con el atributo `nombre` y una lista de empleados.
* Se crean los objetos de las clases `Empresa` y `Empleado`.
* Se añaden los empleados a la empresa.
* Se obtienen los empleados de la empresa.
* Se muestran los datos de los empleados.

Este código es un ejemplo de cómo se pueden crear objetos y clases en PHP, y cómo se pueden utilizar para representar datos y realizar operaciones sobre ellos.