```php
<?php
// Definición de la clase 'Persona' para representar los datos de una persona
class Persona {
    private $nombre;
    private $apellido;
    private $edad;

    // Constructor de la clase 'Persona'
    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

    // Método para obtener el nombre de la persona
    public function getNombre() {
        return $this->nombre;
    }

    // Método para obtener el apellido de la persona
    public function getApellido() {
        return $this->apellido;
    }

    // Método para obtener la edad de la persona
    public function getEdad() {
        return $this->edad;
    }

    // Método para imprimir los datos de la persona
    public function imprimirDatos() {
        echo "Nombre: $this->nombre" . PHP_EOL;
        echo "Apellido: $this->apellido" . PHP_EOL;
        echo "Edad: $this->edad" . PHP_EOL;
    }
}

// Definición de la clase 'Empleado' que hereda de la clase 'Persona'
class Empleado extends Persona {
    private $salario;
    private $departamento;

    // Constructor de la clase 'Empleado'
    public function __construct($nombre, $apellido, $edad, $salario, $departamento) {
        parent::__construct($nombre, $apellido, $edad);
        $this->salario = $salario;
        $this->departamento = $departamento;
    }

    // Método para obtener el salario del empleado
    public function getSalario() {
        return $this->salario;
    }

    // Método para obtener el departamento del empleado
    public function getDepartamento() {
        return $this->departamento;
    }

    // Método para imprimir los datos del empleado
    public function imprimirDatos() {
        parent::imprimirDatos();
        echo "Salario: $this->salario" . PHP_EOL;
        echo "Departamento: $this->departamento" . PHP_EOL;
    }
}

// Definición de la clase 'Cliente' que hereda de la clase 'Persona'
class Cliente extends Persona {
    private $direccion;
    private $telefono;

    // Constructor de la clase 'Cliente'
    public function __construct($nombre, $apellido, $edad, $direccion, $telefono) {
        parent::__construct($nombre, $apellido, $edad);
        $this->direccion = $direccion;
        $this->telefono = $telefono;
    }

    // Método para obtener la dirección del cliente
    public function getDireccion() {
        return $this->direccion;
    }

    // Método para obtener el teléfono del cliente
    public function getTelefono() {
        return $this->telefono;
    }

    // Método para imprimir los datos del cliente
    public function imprimirDatos() {
        parent::imprimirDatos();
        echo "Dirección: $this->direccion" . PHP_EOL;
        echo "Teléfono: $this->telefono" . PHP_EOL;
    }
}

// Creación de objetos 'Persona', 'Empleado' y 'Cliente'
$persona = new Persona('Juan', 'García', 30);
$empleado = new Empleado('María', 'Pérez', 25, 2000, 'Ventas');
$cliente = new Cliente('Pedro', 'López', 40, 'Calle Mayor 123', '911234567');

// Impresión de los datos de cada objeto
echo "Datos de la persona:" . PHP_EOL;
$persona->imprimirDatos();
echo PHP_EOL;

echo "Datos del empleado:" . PHP_EOL;
$empleado->imprimirDatos();
echo PHP_EOL;

echo "Datos del cliente:" . PHP_EOL;
$cliente->imprimirDatos();
```

**Explicación del código:**

Este código define tres clases: 'Persona', 'Empleado' y 'Cliente'. La clase 'Persona' representa los datos básicos de una persona, como su nombre, apellido y edad. Las clases 'Empleado' y 'Cliente' heredan de la clase 'Persona' y añaden campos y métodos específicos para cada tipo de persona.

El código crea objetos de cada clase y llama a los métodos 'imprimirDatos()' para imprimir los datos de cada objeto. Esto demuestra cómo las clases y la herencia pueden utilizarse para crear objetos con diferentes propiedades y comportamientos.