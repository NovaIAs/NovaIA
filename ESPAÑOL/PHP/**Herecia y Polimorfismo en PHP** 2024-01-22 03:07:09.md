```php
<?php
// Definición de la clase Persona con atributos públicos y métodos privados.
class Persona {
    public $nombre;
    public $apellido;

    private function saludar() {
        echo "Hola, mi nombre es $this->nombre $this->apellido.\n";
    }

    public function presentarse() {
        $this->saludar();
        echo "Encantado de conocerle.\n";
    }
}

// Definición de la clase Estudiante que hereda de la clase Persona y agrega un atributo adicional.
class Estudiante extends Persona {
    public $universidad;

    public function presentarse() {
        parent::presentarse();
        echo "Estoy estudiando en la universidad $this->universidad.\n";
    }
}

// Definición de la clase Empleado que hereda de la clase Persona y agrega un atributo adicional.
class Empleado extends Persona {
    public $empresa;

    public function presentarse() {
        parent::presentarse();
        echo "Trabajo en la empresa $this->empresa.\n";
    }
}

// Creación de objetos de las clases Persona, Estudiante y Empleado.
$persona1 = new Persona();
$persona1->nombre = "Juan";
$persona1->apellido = "García";

$estudiante1 = new Estudiante();
$estudiante1->nombre = "María";
$estudiante1->apellido = "Pérez";
$estudiante1->universidad = "Universidad Nacional Autónoma de México";

$empleado1 = new Empleado();
$empleado1->nombre = "Pedro";
$empleado1->apellido = "López";
$empleado1->empresa = "Google";

// Llamada al método presentarse() de cada objeto.
$persona1->presentarse();
$estudiante1->presentarse();
$empleado1->presentarse();
?>
```

Explicación del código:

1. Primero, definimos la clase `Persona` con atributos públicos (`nombre` y `apellido`) y un método privado (`saludar()`). El método privado solo puede ser accedido dentro de la clase, mientras que los atributos públicos pueden ser accedidos desde cualquier lugar.

2. A continuación, definimos las clases `Estudiante` y `Empleado` que heredan de la clase `Persona`. Estas clases añaden un atributo adicional (`universidad` para `Estudiante` y `empresa` para `Empleado`) y sobreescriben el método `presentarse()` para incluir información adicional.

3. Luego, creamos objetos de las clases `Persona`, `Estudiante` y `Empleado`. Asignamos valores a los atributos de los objetos y llamamos al método `presentarse()` de cada objeto.

4. El resultado es que se imprimen los siguientes mensajes en la consola:

```
Hola, mi nombre es Juan García.
Encantado de conocerle.
Estoy estudiando en la universidad Universidad Nacional Autónoma de México.
Hola, mi nombre es María Pérez.
Encantado de conocerle.
Trabajo en la empresa Google.
```