```php
// Definición de la clase Persona con propiedades y métodos básicos
class Persona {
    public $nombre;
    public $edad;

    public function saludar() {
        echo "Hola, mi nombre es $this->nombre y tengo $this->edad años.";
    }
}

// Creación y utilización de dos objetos de la clase Persona
$persona1 = new Persona();
$persona1->nombre = "Juan";
$persona1->edad = 20;
$persona1->saludar();

$persona2 = new Persona();
$persona2->nombre = "María";
$persona2->edad = 25;
$persona2->saludar();

// Definición de una interfaz Vehículo con métodos comunes a diferentes vehículos
interface Vehiculo {
    public function acelerar();
    public function frenar();
}

// Creación de clases Coche y Moto que implementan la interfaz Vehículo
class Coche implements Vehiculo {
    public function acelerar() {
        echo "El coche acelera.";
    }

    public function frenar() {
        echo "El coche frena.";
    }
}

class Moto implements Vehiculo {
    public function acelerar() {
        echo "La moto acelera.";
    }

    public function frenar() {
        echo "La moto frena.";
    }
}

// Creación y utilización de objetos de las clases Coche y Moto
$coche = new Coche();
$moto = new Moto();

$coche->acelerar();
$coche->frenar();

$moto->acelerar();
$moto->frenar();

// Definición de una clase abstracta Animal con propiedades y métodos comunes a diferentes animales
abstract class Animal {
    public $nombre;
    public $especie;

    public function comer() {
        echo "El animal come.";
    }

    public function dormir() {
        echo "El animal duerme.";
    }

    // Método abstracto que debe ser implementado en las clases derivadas
    abstract public function hacerRuido();
}

// Creación de clases Perro y Gato que extienden de la clase abstracta Animal
class Perro extends Animal {
    public function hacerRuido() {
        echo "El perro ladra.";
    }
}

class Gato extends Animal {
    public function hacerRuido() {
        echo "El gato maúlla.";
    }
}

// Creación y utilización de objetos de las clases Perro y Gato
$perro = new Perro();
$gato = new Gato();

$perro->comer();
$perro->dormir();
$perro->hacerRuido();

$gato->comer();
$gato->dormir();
$gato->hacerRuido();
```

Explicación del código:

* Definimos una clase Persona con las propiedades `nombre` y `edad`, y un método `saludar()`.
* Creamos dos objetos de la clase Persona, `persona1` y `persona2`, y les asignamos valores a sus propiedades.
* Utilizamos el método `saludar()` de los objetos para imprimir sus nombres y edades.
* Definimos una interfaz Vehículo con dos métodos, `acelerar()` y `frenar()`.
* Creamos dos clases, `Coche` y `Moto`, que implementan la interfaz Vehículo y proporcionan implementaciones específicas para los métodos `acelerar()` y `frenar()`.
* Creamos objetos de las clases `Coche` y `Moto`, y utilizamos sus métodos para simular la aceleración y el frenado de un coche y una moto.
* Definimos una clase abstracta `Animal` con las propiedades `nombre` y `especie`, y dos métodos, `comer()` y `dormir()`.
* Definimos un método abstracto `hacerRuido()` que debe ser implementado en las clases derivadas.
* Creamos dos clases, `Perro` y `Gato`, que extienden de la clase abstracta `Animal` y proporcionan implementaciones específicas para el método `hacerRuido()`.
* Creamos objetos de las clases `Perro` y `Gato`, y utilizamos sus métodos para simular el comportamiento de un perro y un gato.