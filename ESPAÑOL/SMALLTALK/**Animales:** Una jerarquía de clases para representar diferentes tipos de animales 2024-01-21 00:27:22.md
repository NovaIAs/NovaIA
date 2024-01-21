```smalltalk

;; Clase Animal
Animal superclass: Object
instvars: nombre edad tipo

Animal new nombre: 'Perro' edad: 5 tipo: 'Mamífero'.

Animal nombre -> 'Perro'.
Animal edad -> 5.
Animal tipo -> 'Mamífero'.

Animal hablar -> 'Guau!'.

;; Clase Perro
Perro superclass: Animal

Perro new nombre: 'Toby' edad: 3 tipo: 'Mamífero' raza: 'Pastor alemán'.

Perro raza -> 'Pastor alemán'.

Perro ladrar -> 'Woof!'.

;; Clase Gato
Gato superclass: Animal

Gato new nombre: 'Michi' edad: 2 tipo: 'Mamífero' raza: 'Siamés'.

Gato raza -> 'Siamés'.

Gato maullar -> 'Miau!'.

;; Clase Pájaro
Pájaro superclass: Animal

Pájaro new nombre: 'Loro' edad: 1 tipo: 'Ave' raza: 'Guacamayo'.

Pájaro raza -> 'Guacamayo'.

Pájaro cantar -> 'Pío!'.

;; Clase Pez
Pez superclass: Animal

Pez new nombre: 'Nemo' edad: 1 tipo: 'Pez' raza: 'Pez payaso'.

Pez raza -> 'Pez payaso'.

Pez nadar -> 'Glub!'.

```

**Explicación:**

* La clase `Animal` es la clase base para todos los animales. Tiene tres variables de instancia: `nombre`, `edad` y `tipo`.
* La clase `Perro` es una subclase de `Animal`. Tiene una variable de instancia adicional: `raza`.
* La clase `Gato` es una subclase de `Animal`. Tiene una variable de instancia adicional: `raza`.
* La clase `Pájaro` es una subclase de `Animal`. Tiene una variable de instancia adicional: `raza`.
* La clase `Pez` es una subclase de `Animal`. Tiene una variable de instancia adicional: `raza`.
* Cada clase tiene un constructor que inicializa las variables de instancia.
* Cada clase tiene un método `nombre` que devuelve el nombre del animal.
* Cada clase tiene un método `edad` que devuelve la edad del animal.
* Cada clase tiene un método `tipo` que devuelve el tipo del animal.
* La clase `Perro` tiene un método `raza` que devuelve la raza del perro.
* La clase `Gato` tiene un método `raza` que devuelve la raza del gato.
* La clase `Pájaro` tiene un método `raza` que devuelve la raza del pájaro.
* La clase `Pez` tiene un método `raza` que devuelve la raza del pez.
* Cada clase tiene un método `hablar` que devuelve el sonido que hace el animal.
* El método `hablar` de la clase `Perro` devuelve 'Guau!'.
* El método `hablar` de la clase `Gato` devuelve 'Miau!'.
* El método `hablar` de la clase `Pájaro` devuelve 'Pío!'.
* El método `hablar` de la clase `Pez` devuelve 'Glub!'.