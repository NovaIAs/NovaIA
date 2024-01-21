```smalltalk
# Módulo principal
Módulo {
    # Clase principal que encapsula todo el código
    Clase {

        # Constructor
        constructor {
            # Inicializaciones
            super();

            # Variable de ejemplo
            variable := 0;

            # Crea una instancia de una clase secundaria
            subClase := SubClase new();
        }

        # Método principal
        métodoPrincipal {
            # Imprime un mensaje
            Transcript show: '¡Hola, mundo!';

            # Llama al método de la clase secundaria
            subClase métodoSecundario();

            # Imprime el valor de la variable de ejemplo
            Transcript show: 'El valor de la variable es: ', variable asString();
        }

        # Método secundario
        métodoSecundario {
            # Imprime un mensaje
            Transcript show: '¡Hola, desde el método secundario!';
        }
    }

    # Clase secundaria
    Clase SubClase {

        # Constructor
        constructor {
            # Inicializaciones
            super();

            # Variable de ejemplo
            subVariable := 0;
        }

        # Método secundario
        métodoSecundario {
            # Imprime un mensaje
            Transcript show: '¡Hola, desde la clase secundaria!';

            # Imprime el valor de la variable de ejemplo
            Transcript show: 'El valor de la variable es: ', subVariable asString();
        }
    }
}
```

**Explicación del código:**

* El código anterior está escrito en Smalltalk, un lenguaje de programación orientado a objetos.
* El código crea un módulo, que es una unidad organizativa en Smalltalk.
* Dentro del módulo, se definen dos clases: `Clase` y `SubClase`.
* La clase `Clase` es la clase principal y contiene el método principal, que se llama `métodoPrincipal()`.
* El método `métodoPrincipal()` imprime un mensaje y llama al método `métodoSecundario()` de la clase `SubClase`.
* La clase `SubClase` es una clase secundaria que contiene el método `métodoSecundario()`.
* El método `métodoSecundario()` imprime un mensaje y llama al constructor de la clase.
* El constructor de la clase `Clase` inicializa la variable de ejemplo `variable`.
* El constructor de la clase `SubClase` inicializa la variable de ejemplo `subVariable`.
* El código finalmente llama al método `métodoPrincipal()` de la clase `Clase`.