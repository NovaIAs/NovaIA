```perl
use strict;
use warnings;

mi_paquete: {
    # Definición de una subrutina local
    sub saludar {
        my $nombre = shift;
        print "Hola, $nombre!\n";
    }

    # Definición de una variable de paquete local
    my $secreto = "Soy un secreto";
}

# Uso de la subrutina local
mi_paquete::saludar("Juan");

# Acceso a la variable de paquete local
print "El secreto es: $mi_paquete::secreto\n";

mi_segundo_paquete: {
    # Definición de una clase local
    package MiClase;

    sub new {
        my ($class, %args) = @_;
        bless {
            nombre => $args{nombre},
            edad   => $args{edad},
        }, $class;
    }

    sub saludar {
        my $self = shift;
        print "Hola, mi nombre es $self->{nombre} y tengo $self->{edad} años.\n";
    }
}

# Creación de una instancia de la clase local
my $persona = MiClase->new(nombre => "María", edad => 25);

# Uso del método local
$persona->saludar();

# Acceso a los atributos de la clase local
print "El nombre de la persona es: $persona->{nombre}\n";
print "La edad de la persona es: $persona->{edad}\n";
```

Explicación del código:

1. **Definición de Paquetes:** Se definen dos paquetes, `mi_paquete` y `mi_segundo_paquete`, cada uno con su propio espacio de nombres.
2. **Definición de Subrutinas Locales:** Se definen dos subrutinas locales en cada paquete: `saludar` en `mi_paquete` y `saludar` en `mi_segundo_paquete`. Estas subrutinas son accesibles sólo dentro del paquete en el que están definidas.
3. **Definición de Variables de Paquete Locales:** Se define una variable de paquete local en `mi_paquete` llamada `secreto`. Esta variable es accesible sólo dentro del paquete `mi_paquete`.
4. **Uso de Subrutinas Locales:** Se llama a la subrutina `saludar` en `mi_paquete` para saludar a un individuo llamado "Juan".
5. **Acceso a Variables de Paquete Locales:** Se imprime el valor de la variable de paquete local `secreto` definida en `mi_paquete`.
6. **Definición de Clases Locales:** Se define una clase local `MiClase` en el paquete `mi_segundo_paquete`. Esta clase puede ser utilizada para crear objetos que representen personas.
7. **Creación de Instancias de Clases Locales:** Se crea una instancia de la clase `MiClase` llamada `persona`, con el nombre "María" y la edad 25.
8. **Uso de Métodos Locales:** Se llama al método `saludar` del objeto `persona` para que salude.
9. **Acceso a Atributos de Clases Locales:** Se imprime el valor de los atributos `nombre` y `edad` del objeto `persona`.