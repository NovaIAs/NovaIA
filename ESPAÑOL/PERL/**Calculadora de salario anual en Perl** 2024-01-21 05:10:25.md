```perl
use strict;
use warnings;

# Definimos las variables que vamos a utilizar
my $nombre = "Juan";
my $edad = 20;
my $sueldo = 1000;

# Creamos una función para imprimir un mensaje
sub imprimir_mensaje {
    my $mensaje = shift;
    print "$mensaje\n";
}

# Creamos una función para calcular el salario anual
sub calcular_salario_anual {
    my $sueldo_mensual = shift;
    my $salario_anual = $sueldo_mensual * 12;
    return $salario_anual;
}

# Imprimimos un mensaje de bienvenida
imprimir_mensaje("Bienvenido al programa");

# Imprimimos el nombre del usuario
imprimir_mensaje("Tu nombre es $nombre");

# Imprimimos la edad del usuario
imprimir_mensaje("Tu edad es $edad");

# Imprimimos el sueldo del usuario
imprimir_mensaje("Tu sueldo es $sueldo");

# Calculamos el salario anual del usuario
my $salario_anual = calcular_salario_anual($sueldo);

# Imprimimos el salario anual del usuario
imprimir_mensaje("Tu salario anual es $salario_anual");
```

Explicación del código:

* Creamos las variables `$nombre`, `$edad` y `$sueldo` para almacenar los datos del usuario.
* Creamos una función `imprimir_mensaje` para imprimir un mensaje en la pantalla.
* Creamos una función `calcular_salario_anual` para calcular el salario anual del usuario.
* Imprimimos un mensaje de bienvenida en la pantalla.
* Imprimimos el nombre del usuario en la pantalla.
* Imprimimos la edad del usuario en la pantalla.
* Imprimimos el sueldo del usuario en la pantalla.
* Calculamos el salario anual del usuario.
* Imprimimos el salario anual del usuario en la pantalla.