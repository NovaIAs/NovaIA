```perl
use strict;
use warnings;

# Crear un hash para almacenar los datos de los empleados
my %empleados = (
    '001' => {
        nombre   => 'Juan Pérez',
        puesto   => 'Gerente',
        salario  => '10000'
    },
    '002' => {
        nombre   => 'María López',
        puesto   => 'Contadora',
        salario  => '8000'
    },
    '003' => {
        nombre   => 'Pedro González',
        puesto   => 'Programador',
        salario  => '7000'
    }
);

# Solicitar al usuario que introduzca el código de empleado
print "Introduzca el código de empleado: ";
my $codigo_empleado = <STDIN>;
chomp $codigo_empleado;

# Comprobar si el código de empleado existe en el hash
if (exists $empleados{$codigo_empleado}) {
    # Obtener los datos del empleado
    my $nombre = $empleados{$codigo_empleado}->{nombre};
    my $puesto = $empleados{$codigo_empleado}->{puesto};
    my $salario = $empleados{$codigo_empleado}->{salario};

    # Mostrar los datos del empleado
    print "Nombre: $nombre\n";
    print "Puesto: $puesto\n";
    print "Salario: $salario\n";
} else {
    # Mostrar un mensaje de error
    print "El código de empleado no existe.\n";
}
```

Explicación del código:

* Se crea un hash llamado `%empleados` para almacenar los datos de los empleados. Cada clave del hash es el código de empleado y cada valor es un hash con los datos del empleado, como el nombre, el puesto y el salario.
* Se solicita al usuario que introduzca el código de empleado.
* Se comprueba si el código de empleado existe en el hash.
* Si el código de empleado existe, se obtienen los datos del empleado y se muestran en la pantalla.
* Si el código de empleado no existe, se muestra un mensaje de error.