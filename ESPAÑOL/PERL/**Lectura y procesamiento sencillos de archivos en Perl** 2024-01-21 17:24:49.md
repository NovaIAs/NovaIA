```perl
#! /usr/bin/perl -w

# Declaración de variables
my $nombre_archivo = 'datos.txt';
my $contenido_archivo;
my @lineas_archivo;
my %datos_archivo;

# Abrir el archivo de texto para lectura
open(ARCHIVO, '<', $nombre_archivo) or die "No se pudo abrir el archivo $nombre_archivo: $!\n";

# Leer el contenido del archivo
$contenido_archivo = do { local $/; <ARCHIVO> };

# Cerrar el archivo de texto
close(ARCHIVO);

# Dividir el contenido del archivo en líneas
@lineas_archivo = split("\n", $contenido_archivo);

# Iterar sobre las líneas del archivo
foreach my $linea (@lineas_archivo) {

    # Eliminar los espacios en blanco al principio y al final de la línea
    $linea =~ s/^\s+|\s+$//g;

    # Comprobar si la línea está vacía
    if ($linea eq "") {
        next;
    }

    # Dividir la línea en campos
    my @campos = split(", ", $linea);

    # Almacenar los datos en un hash
    $datos_archivo{$campos[0]} = $campos[1];
}

# Mostrar los datos del archivo
foreach my $llave (keys %datos_archivo) {
    print "$llave: $datos_archivo{$llave}\n";
}
```

Explicación del código:

* **Declaración de variables:** Se declaran las variables que se utilizarán en el código.
* **Abrir el archivo de texto:** Se abre el archivo de texto especificado en la variable `$nombre_archivo` para lectura.
* **Leer el contenido del archivo:** Se lee el contenido del archivo de texto y se almacena en la variable `$contenido_archivo`.
* **Cerrar el archivo de texto:** Se cierra el archivo de texto.
* **Dividir el contenido del archivo en líneas:** Se divide el contenido del archivo de texto en líneas y se almacena en el array `@lineas_archivo`.
* **Iterar sobre las líneas del archivo:** Se itera sobre las líneas del archivo de texto.
* **Eliminar los espacios en blanco al principio y al final de la línea:** Se eliminan los espacios en blanco al principio y al final de la línea.
* **Comprobar si la línea está vacía:** Se comprueba si la línea está vacía.
* **Dividir la línea en campos:** Se divide la línea en campos y se almacena en el array `@campos`.
* **Almacenar los datos en un hash:** Se almacenan los datos en un hash, utilizando el primer campo de la línea como llave y el segundo campo de la línea como valor.
* **Mostrar los datos del archivo:** Se muestran los datos del archivo, iterando sobre las llaves del hash y mostrando el valor correspondiente a cada llave.