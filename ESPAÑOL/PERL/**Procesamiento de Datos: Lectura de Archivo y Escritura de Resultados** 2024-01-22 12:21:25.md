```perl
use strict;
use warnings;

# Definir variables globales
my $archivo_entrada = 'datos.txt';
my $archivo_salida = 'resultados.txt';
my $encabezado = 'Nombre,Edad,Ciudad';

# Abrir el archivo de entrada
open(my $fh_entrada, '<', $archivo_entrada) or die "Error al abrir el archivo de entrada: $!";

# Abrir el archivo de salida
open(my $fh_salida, '>', $archivo_salida) or die "Error al abrir el archivo de salida: $!";

# Escribir el encabezado en el archivo de salida
print $fh_salida "$encabezado\n";

# Leer el archivo de entrada línea por línea
while (my $linea = <$fh_entrada>) {
    # Dividir la línea en campos
    my @campos = split /,/, $linea;

    # Obtener el nombre, la edad y la ciudad del registro
    my $nombre = $campos[0];
    my $edad = $campos[1];
    my $ciudad = $campos[2];

    # Formatear el registro en una cadena
    my $registro = "$nombre,$edad,$ciudad";

    # Escribir el registro en el archivo de salida
    print $fh_salida "$registro\n";
}

# Cerrar el archivo de entrada
close($fh_entrada);

# Cerrar el archivo de salida
close($fh_salida);

# Mostrar un mensaje de confirmación
print "El archivo de resultados se ha generado correctamente.\n";
```

Este código lee un archivo de texto llamado `datos.txt` que contiene una lista de nombres, edades y ciudades, y genera un nuevo archivo de texto llamado `resultados.txt` que contiene los mismos datos en un formato más estructurado. El código utiliza el módulo `strict` para garantizar el uso correcto de las variables y el módulo `warnings` para mostrar advertencias en caso de problemas potenciales.

El código define dos variables globales: `$archivo_entrada` y `$archivo_salida`, que contienen las rutas a los archivos de entrada y salida respectivamente. También define una variable global `$encabezado` que contiene el encabezado que se utilizará en el archivo de salida.

El código abre el archivo de entrada en modo de lectura y el archivo de salida en modo de escritura. Si alguno de los archivos no puede abrirse, el código muestra un mensaje de error y termina.

El código lee el archivo de entrada línea por línea y divide cada línea en campos utilizando el carácter `,` como delimitador. Luego, el código obtiene el nombre, la edad y la ciudad del registro, y los formatea en una cadena. Finalmente, el código escribe el registro en el archivo de salida.

Una vez que todas las líneas del archivo de entrada han sido procesadas, el código cierra los archivos de entrada y salida y muestra un mensaje de confirmación.

Este código es relativamente complejo porque utiliza varios conceptos diferentes de Perl, incluyendo el uso de variables globales, el manejo de archivos, la división de cadenas y la generación de informes. Sin embargo, el código está bien organizado y comentado, lo que lo hace fácil de entender y modificar.