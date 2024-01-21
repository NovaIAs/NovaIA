```perl
# Importar los módulos necesarios
use strict;
use warnings;
use DateTime;
use JSON;
use DBI;

# Definir las constantes
my $db_host = 'localhost';
my $db_user = 'root';
my $db_pass = 'password';
my $db_name = 'mi_base_de_datos';

# Conectarse a la base de datos
my $dbh = DBI->connect("DBI:mysql:$db_name:$db_host", $db_user, $db_pass) or die "No se pudo conectar a la base de datos: $DBI::errstr";

# Preparar la consulta SQL
my $sql = "SELECT * FROM tabla_de_datos WHERE campo_de_fecha >= ? AND campo_de_fecha <= ?";

# Definir el rango de fechas
my $fecha_inicial = DateTime->new(year => 2023, month => 1, day => 1);
my $fecha_final = DateTime->new(year => 2023, month => 12, day => 31);

# Ejecutar la consulta SQL
my $sth = $dbh->prepare($sql);
$sth->execute($fecha_inicial->ymd, $fecha_final->ymd);

# Obtener los resultados de la consulta
my @filas = $sth->fetchall_arrayref;

# Convertir los resultados a JSON
my $json = encode_json \@filas;

# Imprimir los resultados en la consola
print $json;

# Desconectarse de la base de datos
$dbh->disconnect;

# El código anterior realiza las siguientes tareas:

# Importa los módulos necesarios para el funcionamiento del programa.
# Define las constantes necesarias para la conexión a la base de datos.
# Se conecta a la base de datos utilizando el módulo DBI.
# Prepara la consulta SQL que se utilizará para obtener los datos de la tabla.
# Define el rango de fechas para el que se obtendrán los datos.
# Ejecuta la consulta SQL utilizando el módulo DBI.
# Obtiene los resultados de la consulta y los almacena en una variable.
# Convierte los resultados de la consulta a formato JSON utilizando el módulo JSON.
# Imprime los resultados en la consola.
# Se desconecta de la base de datos.
```