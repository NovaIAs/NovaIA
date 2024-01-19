```ruby
# Programa que lee un archivo de texto y cuenta el número de palabras, líneas y caracteres.

# Definir una función para contar las palabras en una línea de texto.
def contar_palabras(linea)
  # Dividir la línea en palabras usando el espacio como delimitador.
  palabras = linea.split(" ")

  # Contar el número de palabras en la lista.
  palabras.length
end

# Definir una función para contar las líneas en un archivo de texto.
def contar_lineas(archivo)
  # Leer el archivo de texto línea por línea.
  lineas = File.readlines(archivo)

  # Contar el número de líneas en la lista.
  lineas.length
end

# Definir una función para contar los caracteres en un archivo de texto.
def contar_caracteres(archivo)
  # Leer el archivo de texto como una cadena de texto.
  texto = File.read(archivo)

  # Contar el número de caracteres en la cadena de texto.
  texto.length
end

# Solicitar al usuario que introduzca el nombre del archivo de texto.
nombre_archivo = ARGV[0]

# Comprobar si el archivo de texto existe.
if File.exist?(nombre_archivo)
  # Leer el archivo de texto y contar el número de palabras, líneas y caracteres.
  palabras = contar_palabras(nombre_archivo)
  lineas = contar_lineas(nombre_archivo)
  caracteres = contar_caracteres(nombre_archivo)

  # Mostrar los resultados al usuario.
  puts "El archivo #{nombre_archivo} tiene #{palabras} palabras, #{lineas} líneas y #{caracteres} caracteres."
else
  # Mostrar un mensaje de error al usuario.
  puts "El archivo #{nombre_archivo} no existe."
end
```

Explicación del código:

* La función `contar_palabras` divide una línea de texto en palabras usando el espacio como delimitador y luego cuenta el número de palabras en la lista.
* La función `contar_lineas` lee un archivo de texto línea por línea y luego cuenta el número de líneas en la lista.
* La función `contar_caracteres` lee un archivo de texto como una cadena de texto y luego cuenta el número de caracteres en la cadena de texto.
* El programa solicita al usuario que introduzca el nombre del archivo de texto y luego comprueba si el archivo existe.
* Si el archivo existe, el programa lee el archivo de texto y cuenta el número de palabras, líneas y caracteres.
* Si el archivo no existe, el programa muestra un mensaje de error al usuario.