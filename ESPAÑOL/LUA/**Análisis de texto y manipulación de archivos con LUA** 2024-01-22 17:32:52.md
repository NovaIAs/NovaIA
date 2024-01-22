```lua
-- Esta función recibe una cadena de texto y la divide en un array de palabras.
function dividirPalabras(texto)
  local palabras = {}
  local inicioPalabra = 1
  for i = 1, #texto do
    if texto:sub(i, i) == " " then
      palabras[#palabras + 1] = texto:sub(inicioPalabra, i - 1)
      inicioPalabra = i + 1
    end
  end
  palabras[#palabras + 1] = texto:sub(inicioPalabra, #texto)
  return palabras
end

-- Esta función recibe un array de palabras y devuelve un array con las palabras únicas.
function palabrasUnicas(palabras)
  local palabrasUnicas = {}
  for i = 1, #palabras do
    local palabra = palabras[i]
    if not palabrasUnicas[palabra] then
      palabrasUnicas[palabra] = true
    end
  end
  return palabrasUnicas
end

-- Esta función recibe un array de palabras y devuelve un diccionario con las palabras como claves y el número de veces que aparecen en el array como valores.
function frecuenciaPalabras(palabras)
  local frecuenciaPalabras = {}
  for i = 1, #palabras do
    local palabra = palabras[i]
    if frecuenciaPalabras[palabra] then
      frecuenciaPalabras[palabra] = frecuenciaPalabras[palabra] + 1
    else
      frecuenciaPalabras[palabra] = 1
    end
  end
  return frecuenciaPalabras
end

-- Esta función recibe un diccionario con las palabras como claves y el número de veces que aparecen en el array como valores, y devuelve un array con las palabras ordenadas por el número de veces que aparecen.
function ordenarPalabrasPorFrecuencia(frecuenciaPalabras)
  local palabrasOrdenadas = {}
  for palabra, frecuencia in pairs(frecuenciaPalabras) do
    palabrasOrdenadas[#palabrasOrdenadas + 1] = {palabra, frecuencia}
  end
  table.sort(palabrasOrdenadas, function(a, b) return b[2] - a[2] end)
  return palabrasOrdenadas
end

-- Esta función recibe una cadena de texto y devuelve un array con las palabras únicas ordenadas por el número de veces que aparecen en el texto.
function analizarTexto(texto)
  local palabras = dividirPalabras(texto)
  local palabrasUnicas = palabrasUnicas(palabras)
  local frecuenciaPalabras = frecuenciaPalabras(palabras)
  local palabrasOrdenadas = ordenarPalabrasPorFrecuencia(frecuenciaPalabras)
  return palabrasOrdenadas
end

-- Esta función recibe una cadena de texto y la imprime en la consola.
function imprimirTexto(texto)
  print(texto)
end

-- Esta función recibe un array de palabras y las imprime en la consola.
function imprimirPalabras(palabras)
  for i = 1, #palabras do
    print(palabras[i])
  end
end

-- Esta función recibe un diccionario con las palabras como claves y el número de veces que aparecen en el array como valores, y las imprime en la consola.
function imprimirFrecuenciaPalabras(frecuenciaPalabras)
  for palabra, frecuencia in pairs(frecuenciaPalabras) do
    print(palabra .. ": " .. frecuencia)
  end
end

-- Esta función recibe un array con las palabras ordenadas por el número de veces que aparecen en el texto y las imprime en la consola.
function imprimirPalabrasOrdenadas(palabrasOrdenadas)
  for i = 1, #palabrasOrdenadas do
    print(palabrasOrdenadas[i][1] .. ": " .. palabrasOrdenadas[i][2])
  end
end

-- Esta función recibe una cadena de texto y la analiza. Luego, imprime el texto, las palabras únicas, la frecuencia de las palabras y las palabras ordenadas por el número de veces que aparecen en el texto.
function analizarEImprimirTexto(texto)
  local palabrasOrdenadas = analizarTexto(texto)
  imprimirTexto(texto)
  print()
  imprimirPalabras(palabrasUnicas(palabrasOrdenadas))
  print()
  imprimirFrecuenciaPalabras(frecuenciaPalabras(palabrasOrdenadas))
  print()
  imprimirPalabrasOrdenadas(palabrasOrdenadas)
end

-- Esta función recibe el nombre de un archivo y lee su contenido. Luego, analiza el contenido del archivo y lo imprime.
function analizarEImprimirArchivo(nombreArchivo)
  local file = io.open(nombreArchivo, "r")
  local texto = file:read("*all")
  file:close()
  analizarEImprimirTexto(texto)
end

-- Esta función recibe el nombre de un directorio y lista todos los archivos que contiene.
function listarArchivos(directorio)
  for file in io.dir(directorio) do
    print(file)
  end
end

-- Esta función recibe el nombre de un directorio y analiza e imprime el contenido de todos los archivos que contiene.
function analizarEImprimirTodosLosArchivos(directorio)
  for file in io.dir(directorio) do
    analizarEImprimirArchivo(directorio .. "/" .. file)
    print()
  end
end

-- Esta función recibe el nombre de un directorio y lo crea si no existe.
function crearDirectorio(directorio)
  if not io.dir(directorio) then
    io.mkdir(directorio)
  end
end

-- Esta función recibe el nombre de un archivo y lo crea si no existe.
function crearArchivo(nombreArchivo)
  local file = io.open(nombreArchivo, "w")
  file:close()
end

-- Esta función recibe el nombre de un archivo y lo elimina si existe.
function eliminarArchivo(nombreArchivo)
  if io.exist(nombreArchivo) then
    io.unlink(nombreArchivo)
  end
end

-- Esta función recibe el nombre de un directorio y lo elimina si existe.
function eliminarDirectorio(directorio)
  if io.dir(directorio) then
    io.rmdir(directorio)
  end
end

-- Esta función recibe el nombre de un archivo y devuelve su extensión.
function obtenerExtensionArchivo(nombreArchivo)
  return nombreArchivo:match(".*%..$")
end

-- Esta función recibe el nombre de un archivo y devuelve su nombre sin la extensión.
function obtenerNombreArchivoSinExtension(nombreArchivo)
  return nombreArchivo:match("^.*%..$")
end

-- Esta función recibe el nombre de un archivo y lo copia a otro archivo.
function copiarArchivo(nombreArchivoOrigen, nombreArchivoDestino)
  local fileOrigen = io.open(nombreArchivoOrigen, "r")
  local fileDestino = io.open(nombreArchivoDestino, "w")
  fileDestino:write(fileOrigen:read("*all"))
  fileOrigen:close()
  fileDestino:close()
end

-- Esta función recibe el nombre de un archivo y lo mueve a otro directorio.
function moverArchivo(nombreArchivo, directorioDestino)
  local nombreArchivoDestino = directorioDestino .. "/" .. obtenerNombreArchivoSinExtension(nombreArchivo) .. obtenerExtensionArchivo(nombreArchivo)
  copiarArchivo(nombreArchivo, nombreArchivoDestino)
  eliminarArchivo(nombreArchivo)
end