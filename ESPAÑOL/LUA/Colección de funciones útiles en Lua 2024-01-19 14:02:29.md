```lua
-- Crear una función que reciba una tabla y devuelva la suma de todos sus elementos.

function sumar_tabla(tabla)
  suma = 0
  for i, valor in ipairs(tabla) do
    suma = suma + valor
  end
  return suma
end

-- Crear una función que reciba una cadena y devuelva el número de vocales que contiene.

function contar_vocales(cadena)
  vocales = 0
  for i = 1, string.len(cadena) do
    caracter = string.sub(cadena, i, i)
    if caracter == "A" or caracter == "E" or caracter == "I" or caracter == "O" or caracter == "U" or caracter == "a" or caracter == "e" or caracter == "i" or caracter == "o" or caracter == "u" then
      vocales = vocales + 1
    end
  end
  return vocales
end

-- Crear una función que reciba un número y devuelva el factorial de ese número.

function factorial(numero)
  if numero == 0 then
    return 1
  else
    return numero * factorial(numero - 1)
  end
end

-- Crear una función que reciba un array de números y devuelva el máximo de ese array.

function maximo_array(array)
  maximo = array[1]
  for i = 2, #array do
    if array[i] > maximo then
      maximo = array[i]
    end
  end
  return maximo
end

-- Crear una función que reciba un array de números y devuelva el mínimo de ese array.

function minimo_array(array)
  minimo = array[1]
  for i = 2, #array do
    if array[i] < minimo then
      minimo = array[i]
    end
  end
  return minimo
end

-- Crear una función que reciba una cadena y devuelva el inverso de esa cadena.

function invertir_cadena(cadena)
  inverso = ""
  for i = #cadena, 1, -1 do
    inverso = inverso .. string.sub(cadena, i, i)
  end
  return inverso
end

-- Crear una función que reciba una tabla de llaves y valores y devuelva un objeto con esas llaves y valores.

function crear_objeto(tabla)
  objeto = {}
  for i, valor in ipairs(tabla) do
    objeto[valor[1]] = valor[2]
  end
  return objeto
end

-- Crear una función que reciba un objeto y devuelva una tabla de llaves y valores correspondiente a ese objeto.

function obtener_tabla(objeto)
  tabla = {}
  for llave, valor in pairs(objeto) do
    table.insert(tabla, {llave, valor})
  end
  return tabla
end

-- Crear una función que reciba una cadena y devuelva un array de las palabras de esa cadena.

function dividir_cadena(cadena)
  palabras = {}
  for palabra in string.gmatch(cadena, "[%w]+") do
    table.insert(palabras, palabra)
  end
  return palabras
end

-- Crear una función que reciba un array de palabras y devuelva una cadena con esas palabras separadas por espacios.

function unir_cadena(palabras)
  cadena = ""
  for i, palabra in ipairs(palabras) do
    if i > 1 then
      cadena = cadena .. " "
    end
    cadena = cadena .. palabra
  end
  return cadena
end

-- Crear una función que reciba un objeto y devuelva un JSON correspondiente a ese objeto.

function crear_json(objeto)
  json = "{"
  for llave, valor in pairs(objeto) do
    if type(valor) == "string" then
      valor = "\"" .. valor .. "\""
    end
    json = json .. "\"" .. llave .. "\":" .. valor .. ","
  end
  json = string.sub(json, 1, #json - 1) .. "}"
  return json
end

-- Crear una función que reciba un JSON y devuelva un objeto correspondiente a ese JSON.

function obtener_objeto(json)
  objeto = {}
  for llave, valor in string.gmatch(json, '"([%w]+)": ?(\[[^\]]+\]|[^,\}]+)') do
    if string.sub(valor, 1, 1) == "[" and string.sub(valor, #valor, #valor) == "]" then
      valor = string.gsub(valor, "\[|\]", "")
      valor = string.split(valor, ",")
    end
    objeto[llave] = valor
  end
  return objeto
end

-- Crear una función que reciba un URL y devuelva el contenido HTML de esa URL.

function obtener_html(url)
  local respuesta = http.request(url)
  return respuesta.body
end

-- Crear una función que reciba un HTML y devuelva un objeto con los datos de ese HTML.

function parsear_html(html)
  datos = {}
  for titulo in string.gmatch(html, "<title>(.*?)</title>") do
    datos["titulo"] = titulo
  end
  for enlace in string.gmatch(html, "<a href=\"(.*?)\">") do
    table.insert(datos["enlaces"], enlace)
  end
  for imagen in string.gmatch(html, "<img src=\"(.*?)\">") do
    table.insert(datos["imagenes"], imagen)
  end
  return datos