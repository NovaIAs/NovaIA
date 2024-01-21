```lua
-- ¡Hola, mundo!

print("¡Hola, mundo!")

-- Variables

-- Las variables en Lua se declaran sin utilizar el tipo de dato explícitamente. El tipo de dato se infiere automáticamente en función del valor asignado.

nombre = "Juan" -- Variable de tipo string
edad = 25 -- Variable de tipo number
casado = false -- Variable de tipo boolean

-- Operadores

-- Los operadores aritméticos básicos son +, -, *, / y %.

resultado = 3 + 4 -- Suma
resultado = 5 - 2 -- Resta
resultado = 6 * 3 -- Multiplicación
resultado = 10 / 2 -- División
resultado = 11 % 3 -- Módulo

-- Condiciones

-- El operador de comparación "==" comprueba la igualdad de dos valores. El operador "!=" comprueba la desigualdad.

if nombre == "Juan" then -- Si el nombre es "Juan", se ejecuta el bloque de código
  print("¡Hola, Juan!")
end

-- Bucles

-- El bucle "for" se utiliza para iterar sobre una colección de valores.

for i = 1, 10 do -- Recorre los números del 1 al 10, incrementando el valor de i en 1 en cada iteración
  print(i)
end

-- Funciones

-- Las funciones en Lua se definen utilizando la palabra clave "function".

function sumar(a, b) -- Define una función que suma dos números
  return a + b
end

resultado = sumar(3, 4) -- Llama a la función sumar y asigna el resultado a la variable "resultado"

-- Tablas

-- Las tablas son estructuras de datos que pueden almacenar pares clave-valor. Los índices de las tablas pueden ser cadenas de texto o números.

tabla = {
  ["nombre"] = "Juan",
  ["edad"] = 25,
  ["casado"] = false
}

print(tabla["nombre"]) -- Imprime el valor del campo "nombre" de la tabla

-- Módulos

-- Los módulos en Lua son archivos que contienen código que puede ser importado y utilizado en otros archivos.

require("mi_modulo") -- Importa el módulo "mi_modulo"

mi_modulo.funcion_1() -- Llama a la función "funcion_1" del módulo "mi_modulo"

-- Metadatos

-- Los metadatos en Lua son información adicional que se puede asociar a un valor. Los metadatos se pueden utilizar para almacenar información sobre el tipo de dato, el origen del valor o cualquier otra información que sea útil.

local valor = 123
metadatos = getmetatable(valor) -- Obtiene los metadatos del valor

setmetatable(valor, {
  __tostring = function(valor) -- Define una función que se llama cuando se intenta convertir el valor a una cadena de texto
    return "El valor es " .. tostring(valor)
  end
})

print(valor) -- Imprime el valor, llamando a la función __tostring definida en los metadatos

```

Este código cubre una amplia gama de conceptos de Lua, incluyendo variables, operadores, condiciones, bucles, funciones, tablas, módulos y metadatos. Es un ejemplo complejo que demuestra la versatilidad y expresividad del lenguaje Lua.