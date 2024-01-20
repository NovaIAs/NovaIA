```lua
-- Módulo para manejar estructuras de datos complejas

-- Define una clase con propiedades y métodos
Clase = {}
function Clase:new(propiedades)
  local obj = {}
  obj.__index = self
  obj.__call = function(self, ...)
    return self:metodo(...)
  end
  -- Asigna las propiedades al objeto
  for nombre, valor in pairs(propiedades) do
    obj[nombre] = valor
  end
  return obj
end

-- Define un método para la clase
Clase.metodo = function(self, arg)
  return self.propiedad + arg
end

-- Crea un objeto de la clase
objeto = Clase:new({propiedad = 10})

-- Llama al método del objeto
resultado = objeto:metodo(5)

-- Imprime el resultado
print(resultado) -- Imprime 15


-- Módulo para manejar expresiones regulares complejas

-- Define una función para validar una expresión regular
function validar_regex(regex)
  local error = nil
  try
    re.compile(regex)
  catch err
    error = err.message
  end
  return error
end

-- Comprueba si una expresión regular es válida
error = validar_regex("[[a-z]+")
if error then
  print(error) -- Imprime "regex error: missing closing bracket near '[['"
else
  print("regex es valida")
end


-- Módulo para manejar números complejos

-- Define una clase para representar números complejos
Complejo = {}
function Complejo:new(real, imaginario)
  local obj = {}
  obj.__index = self
  obj.__call = function(self, ...)
    return self:metodo(...)
  end
  obj.real = real
  obj.imaginario = imaginario
  return obj
end

-- Define un método para sumar números complejos
Complejo.suma = function(self, otro)
  return Complejo:new(self.real + otro.real, self.imaginario + otro.imaginario)
end

-- Crea dos objetos de la clase
c1 = Complejo:new(1, 2)
c2 = Complejo:new(3, 4)

-- Suma los objetos
c3 = c1:suma(c2)

-- Imprime el resultado
print(c3.real, c3.imaginario) -- Imprime "4 6"
```