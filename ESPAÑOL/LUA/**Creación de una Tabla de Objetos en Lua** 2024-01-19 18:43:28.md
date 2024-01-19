```lua
-- Este código crea una tabla de objetos (un objeto es una tabla con atributos y métodos).

-- La tabla de objetos "animales" se define con tres atributos: "nombre", "especie" y "edad".
animales = {}
animales[1] = { nombre = "Perro", especie = "Canis lupus familiaris", edad = 5 }
animales[2] = { nombre = "Gato", especie = "Felis catus", edad = 3 }
animales[3] = { nombre = "Conejo", especie = "Oryctolagus cuniculus", edad = 2 }

-- El método "hablar" devuelve el sonido que hace el animal.
function animales.hablar(self)
  if self.especie == "Canis lupus familiaris" then
    return "Guau!"
  elseif self.especie == "Felis catus" then
    return "Miau!"
  elseif self.especie == "Oryctolagus cuniculus" then
    return "Pio!"
  end
end

-- El método "caminar" devuelve el movimiento que hace el animal.
function animales.caminar(self)
  if self.especie == "Canis lupus familiaris" then
    return "El perro corre."
  elseif self.especie == "Felis catus" then
    return "El gato anda."
  elseif self.especie == "Oryctolagus cuniculus" then
    return "El conejo salta."
  end
end

-- La función "imprimir_animal" imprime los atributos y métodos del animal.
function imprimir_animal(animal)
  print("Nombre: ", animal.nombre)
  print("Especie: ", animal.especie)
  print("Edad: ", animal.edad)
  print("Sonido: ", animal:hablar())
  print("Movimiento: ", animal:caminar())
  print("\n")
end

-- Iteramos sobre la tabla de objetos "animales" y llamamos a la función "imprimir_animal" para cada animal.
for i, animal in pairs(animales) do
  imprimir_animal(animal)
end
```

Este código crea una tabla de objetos (un objeto es una tabla con atributos y métodos). La tabla de objetos "animales" se define con tres atributos: "nombre", "especie" y "edad". El método "hablar" devuelve el sonido que hace el animal. El método "caminar" devuelve el movimiento que hace el animal. La función "imprimir_animal" imprime los atributos y métodos del animal. Iteramos sobre la tabla de objetos "animales" y llamamos a la función "imprimir_animal" para cada animal.