```lua
-- Empezamos definiendo la función principal, que se ejecutará al iniciar el programa.
function main()
  -- Creamos una tabla con los nombres de los planetas del sistema solar.
  planetas = {"Mercurio", "Venus", "Tierra", "Marte", "Júpiter", "Saturno", "Urano", "Neptuno"}

  -- Creamos un loop que iterará sobre la tabla de planetas.
  for i, planeta in ipairs(planetas) do
    -- Imprimimos el nombre del planeta actual.
    print("Planeta", i, ": ", planeta)
  end

  -- Creamos una función para calcular la distancia entre dos planetas.
  function distancia(planeta1, planeta2)
    -- Obtenemos las posiciones de los planetas en el espacio.
    posicion1 = posiciones[planeta1]
    posicion2 = posiciones[planeta2]

    -- Calculamos la distancia entre los planetas usando la fórmula de la distancia euclídea.
    distancia = math.sqrt((posicion1.x - posicion2.x)^2 + (posicion1.y - posicion2.y)^2 + (posicion1.z - posicion2.z)^2)

    -- Devolvemos la distancia calculada.
    return distancia
  end

  -- Creamos una tabla que almacena las posiciones de los planetas en el espacio.
  posiciones = {
    ["Mercurio"] = {x = 0.39, y = 0.24, z = 0.06},
    ["Venus"] = {x = 0.72, y = 0.00, z = -0.09},
    ["Tierra"] = {x = 1.00, y = 0.00, z = 0.00},
    ["Marte"] = {x = 1.52, y = -0.01, z = 0.10},
    ["Júpiter"] = {x = 5.20, y = 0.08, z = -0.04},
    ["Saturno"] = {x = 9.54, y = 0.04, z = 0.16},
    ["Urano"] = {x = 19.22, y = 0.01, z = -0.17},
    ["Neptuno"] = {x = 30.11, y = 0.00, z = 0.30}
  }

  -- Creamos un loop que iterará sobre la tabla de planetas.
  for i, planeta1 in ipairs(planetas) do
    -- Creamos un loop que iterará sobre la tabla de planetas nuevamente, excluyendo al planeta actual.
    for j, planeta2 in ipairs(planetas) do
      -- Si el planeta actual es diferente al planeta que estamos iterando, calculamos la distancia entre ellos.
      if planeta1 ~= planeta2 then
        distancia_planetas = distancia(planeta1, planeta2)

        -- Imprimimos la distancia calculada.
        print("Distancia entre", planeta1, "y", planeta2, ": ", distancia_planetas, "AU")
      end
    end
  end
end

-- Llamamos a la función principal para ejecutar el programa.
main()
```

**Explicación del código:**

* La función `main()` es la función principal del programa. Se ejecuta al iniciar el programa.

* La variable `planetas` es una tabla que contiene los nombres de los planetas del sistema solar.

* El loop `for i, planeta in ipairs(planetas)` itera sobre la tabla de planetas e imprime el nombre de cada planeta.

* La función `distancia()` calcula la distancia entre dos planetas utilizando la fórmula de la distancia euclídea.

* La variable `posiciones` es una tabla que almacena las posiciones de los planetas en el espacio.

* El loop `for i, planeta1 in ipairs(planetas)` itera sobre la tabla de planetas e itera sobre la tabla de planetas nuevamente, excluyendo al planeta actual.

* Si el planeta actual es diferente al planeta que estamos iterando, calculamos la distancia entre ellos utilizando la función `distancia()`.

* Imprimimos la distancia calculada entre los dos planetas.