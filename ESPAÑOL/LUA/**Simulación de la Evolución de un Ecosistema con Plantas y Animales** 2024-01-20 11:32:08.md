```lua
-- Un programa que simula la evolucion de un ecosistema con animales y plantas.

-- Cargamos las librerias necesarias
require "math"
require "random"

-- Definimos los parámetros del ecosistema
ancho = 100
alto = 100
numPlantas = 100
numAnimales = 100

-- Creamos un mundo 2D para representar el ecosistema
mundo = {}
for i = 1, ancho do --rellenamos el array con un array de los valores de alto
    mundo[i] = {}
    for j = 1, alto do
        mundo[i][j] = nil
    end
end

-- Creamos las plantas y los animales
plantas = {}
animales = {}

for i = 1, numPlantas do
    x = math.random(1, ancho)
    y = math.random(1, alto)
    plantas[i] = {x = x, y = y, energia = 100}
end

for i = 1, numAnimales do
    x = math.random(1, ancho)
    y = math.random(1, alto)
    animales[i] = {x = x, y = y, energia = 100, hambre = 0, reproduccion = 0}
end

-- Simulamos la evolución del ecosistema
for iteracion = 1, 1000 do
    -- Actualizamos las plantas
    for i, planta in ipairs(plantas) do
        -- La planta crece y consume energía
        planta.energia = planta.energia + 100
        planta.energia = planta.energia - 10

        -- Controlamos que la planta no se quede sin energía
        if planta.energia <= 0 then
            plantas[i] = nil
        end

        -- La planta se reproduce
        if planta.energia >= 100 then
            x = planta.x + math.random(-1, 1)
            y = planta.y + math.random(-1, 1)

            -- Comprobamos que la nueva planta no está fuera del mundo
            if x >= 0 and x <= ancho and y >= 0 and y <= alto then
                mundo[x][y] = {x = x, y = y, energia = 100}
            end
        end
    end

    -- Actualizamos los animales
    for i, animal in ipairs(animales) do
        -- El animal se mueve
        animal.x = animal.x + math.random(-1, 1)
        animal.y = animal.y + math.random(-1, 1)

        -- Comprobamos que el animal no está fuera del mundo
        if animal.x < 0 then animal.x = ancho end
        if animal.x > ancho then animal.x = 1 end
        if animal.y < 0 then animal.y = alto end
        if animal.y > alto then animal.y = 1 end

        -- El animal come plantas
        if mundo[animal.x][animal.y] ~= nil then
            animal.energia = animal.energia + 100
            mundo[animal.x][animal.y] = nil
        end

        -- El animal tiene hambre
        animal.hambre = animal.hambre + 1

        -- Controlamos que el animal no se quede sin energía
        if animal.energia  <= 0 or animal.hambre >= 100 then
            animales[i] = nil
        end

        -- El animal se reproduce
        if animal.energia  >= 100 and animal.reproduccion >= 10 then
            x = animal.x + math.random(-1, 1)
            y = animal.y + math.random(-1, 1)

            -- Comprobamos que el nuevo animal no está fuera del mundo
            if x >= 0 and x <= ancho and y >= 0 and y <= alto then
                animales[i + 1] = {x = x, y = y, energia = 100, hambre = 0, reproduccion = 0}
            end

            animal.reproduccion = 0
        end
        
        animal.reproduccion = animal.reproduccion + 1
    end

    -- Mostramos el estado del ecosistema
    print("Iteración "..iteracion.."..")
    print("Plantas: "..#plantas)
    print("Animales: "..#animales)
    for i = 1, ancho do
        for j = 1, alto do
            if mundo[i][j] ~= nil then
                print("*",end='')
            else
                print(" ",end='')
            end
        end
        print()
    end
    print()

end
```

Este programa simula la evolución de un ecosistema con plantas y animales. El ecosistema se representa mediante un mundo 2D, donde cada posición puede estar ocupada por una planta o un animal.

Las plantas crecen y consumen energía, y se reproducen cuando tienen suficiente energía. Los animales se mueven, comen plantas y tienen hambre. Se reproducen cuando tienen suficiente energía y han pasado un cierto tiempo desde su última reproducción.

El programa simula la evolución del ecosistema durante un número determinado de iteraciones. En cada iteración, se actualizan las plantas y los animales, y se muestra el estado del ecosistema.

El programa utiliza las siguientes librerías:

* math: para generar números aleatorios y realizar cálculos matemáticos.
* random: para generar números aleatorios.
* ipairs: para iterar sobre un array.

El programa define los siguientes parámetros:

* ancho: el ancho del mundo.
* alto: el alto del mundo.
* numPlantas: el número inicial de plantas.
* numAnimales: el número inicial de animales.

El programa crea el mundo 2D y lo llena con plantas y animales. Luego, simula la evolución del ecosistema durante un número determinado de iteraciones. En cada iteración, se actualizan las plantas y los animales, y se muestra el estado del ecosistema.