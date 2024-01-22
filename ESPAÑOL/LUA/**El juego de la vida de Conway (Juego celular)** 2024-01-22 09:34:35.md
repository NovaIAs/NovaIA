```
-- Proyecto: "El juego de la vida de Conway"

-- Introducción:
-- El juego de la vida de Conway es un juego matemático creado en 1970 por el matemático y científico informático británico John Conway. 
-- Se trata de un juego celular autómata en el que un conjunto de células se somete a una serie de reglas sencillas.
-- Estas reglas determinan el estado de cada célula en la siguiente generación, en función de su estado actual y el de sus células vecinas. 
-- El juego se desarrolla en un espacio bidimensional, y las células pueden estar vivas o muertas.

-- Código:

-- Creamos una cuadrícula bidimensional para representar el mundo del juego.
mundo = {}
ancho = 80 -- Ancho de la cuadrícula
alto = 20 -- Alto de la cuadrícula

-- Inicializamos la cuadrícula con celdas muertas.
for i = 1, ancho do 
    mundo[i] = {}
    for j = 1, alto do
        mundo[i][j] = 0 -- 0 representa célula muerta
    end
end

-- Función para definir el estado inicial del mundo (tablero del juego).
function iniciarMundo()
    -- Celda muerta con vecino vivo se convierte en viva
    mundo[40][10] = 1 -- (x,y)
    mundo[41][10] = 1 -- (x+1,y)
    mundo[42][10] = 1 -- (x+2,y)
    mundo[40][11] = 1 -- (x,y+1)
    mundo[42][11] = 1 -- (x+2,y+1)
    mundo[40][12] = 1 -- (x,y+2)
    mundo[41][12] = 1 -- (x+1,y+2)
    mundo[42][12] = 1 -- (x+2,y+2)

    -- Celda viva con 2 o 3 vecinos vivos permanece viva
    mundo[30][10] = 1 -- (x,y)
    mundo[31][10] = 1 -- (x+1,y)
    mundo[32][10] = 1 -- (x+2,y)
    mundo[30][11] = 1 -- (x,y+1)
    mundo[32][11] = 1 -- (x+2,y+1)

    -- Celda viva con menos de 2 vecinos vivos muere por aislamiento
    mundo[20][10] = 1 -- (x,y)
    mundo[21][10] = 1 -- (x+1,y)

    -- Celda viva con más de 3 vecinos vivos muere por sobrepoblación
    mundo[50][10] = 1 -- (x,y)
    mundo[51][10] = 1 -- (x+1,y)
    mundo[52][10] = 1 -- (x+2,y)
    mundo[53][10] = 1 -- (x+3,y)
    mundo[54][10] = 1 -- (x+4,y)
end

-- Función para visualizar el mundo (tablero del juego) en la consola.
function visualizarMundo()
    -- Borramos la consola para una visualización limpia.
    os.execute("clear")

    -- Iteramos sobre la cuadrícula y mostramos cada celda.
    for i = 1, ancho do
        for j = 1, alto do
            if mundo[i][j] == 1 then -- Celda viva
                print("██", end="") -- Dos caracteres para representar la celda viva
            else -- Celda muerta
                print("  ", end="")
            end
        end
        print() -- Nueva línea para cada fila
    end
end

-- Función para actualizar el estado de las células en la siguiente generación.
function actualizarMundo()
    -- Creamos una nueva cuadrícula para almacenar el estado de la siguiente generación.
    nueva_mundo = {}
    ancho_nueva = ancho
    alto_nueva = alto

    -- Inicializamos la nueva cuadrícula con celdas muertas.
    for i = 1, ancho_nueva do
        nueva_mundo[i] = {}
        for j = 1, alto_nueva do
            nueva_mundo[i][j] = 0
        end
    end

    -- Iteramos sobre cada celda de la cuadrícula actual.
    for i = 1, ancho do
        for j = 1, alto do
            -- Contamos el número de células vecinas vivas.
            vecinos_vivos = 0
            for k = -1, 1 do -- Iteramos sobre los vecinos en las 8 direcciones
                for l = -1, 1 do
                    if not (k == 0 and l == 0) then -- Evitamos contar la celda actual
                        if mundo[(i + k + ancho) % ancho + 1][(j + l + alto) % alto + 1] == 1 then -- Celda vecina viva
                            vecinos_vivos = vecinos_vivos + 1
                        end
                    end
                end
            end

            -- Aplicamos las reglas del juego.
            if mundo[i][j] == 1 then -- Celda viva
                if vecinos_vivos < 2 or vecinos_vivos > 3 then -- Celda viva muere por aislamiento o sobrepoblación
                    nueva_mundo[i][j] = 0
                else -- Celda viva permanece viva
                    nueva_mundo[i][j] = 1
                end
            else -- Celda muerta
                if vecinos_vivos == 3 then -- Celda muerta con 3 vecinos vivos nace
                    nueva_mundo[i][j] = 1
                else -- Celda muerta permanece muerta
                    nueva_mundo[i][j] = 0
                end
            end
        end
    end

    -- Actualizamos la cuadrícula con el estado de la nueva generación.
    mundo = nueva_mundo
end

-- Función principal (main).
function main()
    -- Iniciamos el mundo con el estado inicial.
    iniciarMundo()

    -- Visualizamos el mundo en la consola.
    visualizarMundo()

    -- Bucle principal del juego.
    while true do
        -- Actualizamos el mundo a la siguiente generación.
        actualizarMundo()

        -- Visualizamos el mundo actualizado en la consola.
        visualizarMundo()

        -- Pausa de 100 milisegundos para ralentizar el juego.
        os.sleep(100)
    end
end

-- Llamamos a la función principal para iniciar el juego.
main()
```

**Explicación:**

1. **Creación de la cuadrícula:** Se crea una cuadrícula bidimensional llamada `mundo` para representar el espacio de juego. Cada celda de la cuadrícula puede estar viva (valor 1) o muerta (valor 0).
2. **Función `iniciarMundo()`:** Esta función se utiliza para definir el estado inicial del mundo. En este ejemplo, se crean varios patrones de células vivas para demostrar el comportamiento del juego.
3. **Función `visualizarMundo()`:** Esta función se utiliza para visualizar el estado actual del mundo en la consola. Imprime cada celda como dos caracteres: "██" para una celda viva y "  " para una celda muerta.
4. **Función `actualizarMundo()`:** Esta función se utiliza para actualizar el mundo a la siguiente generación. Aplica las reglas del juego a cada celda en función del estado de sus células vecinas.
5. **Función `main()`:** Esta es la función principal del juego. Inicializa el mundo con el estado inicial, visualiza el mundo y luego entra en un bucle infinito. En cada iteración del bucle, actualiza el mundo y lo visualiza de nuevo. El bucle incluye una pausa de 100 milisegundos para ralentizar el juego.

Cuando se ejecuta este código, el juego de la vida de Conway comienza y se visualiza en la consola. Puedes observar cómo las células vivas cambian de estado en cada generación, siguiendo las reglas del juego. Este juego es un ejemplo clásico de un sistema complejo que puede generar patrones y comportamientos emergentes a partir de reglas simples.