```lua
-- Este código crea una red neuronal sencilla en Lua para resolver el problema XOR.

-- Crear una tabla para almacenar los pesos de la red neuronal.
pesos = {
    {0.1, 0.2}, -- Pesos de la capa de entrada a la capa oculta.
    {0.3, 0.4}  -- Pesos de la capa oculta a la capa de salida.
}

-- Crear una tabla para almacenar las neuronas de la red neuronal.
neuronas = {
    {0, 0},  -- Neuronas de la capa de entrada.
    {0, 0}   -- Neuronas de la capa oculta.
    {0}      -- Neurona de la capa de salida.
}

-- Función para calcular la salida de una neurona.
function calcular_salida(neurona, pesos)
    -- Calcular la suma ponderada de las entradas.
    suma_ponderada = 0
    for i = 1, #neurona do
        suma_ponderada = suma_ponderada + neurona[i] * pesos[i]
    end

    -- Aplicar la función de activación para obtener la salida.
    salida = 1 / (1 + math.exp(-suma_ponderada))

    return salida
end

-- Función para propagar la señal a través de la red neuronal.
function propagar_senial(entrada)
    -- Establecer las neuronas de la capa de entrada.
    neuronas[1][1] = entrada[1]
    neuronas[1][2] = entrada[2]

    -- Calcular las salidas de las neuronas de la capa oculta.
    for i = 1, #neuronas[2] do
        neuronas[2][i] = calcular_salida(neuronas[1], pesos[1])
    end

    -- Calcular la salida de la neurona de la capa de salida.
    neuronas[3][1] = calcular_salida(neuronas[2], pesos[2])

    -- Devolver la salida de la red neuronal.
    return neuronas[3][1]
end

-- Función para entrenar la red neuronal.
function entrenar_red_neuronal(entradas, salidas)
    -- Establecer un valor inicial para la tasa de aprendizaje.
    tasa_aprendizaje = 0.1

    -- Iterar sobre los datos de entrenamiento.
    for i = 1, #entradas do
        -- Propagar la señal a través de la red neuronal.
        salida = propagar_senial(entradas[i])

        -- Calcular el error.
        error = salidas[i] - salida

        -- Actualizar los pesos de la red neuronal.
        for j = 1, #pesos do
            for k = 1, #pesos[j] do
                pesos[j][k] = pesos[j][k] + tasa_aprendizaje * error * neuronas[j][k]
            end
        end
    end
end

-- Entrenar la red neuronal.
entradas = {{0, 0}, {0, 1}, {1, 0}, {1, 1}}
salidas = {0, 1, 1, 0}
entrenar_red_neuronal(entradas, salidas)

-- Probar la red neuronal.
entrada = {0, 1}
salida = propagar_senial(entrada)
print(salida)
```

Este código crea una red neuronal de dos capas con dos neuronas en la capa oculta y una neurona en la capa de salida. La red neuronal utiliza una función de activación sigmoidea.

El código primero crea una tabla para almacenar los pesos de la red neuronal. Luego, crea una tabla para almacenar las neuronas de la red neuronal.

A continuación, el código define una función para calcular la salida de una neurona. Esta función toma como entrada una neurona y los pesos correspondientes y calcula la suma ponderada de las entradas. Luego, aplica la función de activación sigmoidea para obtener la salida.

Después, el código define una función para propagar la señal a través de la red neuronal. Esta función toma como entrada una entrada y propaga la señal a través de las capas de la red neuronal.

Luego, el código define una función para entrenar la red neuronal. Esta función toma como entrada los datos de entrenamiento y entrena la red neuronal ajustando los pesos de la red neuronal.

Finalmente, el código entrena la red neuronal y la prueba con una entrada de prueba.

Este código es un ejemplo de cómo crear y entrenar una red neuronal sencilla en Lua.