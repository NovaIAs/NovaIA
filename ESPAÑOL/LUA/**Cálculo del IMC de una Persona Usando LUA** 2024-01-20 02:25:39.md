```lua
-- Código complejo en LUA

-- Declaración de variables
local variables = {
    "nombre" = "Juan",
    "edad" = 20,
    "sexo" = "masculino",
    "estatura" = 1.80,
    "peso" = 80
}

-- Función para imprimir las variables
function imprimirVariables()
    for key, value in pairs(variables) do
        print(key .. ": " .. value)
    end
end

-- Función para calcular el índice de masa corporal (IMC)
function calcularIMC()
    local imc = variables.peso / (variables.estatura * variables.estatura)
    return imc
end

-- Función para imprimir el IMC
function imprimirIMC()
    local imc = calcularIMC()
    print("IMC: " .. imc)
end

-- Función principal
function main()
    imprimirVariables()
    imprimirIMC()
end

-- Llamada a la función principal
main()
```

Explicación del código:

* **Declaración de variables:** Se declaran las variables que se van a utilizar en el código.

* **Función para imprimir las variables:** Se define una función que imprime las variables.

* **Función para calcular el índice de masa corporal (IMC):** Se define una función que calcula el IMC.

* **Función para imprimir el IMC:** Se define una función que imprime el IMC.

* **Función principal:** Se define la función principal del código.

* **Llamada a la función principal:** Se llama a la función principal.