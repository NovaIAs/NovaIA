```lua
-- Objetivo: Crear una calculadora simple en Lua.

-- Definimos una función llamada "calcular" que recibe dos operandos y un operador.
function calcular(op1, op2, operador)
    -- Verificamos si el operador es válido.
    if operador ~= "+" and operador ~= "-" and operador ~= "*" and operador ~= "/" then
        error("Operador inválido.")
    end

    -- Calculamos el resultado de la operación.
    local resultado = 0
    if operador == "+" then
        resultado = op1 + op2
    elseif operador == "-" then
        resultado = op1 - op2
    elseif operador == "*" then
        resultado = op1 * op2
    elseif operador == "/" then
        if op2 == 0 then
            error("No se puede dividir por cero.")
        end
        resultado = op1 / op2
    end

    -- Devolvemos el resultado de la operación.
    return resultado
end

-- Mostramos un mensaje de bienvenida.
print("Bienvenido a la calculadora.")

-- Solicitamos al usuario que ingrese los operandos y el operador.
print("Ingrese el primer operando:")
local op1 = tonumber(io.read())

print("Ingrese el segundo operando:")
local op2 = tonumber(io.read())

print("Ingrese el operador (+, -, *, /):")
local operador = io.read()

-- Calculamos el resultado de la operación.
local resultado = calcular(op1, op2, operador)

-- Mostramos el resultado de la operación.
print(string.format("%d %s %d = %d", op1, operador, op2, resultado))
```

Explicación del código:

1. Definimos una función llamada "calcular" que recibe dos operandos y un operador. Esta función calcula el resultado de la operación especificada por el operador.
2. Verificamos si el operador es válido. Si el operador no es "+", "-", "*", o "/", se muestra un mensaje de error.
3. Calculamos el resultado de la operación. Si el operador es "+", realizamos una suma. Si el operador es "-", realizamos una resta. Si el operador es "*", realizamos una multiplicación. Si el operador es "/", realizamos una división.
4. Devolvemos el resultado de la operación.
5. Mostramos un mensaje de bienvenida.
6. Solicitamos al usuario que ingrese los operandos y el operador.
7. Calculamos el resultado de la operación.
8. Mostramos el resultado de la operación.