```lua
-- Este código simula el funcionamiento de un sistema operativo simple.

-- Definimos las estructuras de datos básicas del sistema.
 procesos = {}            -- Lista de procesos en ejecución.
 colas_espera = {}       -- Colas de espera de los procesos.
 dispositivos = {}        -- Lista de dispositivos disponibles.
 memoria = {}            -- Memoria del sistema.

-- Definimos las funciones básicas del sistema.
function crear_proceso(nombre, prioridad)
    -- Crea un nuevo proceso y lo agrega a la lista de procesos en ejecución.
    local proceso = {
        nombre = nombre,
        prioridad = prioridad,
        estado = "nuevo",
        pila = {},
        datos = {},
    }
    procesos[nombre] = proceso
    return proceso
end

function ejecutar_proceso(proceso)
    -- Ejecuta el proceso especificado.
    while proceso.estado == "ejecutando" do
        -- Ejecutar la siguiente instrucción del proceso.
        local instruccion = proceso.pila[proceso.pc]
        if instruccion == "hlt" then
            -- Instrucción de detención.
            proceso.estado = "terminado"
            break
        elseif instruccion == "mov" then
            -- Instrucción de movimiento.
            local destino = proceso.pila[proceso.pc + 1]
            local origen = proceso.pila[proceso.pc + 2]
            proceso.datos[destino] = proceso.datos[origen]
            proceso.pc = proceso.pc + 3
        elseif instruccion == "add" then
            -- Instrucción de suma.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            proceso.datos[destino] = operando1 + operando2
            proceso.pc = proceso.pc + 4
        elseif instruccion == "sub" then
            -- Instrucción de resta.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            proceso.datos[destino] = operando1 - operando2
            proceso.pc = proceso.pc + 4
        elseif instruccion == "mul" then
            -- Instrucción de multiplicación.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            proceso.datos[destino] = operando1 * operando2
            proceso.pc = proceso.pc + 4
        elseif instruccion == "div" then
            -- Instrucción de división.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            proceso.datos[destino] = operando1 / operando2
            proceso.pc = proceso.pc + 4
        elseif instruccion == "jmp" then
            -- Instrucción de salto.
            proceso.pc = proceso.pila[proceso.pc + 1]
        elseif instruccion == "jeq" then
            -- Instrucción de salto si igual.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            if operando1 == operando2 then
                proceso.pc = destino
            else
                proceso.pc = proceso.pc + 4
            end
        elseif instruccion == "jne" then
            -- Instrucción de salto si no igual.
            local destino = proceso.pila[proceso.pc + 1]
            local operando1 = proceso.datos[proceso.pila[proceso.pc + 2]]
            local operando2 = proceso.datos[proceso.pila[proceso.pc + 3]]
            if operando1 ~= operando2 then
                proceso.pc = destino