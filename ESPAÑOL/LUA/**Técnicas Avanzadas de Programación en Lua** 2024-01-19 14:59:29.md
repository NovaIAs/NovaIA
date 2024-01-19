```lua
-- Definición de tabla
local tabla = {
    ["elemento1"] = "valor1",
    ["elemento2"] = 2,
    ["elemento3"] = {
        ["subElemento1"] = "valor3.1",
        ["subElemento2"] = false,
    },
}

-- Función para recorrer tabla
function recorrerTabla(tabla)
    for clave, valor in pairs(tabla) do
        print("Clave:", clave)
        print("Valor:", valor)
        if type(valor) == "table" then
            print("Es una tabla, recorriendo...")
            recorrerTabla(valor)
        end
    end
end

-- Llamada a la función para recorrer la tabla
recorrerTabla(tabla)

-- Definición de función con argumentos
function suma(a, b)
    return a + b
end

-- Llamada a la función con argumentos
print("Suma de 3 y 5:", suma(3, 5))

-- Creación de un cierre
local capturarVariable = function()
    local variableLocal = 10
    return function()
        return variableLocal
    end
end

-- Obtención del cierre
local funcionCierre = capturarVariable()

-- Llamada al cierre
print("Valor capturado por el cierre:", funcionCierre())

-- Manejo de errores con try-catch
try
    -- Código que puede generar un error
    error("Ocurrió un error")
catch
    -- Captura del error y manejo
    print("Error capturado:", error)
end

-- Expresión regular para validar correos electrónicos
local patronCorreo = "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

-- Función para validar correos electrónicos
function validarCorreo(correo)
    return correo:match(patronCorreo) ~= nil
end

-- Llamada a la función de validación de correo electrónico
print("¿'example@example.com' es un correo electrónico válido?", validarCorreo("example@example.com"))

-- Programación orientada a objetos
class Persona {
    constructor(nombre, edad) {
        self.nombre = nombre
        self.edad = edad
    }
    hablar() {
        print("Hola, mi nombre es", self.nombre, "y tengo", self.edad, "años.")
    }
}

-- Creación de una instancia de la clase Persona
local persona = Persona("Juan", 25)

-- Llamada al método hablar de la instancia
persona:hablar()

-- Módulo para gestionar usuarios
module("Usuarios") {
    local usuarios = {}

    function crearUsuario(nombre, contrasena)
        usuarios[nombre] = contrasena
        print("Usuario", nombre, "creado.")
    end

    function obtenerUsuario(nombre)
        return usuarios[nombre]
    end

    function eliminarUsuario(nombre)
        usuarios[nombre] = nil
        print("Usuario", nombre, "eliminado.")
    end
}

-- Llamadas a las funciones del módulo
Usuarios.crearUsuario("usuario1", "contrasena1")
print("Contraseña del usuario 'usuario1':", Usuarios.obtenerUsuario("usuario1"))
Usuarios.eliminarUsuario("usuario1")
```

Explicación del código:

1. Se define una tabla con diferentes tipos de elementos, incluyendo una subtabla.
2. Se crea una función para recorrer la tabla y mostrar las claves y valores de sus elementos.
3. Se define una función llamada "suma" que toma dos argumentos y devuelve su suma.
4. Se utiliza un cierre para capturar una variable local dentro de una función y acceder a ella desde fuera de la función.
5. Se utiliza el manejo de errores con "try-catch" para capturar y manejar errores que puedan ocurrir durante la ejecución del código.
6. Se define una expresión regular para validar direcciones de correo electrónico.
7. Se crea una función que utiliza la expresión regular para validar una dirección de correo electrónico dada.
8. Se utiliza la programación orientada a objetos para definir una clase llamada "Persona" y crear una instancia de la clase.
9. Se crea un módulo llamado "Usuarios" que proporciona funciones para gestionar usuarios, como crear, obtener y eliminar usuarios.

Este código demuestra varias técnicas avanzadas de programación en Lua, incluyendo el manejo de tablas, funciones, cierres, manejo de errores, expresiones regulares, programación orientada a objetos y módulos.