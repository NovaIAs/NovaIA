```lua
-- Módulo principal del programa
local m = {}

-- Función que inicializa el programa
function m.inicializar()
    -- Creamos una tabla para almacenar los datos de los empleados
    local empleados = {}

    -- Añadimos algunos empleados de ejemplo
    empleados[1] = { nombre = "Juan", apellido = "García", edad = 25 }
    empleados[2] = { nombre = "María", apellido = "Pérez", edad = 30 }
    empleados[3] = { nombre = "Pedro", apellido = "López", edad = 35 }

    -- Imprimimos los datos de los empleados
    for i, empleado in pairs(empleados) do
        print(i, empleado.nombre, empleado.apellido, empleado.edad)
    end
end

-- Función que imprime el menú de opciones
function m.menu()
    print("\nMenú de opciones:")
    print("1. Añadir empleado")
    print("2. Eliminar empleado")
    print("3. Buscar empleado")
    print("4. Salir")
    print("¿Qué opción desea elegir?")
end

-- Función que añade un empleado
function m.añadirEmpleado()
    -- Pedimos los datos del empleado al usuario
    print("\nIntroduce los datos del empleado:")
    print("Nombre:")
    local nombre = io.read()
    print("Apellido:")
    local apellido = io.read()
    print("Edad:")
    local edad = io.read()

    -- Creamos el nuevo empleado
    local empleado = { nombre = nombre, apellido = apellido, edad = edad }

    -- Añadimos el nuevo empleado a la tabla de empleados
    local empleados = m.getEmpleados()
    empleados[#empleados + 1] = empleado

    -- Imprimimos un mensaje de confirmación
    print("\nEl empleado se ha añadido correctamente.")
end

-- Función que elimina un empleado
function m.eliminarEmpleado()
    -- Pedimos el ID del empleado al usuario
    print("\nIntroduce el ID del empleado que desea eliminar:")
    local id = io.read()

    -- Eliminamos el empleado de la tabla de empleados
    local empleados = m.getEmpleados()
    empleados[id] = nil

    -- Imprimimos un mensaje de confirmación
    print("\nEl empleado se ha eliminado correctamente.")
end

-- Función que busca un empleado
function m.buscarEmpleado()
    -- Pedimos el nombre del empleado al usuario
    print("\nIntroduce el nombre del empleado que desea buscar:")
    local nombre = io.read()

    -- Buscamos el empleado en la tabla de empleados
    local empleados = m.getEmpleados()
    for id, empleado in pairs(empleados) do
        if empleado.nombre == nombre then
            -- Imprimimos los datos del empleado
            print("\n", id, empleado.nombre, empleado.apellido, empleado.edad)
            return
        end
    end

    -- Imprimimos un mensaje de error si el empleado no se encontró
    print("\nNo se encontró ningún empleado con ese nombre.")
end

-- Función que devuelve la tabla de empleados
function m.getEmpleados()
    -- Si la tabla de empleados no existe, la creamos
    if not m.empleados then
        m.empleados = {}
    end

    -- Devolvemos la tabla de empleados
    return m.empleados
end

-- Función que ejecuta el programa
function m.ejecutar()
    -- Inicializamos el programa
    m.inicializar()

    -- Mostramos el menú de opciones
    repeat
        m.menu()

        -- Leemos la opción elegida por el usuario
        local opcion = io.read()

        -- Ejecutamos la función correspondiente a la opción elegida
        if opcion == "1" then
            m.añadirEmpleado()
        elseif opcion == "2" then
            m.eliminarEmpleado()
        elseif opcion == "3" then
            m.buscarEmpleado()
        elseif opcion == "4" then
            break
        else
            print("\nOpción no válida.")
        end
    until opcion == "4"
end

-- Ejecutamos el programa
m.ejecutar()
```

Este código es un programa completo en LUA que permite gestionar una lista de empleados. El programa tiene un menú de opciones que permite añadir, eliminar y buscar empleados. El programa también almacena los datos de los empleados en una tabla.

El código está bien comentado y es fácil de entender. También es extensible, ya que se pueden añadir nuevas funciones al programa fácilmente.

Aquí tienes una explicación más detallada del código:

* La función `m.inicializar()` inicializa el programa y crea la tabla de empleados.
* La función `m.menu()` muestra el menú de opciones al usuario.
* La función `m.añadirEmpleado()` añade un nuevo empleado a la tabla de empleados.
* La función `m.eliminarEmpleado()` elimina un empleado de la tabla de empleados.
* La función `m.buscarEmpleado()` busca un empleado en la tabla de empleados.
* La función `m.getEmpleados()` devuelve la tabla de empleados.
* La función `m.ejecutar()` ejecuta el programa.

El código utiliza la función `io.read()` para leer los datos del usuario. También utiliza la función `print()` para imprimir mensajes en la consola.

El código está organizado en módulos. Un módulo es un archivo que contiene código que se puede reutilizar en otros programas. El módulo `m` contiene el código principal del programa.

El código utiliza comentarios para documentar el código. Los comentarios son líneas de texto que no se ejecutan cuando se ejecuta el programa. Los comentarios se utilizan para explicar el código y hacer que sea más fácil de entender.