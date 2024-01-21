```lua
-- Variable global para almacenar la lista de tareas
tareas = {}

-- Función para agregar una nueva tarea a la lista
function agregarTarea(descripcion)
  -- Obtener el número de tareas actuales
  numTareas = #tareas

  -- Crear una nueva tabla para la nueva tarea
  nuevaTarea = {
    descripcion = descripcion,
    completada = false
  }

  -- Agregar la nueva tarea a la lista
  tareas[numTareas + 1] = nuevaTarea

  -- Imprimir un mensaje de confirmación
  print("Tarea agregada exitosamente.")
end

-- Función para marcar una tarea como completada
function marcarCompletada(numeroTarea)
  -- Verificar si el número de tarea es válido
  if numeroTarea <= #tareas and numeroTarea > 0 then
    -- Obtener la tarea correspondiente al número dado
    tarea = tareas[numeroTarea]

    -- Marcar la tarea como completada
    tarea.completada = true

    -- Imprimir un mensaje de confirmación
    print("Tarea marcada como completada.")
  else
    -- Imprimir un mensaje de error
    print("El número de tarea no es válido.")
  end
end

-- Función para imprimir la lista de tareas
function imprimirListaTareas()
  -- Recorrer la lista de tareas
  for i = 1, #tareas do
    -- Obtener la tarea actual
    tarea = tareas[i]

    -- Imprimir el número de tarea
    print(string.format("%d. ", i))

    -- Imprimir la descripción de la tarea
    print(tarea.descripcion)

    -- Imprimir el estado de la tarea (completada o no)
    if tarea.completada then
      print("Completada")
    else
      print("Pendiente")
    end

    -- Imprimir una línea en blanco
    print("")
  end
end

-- Función principal del programa
function main()
  -- Imprimir un mensaje de bienvenida
  print("Bienvenido al Gestor de Tareas")

  -- Mostrar el menú de opciones
  while true do
    print("Opciones:")
    print("1. Agregar una nueva tarea")
    print("2. Marcar una tarea como completada")
    print("3. Imprimir la lista de tareas")
    print("4. Salir")

    -- Obtener la opción seleccionada por el usuario
    opcion = io.read()

    -- Procesar la opción seleccionada
    if opcion == 1 then
      -- Solicitar al usuario que ingrese la descripción de la nueva tarea
      print("Ingrese la descripción de la nueva tarea:")
      descripcion = io.read()

      -- Agregar la nueva tarea a la lista
      agregarTarea(descripcion)
    elseif opcion == 2 then
      -- Solicitar al usuario que ingrese el número de la tarea a marcar como completada
      print("Ingrese el número de la tarea a marcar como completada:")
      numeroTarea = io.read()

      -- Marcar la tarea como completada
      marcarCompletada(numeroTarea)
    elseif opcion == 3 then
      -- Imprimir la lista de tareas
      imprimirListaTareas()
    elseif opcion == 4 then
      -- Salir del programa
      break
    else
      -- Imprimir un mensaje de error
      print("Opción no válida.")
    end
  end

  -- Imprimir un mensaje de despedida
  print("Gracias por usar el Gestor de Tareas")
end

-- Llamar a la función principal para iniciar el programa
main()
```

Explicación del código:

1. Variable global `tareas`: Esta variable se utiliza para almacenar la lista de tareas. Es una tabla (array asociativo) donde cada elemento es una tabla que representa una tarea. Cada tarea tiene dos campos: `descripcion` (la descripción de la tarea) y `completada` (un booleano que indica si la tarea está completada o no).

2. Función `agregarTarea`: Esta función agrega una nueva tarea a la lista de tareas. Recibe un parámetro `descripcion` que es la descripción de la nueva tarea. La función crea una nueva tabla para la nueva tarea, la agrega a la lista y imprime un mensaje de confirmación.

3. Función `marcarCompletada`: Esta función marca una tarea como completada. Recibe un parámetro `numeroTarea` que es el número de la tarea a marcar como completada. La función verifica si el número de tarea es válido (es decir, si está dentro del rango de tareas existentes) y, si es así, marca la tarea como completada imprimiendo un mensaje de confirmación.

4. Función `imprimirListaTareas`: Esta función imprime la lista de tareas. Recorre la lista de tareas y, para cada tarea, imprime el número de tarea, la descripción y el estado de la tarea (completada o no).

5. Función `main`: Esta es la función principal del programa. Muestra un menú de opciones al usuario y le permite agregar nuevas tareas, marcar tareas como completadas, imprimir la lista de tareas y salir del programa.

6. Llamada a la función `main`: Esta línea llama a la función `main` para iniciar el programa.