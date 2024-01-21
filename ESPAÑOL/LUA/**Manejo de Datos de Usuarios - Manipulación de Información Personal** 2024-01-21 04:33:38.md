```lua
-- Creación de una tabla global para almacenar datos
datosGlobales = {}

-- Función para crear un nuevo usuario
function crearUsuario(nombre, apellido, correo)
  -- Creamos una tabla para almacenar los datos del usuario
  nuevoUsuario = {}
  nuevoUsuario.nombre = nombre
  nuevoUsuario.apellido = apellido
  nuevoUsuario.correo = correo

  -- Agregamos el nuevo usuario a la tabla global
  datosGlobales[nombre] = nuevoUsuario
end

-- Función para obtener los datos de un usuario
function obtenerUsuario(nombre)
  -- Devolvemos los datos del usuario almacenados en la tabla global
  return datosGlobales[nombre]
end

-- Función para actualizar los datos de un usuario
function actualizarUsuario(nombre, nuevosDatos)
  -- Actualizamos los datos del usuario en la tabla global
  datosGlobales[nombre] = nuevosDatos
end

-- Crear un nuevo usuario
crearUsuario("Juan", "Pérez", "juanperez@gmail.com")

-- Obtener los datos del usuario
usuario = obtenerUsuario("Juan")
print("Nombre:", usuario.nombre)
print("Apellido:", usuario.apellido)
print("Correo:", usuario.correo)

-- Actualizar los datos del usuario
nuevosDatos = {
  nombre = "Juan Carlos",
  apellido = "Pérez García",
  correo = "juancperez@gmail.com"
}
actualizarUsuario("Juan", nuevosDatos)

-- Comprobar si los datos del usuario se actualizaron
usuario = obtenerUsuario("Juan")
print("\nNombre:", usuario.nombre)
print("Apellido:", usuario.apellido)
print("Correo:", usuario.correo)
```

Explicación del código:

1. Creamos una tabla global llamada `datosGlobales` para almacenar los datos de los usuarios.
2. Definimos una función llamada `crearUsuario` que recibe tres argumentos: el nombre, el apellido y el correo electrónico del usuario. Esta función crea una tabla con los datos del usuario y la añade a la tabla global `datosGlobales`.
3. Definimos una función llamada `obtenerUsuario` que recibe un argumento: el nombre del usuario. Esta función devuelve los datos del usuario almacenados en la tabla global `datosGlobales`.
4. Definimos una función llamada `actualizarUsuario` que recibe dos argumentos: el nombre del usuario y los nuevos datos del usuario. Esta función actualiza los datos del usuario en la tabla global `datosGlobales`.
5. Creamos un nuevo usuario llamando a la función `crearUsuario` y pasando los datos del usuario.
6. Obtenemos los datos del usuario llamando a la función `obtenerUsuario` y pasando el nombre del usuario.
7. Actualizamos los datos del usuario llamando a la función `actualizarUsuario` y pasando el nombre del usuario y los nuevos datos del usuario.
8. Comprobamos si los datos del usuario se actualizaron llamando de nuevo a la función `obtenerUsuario` y pasando el nombre del usuario.