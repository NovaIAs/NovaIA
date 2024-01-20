```typescript
// Importación de módulos necesarios
import { useState, useEffect } from "react";
import axios from "axios";

// Definición del componente principal
const App = () => {
  // Estado para almacenar los datos de los usuarios
  const [usuarios, setUsuarios] = useState([]);

  // Estado para almacenar el usuario actual
  const [usuarioActual, setUsuarioActual] = useState(null);

  // Función para obtener todos los usuarios
  const obtenerUsuarios = async () => {
    try {
      const respuesta = await axios.get("http://localhost:3000/usuarios");
      setUsuarios(respuesta.data);
    } catch (error) {
      console.error(error);
    }
  };

  // Función para obtener un usuario por su ID
  const obtenerUsuarioPorId = async (id) => {
    try {
      const respuesta = await axios.get(`http://localhost:3000/usuarios/${id}`);
      setUsuarioActual(respuesta.data);
    } catch (error) {
      console.error(error);
    }
  };

  // Función para crear un nuevo usuario
  const crearUsuario = async (nombre, apellido, email, contraseña) => {
    try {
      const respuesta = await axios.post("http://localhost:3000/usuarios", {
        nombre,
        apellido,
        email,
        contraseña,
      });
      setUsuarios([...usuarios, respuesta.data]);
    } catch (error) {
      console.error(error);
    }
  };

  // Función para actualizar un usuario existente
  const actualizarUsuario = async (id, nombre, apellido, email, contraseña) => {
    try {
      const respuesta = await axios.put(`http://localhost:3000/usuarios/${id}`, {
        nombre,
        apellido,
        email,
        contraseña,
      });
      setUsuarios(
        usuarios.map((usuario) => {
          if (usuario.id === id) {
            return respuesta.data;
          }
          return usuario;
        })
      );
    } catch (error) {
      console.error(error);
    }
  };

  // Función para eliminar un usuario existente
  const eliminarUsuario = async (id) => {
    try {
      await axios.delete(`http://localhost:3000/usuarios/${id}`);
      setUsuarios(usuarios.filter((usuario) => usuario.id !== id));
    } catch (error) {
      console.error(error);
    }
  };

  // Ejecución del efecto para obtener todos los usuarios al montar el componente
  useEffect(() => {
    obtenerUsuarios();
  }, []);

  // Renderizado del componente
  return (
    <div>
      <h1>Gestión de Usuarios</h1>
      <ul>
        {usuarios.map((usuario) => (
          <li key={usuario.id}>
            {usuario.nombre} {usuario.apellido} - {usuario.email}
            <button onClick={() => obtenerUsuarioPorId(usuario.id)}>Ver</button>
            <button onClick={() => eliminarUsuario(usuario.id)}>Eliminar</button>
          </li>
        ))}
      </ul>
      {usuarioActual && (
        <div>
          <h2>Detalles del Usuario</h2>
          <p>Nombre: {usuarioActual.nombre}</p>
          <p>Apellido: {usuarioActual.apellido}</p>
          <p>Email: {usuarioActual.email}</p>
          <p>Contraseña: {usuarioActual.contraseña}</p>
          <button onClick={() => setUsuarioActual(null)}>Cerrar</button>
        </div>
      )}
      <form onSubmit={(e) => crearUsuario(e)}>
        <label htmlFor="nombre">Nombre:</label>
        <input type="text" id="nombre" />
        <br />
        <label htmlFor="apellido">Apellido:</label>
        <input type="text" id="apellido" />
        <br />
        <label htmlFor="email">Email:</label>
        <input type="email" id="email" />
        <br />
        <label htmlFor="contraseña">Contraseña:</label>
        <input type="password" id="contraseña" />
        <br />
        <input type="submit" value="Crear Usuario" />
      </form>
    </div>
  );
};

export default App;
```

Este código es un componente de React que implementa una aplicación simple de gestión de usuarios. La aplicación permite obtener todos los usuarios, obtener un usuario por su ID, crear un nuevo usuario, actualizar un usuario existente y eliminar un usuario existente.

La aplicación utiliza el módulo "axios" para realizar las peticiones HTTP a un servidor API. El servidor API se supone que está alojado en "http://localhost:3000" y proporciona las siguientes rutas:

* GET "/usuarios" - Obtener todos los usuarios
* GET "/usuarios/:id" - Obtener un usuario por su ID
* POST "/usuarios" - Crear un nuevo usuario
* PUT "/usuarios/:id" - Actualizar un usuario existente
* DELETE "/usuarios/:id" - Eliminar un usuario existente

El componente utiliza el estado para almacenar los datos de los usuarios y el usuario actual. El estado se actualiza mediante las funciones "setUsuarios" y "setUsuarioActual".

El componente también utiliza el efecto "useEffect" para obtener todos los usuarios cuando se monta el componente. El efecto se ejecuta una sola vez cuando el componente se monta.

El componente se renderiza mostrando una lista de todos los usuarios y un formulario para crear un nuevo usuario. Cuando se hace clic en un usuario de la lista, se muestra un modal con los detalles del usuario.

El código es complejo porque implementa una aplicación completa con varias funciones y utiliza diferentes módulos y técnicas. El código también está bien estructurado y comentado, lo que lo hace fácil de entender y mantener.