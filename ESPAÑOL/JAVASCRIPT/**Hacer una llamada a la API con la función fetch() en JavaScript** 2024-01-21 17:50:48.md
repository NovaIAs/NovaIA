```javascript
// Crear un objeto de configuración para la llamada a la API
const configuracion = {
  // Indicamos el método de la petición HTTP
  method: 'GET',
  // Indicamos la cabecera de la petición
  headers: {
    'Content-Type': 'application/json',
    'Authorization': 'Bearer ' + accessToken,
  },
  // Indicamos el cuerpo de la petición
  body: JSON.stringify({
    nombre: 'Juan',
    apellido: 'García',
    edad: 30,
  }),
};

// Realizar una llamada a la API utilizando la función fetch()
fetch('https://api.example.com/usuarios', configuracion)
  // Convertir la respuesta en un objeto JSON
  .then((respuesta) => respuesta.json())
  // Procesar el objeto JSON
  .then((datos) => {
    // Mostrar los datos del usuario en el navegador
    const nombre = datos.nombre;
    const apellido = datos.apellido;
    const edad = datos.edad;

    document.getElementById('nombre').innerHTML = nombre;
    document.getElementById('apellido').innerHTML = apellido;
    document.getElementById('edad').innerHTML = edad;
  })
  // Manejar los errores
  .catch((error) => {
    console.error('Error al realizar la llamada a la API:', error);
  });
```

**Explicación del código:**

* La función `fetch()` se utiliza para realizar una llamada a la API.
* La propiedad `method` de la configuración indica el método de la petición HTTP que se quiere realizar. En este caso, se utiliza el método `GET`.
* La propiedad `headers` de la configuración indica la cabecera de la petición HTTP. En este caso, se indica que el tipo de contenido de la petición es `application/json` y que se incluye un token de acceso como autorización.
* La propiedad `body` de la configuración indica el cuerpo de la petición HTTP. En este caso, se envía un objeto JSON con los datos del usuario.
* La función `.then()` se utiliza para procesar la respuesta de la llamada a la API. En este caso, se convierte la respuesta en un objeto JSON.
* La función `.then()` se utiliza de nuevo para procesar el objeto JSON. En este caso, se muestran los datos del usuario en el navegador.
* La función `.catch()` se utiliza para manejar los errores que puedan ocurrir durante la llamada a la API.