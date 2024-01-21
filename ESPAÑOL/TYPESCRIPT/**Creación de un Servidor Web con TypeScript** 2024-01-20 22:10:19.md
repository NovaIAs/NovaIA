```typescript
// Este código crea un servidor web simple que responde a peticiones GET y POST.

// Importamos los módulos necesarios.
import * as express from "express";
import * as bodyParser from "body-parser";

// Creamos una aplicación de express.
const app = express();

// Configuramos el puerto en el que escuchará el servidor.
const port = 3000;

// Utilizamos el middleware de body-parser para poder leer los datos del cuerpo de las peticiones POST.
app.use(bodyParser.json());

// Definimos la ruta raíz del servidor.
app.get("/", (req, res) => {
  res.send("¡Hola, mundo!");
});

// Definimos la ruta para la petición POST.
app.post("/peticion-post", (req, res) => {
  // Obtenemos los datos del cuerpo de la petición.
  const datos = req.body;

  // Procesamos los datos.
  const resultado = datos.numero1 + datos.numero2;

  // Devolvemos el resultado.
  res.send({ resultado });
});

// Iniciamos el servidor.
app.listen(port, () => {
  console.log(`Servidor escuchando en el puerto ${port}`);
});
```

**Explicación del código:**

* El código empieza importando los módulos necesarios.
* Creamos una aplicación de express y configuramos el puerto en el que escuchará el servidor.
* Utilizamos el middleware de body-parser para poder leer los datos del cuerpo de las peticiones POST.
* Definimos la ruta raíz del servidor, que responde con un mensaje de "Hola, mundo!".
* Definimos la ruta para la petición POST, que recibe dos números en el cuerpo de la petición y devuelve su suma.
* Iniciamos el servidor y lo hacemos escuchar en el puerto especificado.

Este código es un ejemplo simple de cómo crear un servidor web en TypeScript. Puede ser ampliado y personalizado para crear aplicaciones web más complejas.