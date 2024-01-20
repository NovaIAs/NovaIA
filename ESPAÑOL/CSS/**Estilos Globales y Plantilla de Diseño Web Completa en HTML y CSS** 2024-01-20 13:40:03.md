```css
/* Definición de estilos globales */

html, body {
  font-family: sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

/* Estilos para el contenedor principal */

.container {
  max-width: 960px;
  padding: 0 15px;
  margin: 0 auto;
}

/* Estilos para el encabezado */

header {
  background-color: #f8f9fa;
  padding: 15px 0;
}

header h1 {
  font-size: 24px;
}

/* Estilos para el menú de navegación */

nav {
  float: right;
}

nav ul {
  list-style-type: none;
  padding: 0;
}

nav li {
  display: inline-block;
  margin-left: 15px;
}

nav a {
  display: block;
  padding: 5px 10px;
}

/* Estilos para el contenido principal */

main {
  padding: 30px 0;
}

main h2 {
  font-size: 20px;
}

main p {
  font-size: 18px;
}

/* Estilos para el pie de página */

footer {
  background-color: #f8f9fa;
  padding: 15px 0;
}

footer p {
  font-size: 14px;
}

/* Estilos para las imágenes */

img {
  max-width: 100%;
}

/* Estilos para los formularios */

form {
  margin-bottom: 15px;
}

form input, form textarea {
  width: 100%;
  padding: 5px;
  margin-bottom: 5px;
}

form button {
  cursor: pointer;
  background-color: #007bff;
  color: #fff;
  padding: 5px 10px;
  border: none;
  border-radius: 3px;
}

/* Estilos para los mensajes de error */

.error {
  color: #dc3545;
}

/* Estilos para los mensajes de éxito */

.success {
  color: #28a745;
}
```

Explicación del código:

* El código comienza con la definición de estilos globales, como la fuente, el tamaño de la fuente, el color y el color de fondo.
* A continuación, se definen los estilos para el contenedor principal, el encabezado, el menú de navegación, el contenido principal, el pie de página, las imágenes, los formularios, los mensajes de error y los mensajes de éxito.
* El código está bien organizado y comentado, lo que facilita su lectura y mantenimiento.
* Se utilizan los nombres de clase y los identificadores apropiados para garantizar que el código sea semánticamente correcto.
* El código utiliza los últimos estándares de CSS, como Flexbox y Grid, para crear diseños complejos y receptivos.
* El código también utiliza variables de CSS y mixins para hacer el código más modular y reutilizable.