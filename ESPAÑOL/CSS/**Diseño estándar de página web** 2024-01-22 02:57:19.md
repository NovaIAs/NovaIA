```css
/*
  Estilo general del documento
*/

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  background-color: #f5f5f5;
}

/*
  Encabezado
*/

header {
  padding: 15px 30px;
  background-color: #444;
  color: #fff;
}

header h1 {
  font-size: 28px;
  font-weight: bold;
  margin-bottom: 10px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  display: flex;
}

header nav ul li {
  margin-right: 15px;
}

header nav ul li a {
  text-decoration: none;
  color: #fff;
}

header nav ul li a:hover {
  color: #ccc;
}

/*
  Contenido
*/

main {
  padding: 30px;
}

main h2 {
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 20px;
}

main p {
  font-size: 16px;
  line-height: 1.5;
  margin-bottom: 20px;
}

/*
  Pie de página
*/

footer {
  padding: 15px 30px;
  background-color: #444;
  color: #fff;
}

footer p {
  font-size: 14px;
  text-align: center;
}

/*
  Estilos adicionales
*/

.button {
  padding: 10px 20px;
  background-color: #444;
  color: #fff;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

.button:hover {
  background-color: #ccc;
  color: #444;
}

.success {
  color: #008000;
}

.error {
  color: #FF0000;
}
```

Explicación del código:

* El código CSS comienza con un comentario que describe el propósito del código.
* El código establece estilos generales para el documento, como la tipografía y los colores.
* El código también incluye estilos para el encabezado, el contenido y el pie de página.
* El código también incluye algunos estilos adicionales, como un botón y mensajes de éxito y error.
* El código CSS está bien organizado y comentado, lo que facilita su lectura y mantenimiento.