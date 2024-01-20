```css
/* ESTILOS GENERALES */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: "Arial", sans-serif;
  background-color: #f5f5f5;
}

/* ENCABEZADO */

header {
  background-color: #333;
  color: #fff;
  padding: 15px;
  text-align: center;
}

header h1 {
  font-size: 24px;
  margin-bottom: 10px;
}

header nav {
  margin-top: 10px;
}

header nav a {
  color: #fff;
  text-decoration: none;
  padding: 5px 10px;
  margin-right: 10px;
}

header nav a:hover {
  background-color: #444;
}

/* SECCIÓN DE CONTENIDO */

section {
  padding: 15px;
}

section h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

section p {
  font-size: 16px;
  line-height: 1.5em;
}

/* ELEMENTOS DEL FORMULARIO */

form {
  margin-top: 20px;
}

form label {
  display: block;
  margin-bottom: 5px;
}

form input[type="text"],
form input[type="email"],
form textarea {
  width: 100%;
  padding: 5px;
  margin-bottom: 10px;
}

form button {
  background-color: #333;
  color: #fff;
  padding: 5px 10px;
  border: none;
  cursor: pointer;
}

form button:hover {
  background-color: #444;
}

/* PIE DE PÁGINA */

footer {
  background-color: #333;
  color: #fff;
  padding: 15px;
  text-align: center;
}

footer p {
  margin-bottom: 0;
}

/* ESTILOS DE RESPUESTA */

@media (max-width: 768px) {
  header nav {
    flex-direction: column;
  }

  header nav a {
    margin-right: 0;
    margin-bottom: 10px;
  }

  section {
    padding: 10px;
  }
}
```

Este código CSS crea un diseño básico para un sitio web con un encabezado, una sección de contenido, un formulario y un pie de página. También incluye estilos de respuesta para dispositivos móviles.

El código está organizado en varias secciones, cada una de las cuales contiene los estilos para un elemento diferente del sitio web. Por ejemplo, la sección "Estilos generales" contiene los estilos que se aplican a todos los elementos del sitio web, como la fuente y el color de fondo. La sección "Encabezado" contiene los estilos para el encabezado del sitio web, como el color de fondo y el texto.

El código también utiliza consultas de medios para aplicar diferentes estilos a dispositivos móviles. Por ejemplo, la consulta de medios "@media (max-width: 768px)" aplica estilos a los dispositivos móviles que tienen un ancho máximo de 768 píxeles. Estos estilos incluyen cambiar la dirección del menú de navegación de horizontal a vertical y reducir el tamaño de la fuente del texto.