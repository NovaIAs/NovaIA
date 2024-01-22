```css
/* Estructura de la página */

* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

html, body {
  height: 100%;
  font-family: sans-serif;
  font-size: 16px;
  line-height: 1.6;
}

/* Encabezado */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
}

header h1 {
  font-size: 24px;
  margin: 0;
  padding: 10px;
}

/* Navegación */

nav {
  float: left;
  width: 200px;
  height: 100%;
  background-color: #444;
}

nav ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

nav li {
  display: block;
  padding: 10px;
  border-bottom: 1px solid #555;
}

nav li:hover {
  background-color: #555;
}

nav li a {
  color: #fff;
  text-decoration: none;
}

/* Contenido */

main {
  margin-left: 200px;
  padding: 10px;
}

/* Pie de página */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
}

footer p {
  margin: 0;
  padding: 10px;
}

/* Estilos adicionales */

.btn {
  display: inline-block;
  padding: 10px 20px;
  border: 1px solid #ccc;
  border-radius: 5px;
  background-color: #eee;
  color: #333;
  text-decoration: none;
}

.btn:hover {
  background-color: #ccc;
}

.error {
  color: #ff0000;
}

.success {
  color: #00ff00;
}

/* Consultas de medios */

@media (max-width: 768px) {
  nav {
    width: 100%;
    height: auto;
  }

  main {
    margin-left: 0;
  }
}
```

Este código CSS crea un diseño de página web básico con un encabezado, una navegación, un contenido y un pie de página. El encabezado y el pie de página son fijos, mientras que la navegación y el contenido son fluidos. La navegación está a la izquierda y tiene un ancho fijo de 200 píxeles. El contenido está a la derecha de la navegación y tiene un ancho fluido.

El código CSS también incluye algunos estilos adicionales para botones, mensajes de error y mensajes de éxito. Además, incluye una consulta de medios para dispositivos móviles, que cambia el diseño de la página web para que sea más fácil de usar en dispositivos móviles.

Aquí hay una explicación detallada del código CSS:

* La sección **estructura de la página** define los estilos básicos para la página web, incluyendo el tamaño de la fuente, la altura de la línea y el tamaño de la caja.
* La sección **encabezado** define el estilo del encabezado de la página web, incluyendo el color de fondo, el color del texto y el tamaño de la fuente.
* La sección **navegación** define el estilo de la navegación de la página web, incluyendo el color de fondo, el color del texto y el tamaño de la fuente. También incluye un estilo para los elementos de la lista de navegación y para los enlaces dentro de los elementos de la lista.
* La sección **contenido** define el estilo del contenido de la página web, incluyendo el margen y el relleno.
* La sección **pie de página** define el estilo del pie de página de la página web, incluyendo el color de fondo, el color del texto y el tamaño de la fuente.
* La sección **estilos adicionales** define algunos estilos adicionales para botones, mensajes de error y mensajes de éxito.
* La sección **consultas de medios** define un estilo para dispositivos móviles, que cambia el diseño de la página web para que sea más fácil de usar en dispositivos móviles.