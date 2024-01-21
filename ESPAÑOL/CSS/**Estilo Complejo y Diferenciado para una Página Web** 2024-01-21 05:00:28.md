```css
/* ESTILOS GLOBALES */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: sans-serif;
  line-height: 1.6;
  background-color: #f5f5f5;
}

/* ENCABEZADO */

header {
  background-color: #333;
  color: #fff;
  padding: 20px 0;
}

header h1 {
  font-size: 36px;
  font-weight: bold;
  text-align: center;
}

header nav {
  text-align: center;
}

header nav a {
  color: #fff;
  text-decoration: none;
  padding: 10px;
}

header nav a:hover {
  background-color: #444;
}

/* CONTENIDO PRINCIPAL */

main {
  margin-top: 20px;
  padding: 20px;
  background-color: #fff;
}

main h2 {
  font-size: 24px;
  font-weight: bold;
}

main p {
  font-size: 16px;
}

main ul {
  list-style-type: none;
  padding: 0;
}

main ul li {
  padding: 5px;
}

/* PIE DE PÁGINA */

footer {
  background-color: #333;
  color: #fff;
  padding: 20px 0;
  text-align: center;
}

footer p {
  font-size: 12px;
}

/* MEDIOS */

@media (max-width: 768px) {
  header nav {
    text-align: left;
  }

  header nav a {
    display: block;
  }
}
```

Este código CSS crea un diseño complejo y diferenciado para una página web. El código está dividido en secciones para facilitar su lectura y mantenimiento:

* **Estilos globales:** Esta sección contiene estilos que se aplican a todos los elementos de la página, como la fuente, el tamaño de letra y el color de fondo.
* **Encabezado:** Esta sección contiene estilos para el encabezado de la página, incluyendo el logotipo, el menú de navegación y el título de la página.
* **Contenido principal:** Esta sección contiene estilos para el contenido principal de la página, incluyendo el título, los párrafos, las listas y las imágenes.
* **Pie de página:** Esta sección contiene estilos para el pie de página de la página, incluyendo el texto y los enlaces.
* **Medios:** Esta sección contiene consultas de medios que se utilizan para aplicar diferentes estilos a la página en función del dispositivo en el que se esté visualizando.

Este código es un ejemplo de cómo se puede utilizar CSS para crear diseños complejos y diferenciados para páginas web.