```css
/* 1. Modo oscuro global */
:root {
  --bg-color: #222;
  --text-color: #fff;
}

html, body {
  background-color: var(--bg-color);
  color: var(--text-color);
}

/* 2. Layout de la página */
.container {
  max-width: 960px;
  padding: 20px;
  margin: 0 auto;
}

header {
  background-color: #333;
  padding: 15px;
  margin-bottom: 20px;
}

header h1 {
  font-size: 24px;
  margin-bottom: 5px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  padding: 0;
}

header nav li {
  display: inline-block;
  margin-right: 10px;
}

header nav li a {
  text-decoration: none;
  color: #fff;
}

/* 3. Zona principal de contenido */
main {
  margin-bottom: 20px;
}

article {
  background-color: #fff;
  padding: 20px;
  margin-bottom: 20px;
}

article h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

article p {
  font-size: 16px;
  line-height: 1.5;
}

/* 4. Barra lateral */
aside {
  float: right;
  width: 250px;
}

aside h3 {
  font-size: 18px;
  margin-bottom: 10px;
}

aside ul {
  list-style-type: none;
  padding: 0;
}

aside li {
  margin-bottom: 10px;
}

aside li a {
  text-decoration: none;
  color: #333;
}

/* 5. Pie de página */
footer {
  background-color: #333;
  padding: 15px;
  margin-top: 20px;
}

footer p {
  font-size: 14px;
  text-align: center;
}

/* 6. Estilos adicionales */
h1, h2, h3 {
  font-family: Arial, sans-serif;
}

p {
  font-family: Georgia, serif;
}

a {
  text-decoration: none;
  color: #007bff;
}

img {
  max-width: 100%;
}
```

Explicación del código:

* **Modo oscuro global:** El código CSS comienza estableciendo un modo oscuro global para la página web. Esto se hace utilizando variables CSS, que permiten definir valores que pueden ser reutilizados en todo el código. En este caso, se definen las variables `--bg-color` y `--text-color` con los valores `#222` y `#fff` respectivamente.

* **Layout de la página:** El código crea un contenedor centrado con un ancho máximo de 960 píxeles. El contenedor contiene un encabezado, una zona principal de contenido y una barra lateral.

* **Zona principal de contenido:** La zona principal de contenido contiene los artículos de la página web. Cada artículo tiene un encabezado, un párrafo y una lista de elementos.

* **Barra lateral:** La barra lateral contiene una lista de enlaces a otras páginas web.

* **Pie de página:** El pie de página contiene un párrafo con el texto "Copyright 2023" centrado.

* **Estilos adicionales:** El código CSS también incluye algunos estilos adicionales para elementos específicos, como encabezados, párrafos, enlaces e imágenes.

Este código CSS es complejo y completo, y puede ser utilizado para crear páginas web con un diseño profesional y elegante.