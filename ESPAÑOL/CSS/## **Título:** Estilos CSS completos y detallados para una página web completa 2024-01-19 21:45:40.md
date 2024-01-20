```css
/* Estilos generales */

body {
  font-family: sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
  text-decoration: underline;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.75rem;
}

h4 {
  font-size: 1.5rem;
}

h5 {
  font-size: 1.25rem;
}

h6 {
  font-size: 1rem;
}

p {
  margin-bottom: 1rem;
}

ul, ol {
  list-style-position: inside;
  padding-left: 1.25rem;
}

ul li, ol li {
  margin-bottom: 0.5rem;
}

/* Estilos del encabezado */

header {
  background-color: #f8f9fa;
  padding: 0.5rem 1rem;
}

.navbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.navbar-brand {
  font-size: 1.5rem;
}

.navbar-nav {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.navbar-nav li {
  margin-right: 1rem;
}

.navbar-nav a {
  padding: 0.25rem 0.5rem;
}

/* Estilos del cuerpo */

main {
  padding: 1rem;
}

.container {
  max-width: 960px;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -0.5rem;
  margin-left: -0.5rem;
}

.col {
  flex: 1 0 auto;
  padding-right: 0.5rem;
  padding-left: 0.5rem;
}

.col-1 {
  width: 8.333333%;
}

.col-2 {
  width: 16.666667%;
}

.col-3 {
  width: 25%;
}

.col-4 {
  width: 33.333333%;
}

.col-5 {
  width: 41.666667%;
}

.col-6 {
  width: 50%;
}

.col-7 {
  width: 58.333333%;
}

.col-8 {
  width: 66.666667%;
}

.col-9 {
  width: 75%;
}

.col-10 {
  width: 83.333333%;
}

.col-11 {
  width: 91.666667%;
}

.col-12 {
  width: 100%;
}

/* Estilos del pie de página */

footer {
  background-color: #f8f9fa;
  padding: 0.5rem 1rem;
}

.footer-copyright {
  text-align: center;
}

/* Estilos responsivos */

@media (max-width: 768px) {
  .navbar-nav {
    flex-direction: column;
    align-items: stretch;
  }

  .navbar-nav li {
    margin-right: 0;
    margin-bottom: 0.5rem;
  }

  .col {
    flex-basis: 100%;
  }
}
```

Este código CSS es un ejemplo de un código complejo y bien organizado. Está dividido en varias secciones, cada una de las cuales se encarga de un aspecto diferente del estilo de la página.

La sección "Estilos generales" define los estilos básicos de la página, como la fuente, el color y el tamaño del texto, así como los estilos de los enlaces y los encabezados.

La sección "Estilos del encabezado" define el estilo del encabezado de la página, incluyendo el color de fondo, el tamaño y el estilo del texto, y el diseño del menú de navegación.

La sección "Estilos del cuerpo" define el estilo del cuerpo de la página, incluyendo el color de fondo, el tamaño y el estilo del texto, y el diseño de las filas y columnas.

La sección "Estilos del pie de página" define el estilo del pie