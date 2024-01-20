```css
/*
  Código CSS complejo para crear una interfaz de usuario atractiva y funcional.
*/

/* Colores */

:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-terciario: #fff;
  --color-fondo: #f8f9fa;
}

/* Fuentes */

@font-face {
  font-family: 'Roboto Condensed', sans-serif;
  src: url('RobotoCondensed-Regular.ttf') format('truetype');
}

/* Estilos generales */

body {
  font-family: 'Roboto Condensed', sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: var(--color-secundario);
  background-color: var(--color-fondo);
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
}

a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  color: var(--color-secundario);
  text-decoration: underline;
}

/* Encabezado */

header {
  background-color: var(--color-primario);
  color: var(--color-terciario);
  padding: 20px;
}

header h1 {
  font-size: 24px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

header nav ul li {
  display: inline-block;
  margin-right: 20px;
}

header nav ul li a {
  color: var(--color-terciario);
}

/* Contenido principal */

main {
  padding: 20px;
}

main h2 {
  font-size: 20px;
}

main p {
  margin-bottom: 20px;
}

/* Barra lateral */

aside {
  float: right;
  width: 300px;
  padding: 20px;
  background-color: var(--color-fondo);
}

aside h3 {
  font-size: 18px;
}

aside ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

aside ul li {
  margin-bottom: 10px;
}

aside ul li a {
  color: var(--color-secundario);
}

/* Pie de página */

footer {
  background-color: var(--color-primario);
  color: var(--color-terciario);
  padding: 20px;
}

footer p {
  text-align: center;
}

/* Medios responsive */

@media (max-width: 768px) {
  aside {
    float: none;
    width: 100%;
  }
}

/* Estilos adicionales */

.error {
  color: #d32f2f;
}

.success {
  color: #4caf50;
}

.warning {
  color: #ffc107;
}

.info {
  color: #007bff;
}
```

Explicación del código:

* El código define una serie de variables CSS personalizadas para los colores, las fuentes y los tamaños de fuente.
* Los estilos generales definen la apariencia general del documento, incluyendo la familia de fuentes, el tamaño de fuente, el color del texto y el color de fondo.
* El encabezado define el estilo del encabezado de la página, incluyendo el color de fondo, el color del texto, el tamaño de la fuente y el estilo del menú de navegación.
* El contenido principal define el estilo del contenido principal de la página, incluyendo el tamaño de la fuente y el espaciado entre párrafos.
* La barra lateral define el estilo de la barra lateral de la página, incluyendo el color de fondo, el tamaño de la fuente y el estilo de la lista de elementos.
* El pie de página define el estilo del pie de página de la página, incluyendo el color de fondo, el color del texto y el tamaño de la fuente.
* Los medios responsive definen el estilo de la página para diferentes tamaños de pantalla.
* Los estilos adicionales definen el estilo de los mensajes de error, éxito, aviso e información.