```
/* Estilos globales */

/* Fuentes */
@font-face {
  font-family: 'MiFuente';
  src: url('mif fuente.ttf');
}

/* Colores */
:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-fondo: #f5f5f5;
}

/* Tipografía */
body {
  font-family: 'MiFuente', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

/* Enlaces */
a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Cabecera */

header {
  background-color: var(--color-fondo);
  padding: 1em;
}

header h1 {
  font-size: 24px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
}

header nav li {
  display: inline-block;
  margin-right: 1em;
}

/* Contenido */

main {
  margin-top: 1em;
}

section {
  margin-bottom: 1em;
}

.columna {
  float: left;
  width: 50%;
}

.columna:last-child {
  margin-right: 0;
}

.clearfix {
  clear: both;
}

/* Pie de página */

footer {
  background-color: var(--color-fondo);
  padding: 1em;
}

footer p {
  text-align: center;
}

/* Estilos adicionales */

.imagen {
  width: 100%;
}

.enlace-externo {
  color: var(--color-secundario);
  text-decoration: underline;
}

.boton {
  background-color: var(--color-primario);
  color: white;
  padding: 0.5em 1em;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

.boton:hover {
  background-color: var(--color-secundario);
}

.tabla {
  border-collapse: collapse;
  width: 100%;
}

.tabla td, .tabla th {
  border: 1px solid #ddd;
  padding: 0.5em;
}

.tabla th {
  background-color: var(--color-fondo);
}
```

**Explicación:**

* **Fuentes:** Se carga una fuente personalizada llamada "MiFuente" y se establece como fuente predeterminada para todo el sitio.
* **Colores:** Se definen variables de color para los colores primarios y secundarios, así como para el color de fondo.
* **Tipografía:** Se establecen estilos generales de tipografía, como la fuente, el tamaño de fuente y el interlineado.
* **Enlaces:** Se ajustan los estilos de los enlaces para que sean de color azul y no estén subrayados por defecto.
* **Cabecera:** Se define la cabecera del sitio, que incluye un logotipo, un menú de navegación y un buscador.
* **Contenido:** Se define el área de contenido principal, que incluye secciones para el texto, las imágenes y otros elementos.
* **Pie de página:** Se define el pie de página del sitio, que incluye información de copyright y enlaces a otras páginas.
* **Estilos adicionales:** Se definen estilos adicionales para elementos comunes, como imágenes, enlaces externos, botones y tablas.

Este es un código CSS completo que se puede utilizar para crear un sitio web básico. Es completamente personalizable, por lo que puede modificarlo para que se ajuste a sus necesidades y preferencias específicas.