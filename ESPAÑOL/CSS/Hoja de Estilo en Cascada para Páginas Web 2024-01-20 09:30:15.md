```css
/* ESTILOS GENERALES */

/* Restablecer estilos por defecto */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Tipografía base */
body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

/* Encabezados */
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
  font-size: 1.5rem;
}

h4 {
  font-size: 1.2rem;
}

h5 {
  font-size: 1rem;
}

h6 {
  font-size: 0.8rem;
}

/* Párrafos */
p {
  margin-bottom: 1rem;
}

/* Enlaces */
a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

/* LISTAS */

/* Listas sin orden */
ul {
  list-style-type: none;
  padding: 0;
}

/* Elementos de listas sin orden */
li {
  margin-bottom: 0.5rem;
}

/* Listas ordenadas */
ol {
  list-style-type: decimal;
  padding: 0;
}

/* Elementos de listas ordenadas */
li {
  margin-bottom: 0.5rem;
}

/* FORMULARIOS */

/* Campos de texto */
input[type="text"], input[type="password"], input[type="email"], input[type="number"] {
  width: 100%;
  padding: 0.375rem 0.75rem;
  font-size: 1rem;
  line-height: 1.5;
  color: #495057;
  background-color: #fff;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
}

/* Campos de texto enfocados */
input[type="text"]:focus, input[type="password"]:focus, input[type="email"]:focus, input[type="number"]:focus {
  border-color: #80bdff;
  outline: 0;
}

/* Botones */
button {
  padding: 0.375rem 0.75rem;
  font-size: 1rem;
  line-height: 1.5;
  color: #fff;
  background-color: #007bff;
  border: 1px solid #007bff;
  border-radius: 0.25rem;
}

/* Botones enfocados */
button:focus {
  outline: 0;
  box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.25);
}

/* Elementos deshabilitados */
input[disabled], button[disabled] {
  opacity: 0.65;
  cursor: not-allowed;
}

/* TABLAS */

/* Tablas */
table {
  width: 100%;
  border-collapse: collapse;
}

/* Cabeceras de tabla */
th {
  font-weight: bold;
  text-align: left;
}

/* Filas de tabla */
tr {
  border-bottom: 1px solid #ced4da;
}

/* Celdas de tabla */
td {
  padding: 0.5rem;
}

/* RESOLUCIONES DE PANTALLA */

/* Teléfonos móviles */
@media (max-width: 575.98px) {
  /* Estilos específicos para teléfonos móviles */
}

/* Tabletas */
@media (min-width: 576px) and (max-width: 767.98px) {
  /* Estilos específicos para tabletas */
}

/* Escritorios */
@media (min-width: 768px) and (max-width: 991.98px) {
  /* Estilos específicos para escritorios */
}

/* Pantallas grandes */
@media (min-width: 992px) and (max-width: 1199.98px) {
  /* Estilos específicos para pantallas grandes */
}

/* Pantallas muy grandes */
@media (min-width: 1200px) {
  /* Estilos específicos para pantallas muy grandes */
}

/* CLASES Y SELECTORES ESPECÍFICOS */

/* Clase "destacado" */
.destacado {
  background-color: #f5f5f5;
  padding: 1rem;
  margin-bottom: 1rem;
}

/* Clase "error" */
.error {
  color: #d32f2f;
}

/* Clase "correcto" */
.correcto {
  color: #4caf50;
}

/* Selector "#banner" */
#banner {
  background-image: url("banner.jpg");
  background-size: cover;
  background-position: center;
  height: 200px;
}

/* Selector ".formulario" */
.formulario {
  max-width: 500px;
  margin: 0 auto;
}

/* EXPLICACIÓN DEL CÓDIGO */

/* ESTILOS GENERALES */

/* Restablecer estilos por defecto */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

Esta línea de código restablece los estilos por defecto de todos los elementos de la página, eliminando cualquier margen, relleno y tamaño de caja predeterminado. Esto nos permite tener un control total sobre el diseño de la página.

/* Tipografía base */
body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

Esta línea de código define la fuente base, el tamaño de fuente y el interlineado para todos los elementos de la página.

/* Encabezados */
h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

Esta línea de código hace que todos los encabezados de la página sean en negrita.

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.5rem;
}

h4 {
  font-size: 1.2rem;
}

h5 {
  font-size: 1rem;
}

h6 {
  font-size: 0.8rem;
}

Estas líneas de código definen el tamaño de fuente para cada nivel de encabezado.

/* Párrafos */
p {
  margin-bottom: 1rem;
}

Esta línea de código añade un margen inferior de 1 rem a todos los párrafos.

/* Enlaces */
a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

Estas líneas de código definen el color y la decoración de los enlaces.

/* LISTAS */

/* Listas sin orden */
ul {
  list-style-type: none;
  padding: 0;
}

/* Elementos de listas sin orden */
li {
  margin-bottom: 0.5rem;
}

Estas líneas de código eliminan las viñetas de las listas sin orden y añaden un margen inferior de 0.5 rem a cada elemento de la lista.

/* Listas ordenadas */
ol {
  list-style-type: decimal;
  padding: 0;
}

/* Elementos de listas ordenadas */
li {
  margin-bottom: 0.5rem;
}

Estas líneas de código definen el tipo de lista y eliminan el margen izquierdo de las listas ordenadas.

/* FORMULARIOS */

/* Campos de texto */
input[type="text"], input[type="password"], input[type="email"], input[type="number"] {
  width: 100%;
  padding: 0.375rem 0.75rem;
  font-size: 1rem;
  line-height: 1.5;
  color: #495057;
  background-color: #fff;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
}

Estas líneas de código definen los estilos para los campos