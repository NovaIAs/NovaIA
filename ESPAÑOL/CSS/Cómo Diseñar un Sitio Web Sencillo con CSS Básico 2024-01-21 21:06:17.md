```css
/* SELECCIÓN DE ELEMENTOS */

* { /* Selección de todos los elementos en la página */
  margin: 0; /* Borde superior e izquierdo en 0 */
  padding: 0; /* Relleno superior e izquierdo en 0 */
  box-sizing: border-box; /* Incluir el ancho del borde en el ancho del elemento */
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif; /* Definir la fuente por defecto */
  background-color: #f5f5f5; /* Establecer el color de fondo */
}

/* ENCABEZADO */

header {
  width: 100%; /* Ancho completo de la página */
  height: 64px; /* Altura de 64 píxeles */
  background-color: #333; /* Color de fondo */
}

header h1 {
  color: #fff; /* Color del texto */
  font-size: 24px; /* Tamaño de la fuente */
  margin: 0 24px; /* Márgenes superior e inferior en 0, margen izquierdo y derecho en 24 píxeles */
  line-height: 64px; /* Altura de línea igual a la altura del encabezado */
}

/* MENÚ DE NAVEGACIÓN */

nav {
  width: 100%; /* Ancho completo de la página */
  height: 48px; /* Altura de 48 píxeles */
  background-color: #444; /* Color de fondo */
}

nav ul {
  list-style-type: none; /* Quitar el estilo de lista por defecto */
  display: flex; /* Disposición en fila */
  justify-content: space-between; /* Distribuir los elementos uniformemente en toda la fila */
  align-items: center; /* Alinear los elementos verticalmente en el centro */
}

nav li {
  display: inline-block; /* Hacer que cada elemento de lista sea un bloque en línea */
  margin: 0 12px; /* Márgenes superior e inferior en 0, margen izquierdo y derecho en 12 píxeles */
}

nav a {
  color: #fff; /* Color del texto */
  font-size: 16px; /* Tamaño de la fuente */
  text-decoration: none; /* Quitar el subrayado del enlace */
}

nav a:hover {
  color: #999; /* Color del texto al pasar el ratón */
}

/* SECCIÓN PRINCIPAL */

main {
  margin-top: 64px; /* Margen superior de 64 píxeles para compensar el encabezado */
}

/* SECCIÓN "ACERCA DE" */

section.about {
  background-color: #fff; /* Color de fondo */
  padding: 32px; /* Relleno superior, inferior, izquierdo y derecho en 32 píxeles */
}

section.about h2 {
  color: #333; /* Color del texto */
  font-size: 24px; /* Tamaño de la fuente */
  margin-bottom: 16px; /* Margen inferior de 16 píxeles */
}

section.about p {
  color: #666; /* Color del texto */
  font-size: 16px; /* Tamaño de la fuente */
  line-height: 1.5; /* Aumentar el espacio entre líneas */
}

/* SECCIÓN "SERVICIOS" */

section.services {
  background-color: #f5f5f5; /* Color de fondo */
  padding: 32px; /* Relleno superior, inferior, izquierdo y derecho en 32 píxeles */
}

section.services h2 {
  color: #333; /* Color del texto */
  font-size: 24px; /* Tamaño de la fuente */
  margin-bottom: 16px; /* Margen inferior de 16 píxeles */
}

section.services ul {
  list-style-type: none; /* Quitar el estilo de lista por defecto */
  display: flex; /* Disposición en fila */
  flex-wrap: wrap; /* Permitir que los elementos se envuelvan en varias filas */
  justify-content: space-between; /* Distribuir los elementos uniformemente en toda la fila */
}

section.services li {
  width: 25%; /* Ancho de cada elemento de lista */
  margin: 16px 0; /* Márgenes superior e inferior de 16 píxeles */
}

section.services li h3 {
  color: #333; /* Color del texto */
  font-size: 20px; /* Tamaño de la fuente */
  margin-bottom: 8px; /* Margen inferior de 8 píxeles */
}

section.services li p {
  color: #666; /* Color del texto */
  font-size: 16px; /* Tamaño de la fuente */
  line-height: 1.5; /* Aumentar el espacio entre líneas */
}

/* SECCIÓN "CONTACTO" */

section.contact {
  background-color: #333; /* Color de fondo */
  padding: 32px; /* Relleno superior, inferior, izquierdo y derecho en 32 píxeles */
}

section.contact h2 {
  color: #fff; /* Color del texto */
  font-size: 24px; /* Tamaño de la fuente */
  margin-bottom: 16px; /* Margen inferior de 16 píxeles */
}

section.contact form {
  display: flex; /* Disposición en fila */
  flex-direction: column; /* Dirección de la fila en vertical */
}

section.contact input,
section.contact textarea {
  width: 100%; /* Ancho completo del elemento */
  padding: 12px; /* Relleno superior, inferior, izquierdo y derecho en 12 píxeles */
  margin-bottom: 8px; /* Margen inferior de 8 píxeles */
  border: 1px solid #ccc; /* Borde de 1 píxel de ancho, color #ccc */
}

section.contact textarea {
  height: 200px; /* Altura del elemento */
  resize: none; /* Deshabilitar el cambio de tamaño del elemento */
}

section.contact button {
  background-color: #444; /* Color de fondo */
  color: #fff; /* Color del texto */
  padding: 12px 24px; /* Relleno superior, inferior, izquierdo y derecho */
  margin-top: 16px; /* Margen superior de 16 píxeles */
  border: none; /* Sin borde */
  cursor: pointer; /* Hacer que el puntero del ratón sea una mano cuando se pasa sobre el elemento */
}

section.contact button:hover {
  background-color: #666; /* Color de fondo al pasar el ratón */
}

/* PIE DE PÁGINA */

footer {
  width: 100%; /* Ancho completo de la página */
  height: 64px; /* Altura de 64 píxeles */
  background-color: #333; /* Color de fondo */
}

footer p {
  color: #fff; /* Color del texto */
  font-size: 16px; /* Tamaño de la fuente */
  line-height: 64px; /* Altura de línea igual a la altura del pie de página */
  text-align: center; /* Alinear el texto en el centro */
}

/* MEDIOS DE RESPUESTA */

@media screen and (max-width: 768px) {
  /* Ajustes para pantallas con un ancho máximo de 768 píxeles */
  section.about,
  section.services,
  section.contact {
    padding: 16px; /* Relleno superior, inferior, izquierdo y derecho en 16 píxeles */
  }

  nav {
    height: 32px; /* Altura de 32 píxeles */
  }

  nav ul {
    height: 32px; /* Altura de 32 píxeles */
  }

  nav li {
    margin: 0 6px; /* Márgenes superior e inferior en 0, margen izquierdo y derecho en 6 píxeles */
  }

  nav a {
    font-size: 14px; /* Tamaño de la fuente */
  }

  section.services ul {
    flex-direction: column; /* Dirección de la fila en vertical */
  }

  section.services li {
    width: 100%; /* Ancho completo del elemento */
    margin: 16px 0; /* Márgenes superior e inferior de 16 píxeles */
  }
}
*/

/* EXPLICACIÓN DEL CÓDIGO */

- Se utilizan unidades relativas (porcentajes y ems) en lugar de unidades absolutas (píxeles) para permitir que el diseño se adapte a diferentes tamaños de pantalla.
- Se utilizan colores neutros y una fuente sans-serif para crear un diseño limpio y legible.
- Se utilizan medios de respuesta para ajustar el diseño a diferentes tamaños de pantalla.
- Se utiliza Flexbox para crear diseños flexibles y responsivos.
- Se utilizan los principios de DRY (Don't Repeat Yourself) y KISS (Keep It Simple Stupid) para mantener el código limpio y fácil de mantener.