```css
/* ESTILO DE ELEMENTOS HTML */

html {
  font-size: 62.5%;  /* Establece el tamaño de fuente base para el documento. */
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; /* Especifica la pila de fuentes para el documento. */
}

body {
  margin: 0;  /* Anula los márgenes predeterminados del cuerpo. */
  padding: 0;  /* Anula el relleno predeterminado del cuerpo. */
  color: #333;  /* Establece el color del texto del cuerpo. */
  line-height: 1.6;  /* Establece el interlineado del cuerpo. */
}

/* ESTILO DE ENCABEZADOS */

h1 {
  font-size: 2.5rem;  /* Establece el tamaño de fuente para los encabezados de nivel 1. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 1. */
}

h2 {
  font-size: 2rem;  /* Establece el tamaño de fuente para los encabezados de nivel 2. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 2. */
}

h3 {
  font-size: 1.5rem;  /* Establece el tamaño de fuente para los encabezados de nivel 3. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 3. */
}

h4 {
  font-size: 1.2rem;  /* Establece el tamaño de fuente para los encabezados de nivel 4. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 4. */
}

h5 {
  font-size: 1rem;  /* Establece el tamaño de fuente para los encabezados de nivel 5. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 5. */
}

h6 {
  font-size: 0.8rem;  /* Establece el tamaño de fuente para los encabezados de nivel 6. */
  font-weight: bold;  /* Establece el peso de la fuente para los encabezados de nivel 6. */
}

/* ESTILO DE PÁRRAFOS */

p {
  margin-bottom: 1.2rem;  /* Establece el margen inferior para los párrafos. */
}

/* ESTILO DE LISTAS */

ul {
  list-style-type: none;  /* Elimina el estilo de marcador predeterminado para las listas. */
  padding: 0;  /* Anula el relleno predeterminado de las listas. */
}

li {
  margin-bottom: 0.5rem;  /* Establece el margen inferior para los elementos de la lista. */
  line-height: 1.6;  /* Establece el interlineado para los elementos de la lista. */
}

/* ESTILO DE ENLACES */

a {
  color: #007bff;  /* Establece el color del texto de los enlaces. */
  text-decoration: none;  /* Elimina la decoración de texto predeterminada de los enlaces. */

  &:hover {  /* Aplica estilos cuando se desplaza sobre un enlace. */
    color: #0056b3;  /* Cambia el color del texto del enlace al pasar el ratón. */
  }
}

/* ESTILO DE FORMULARIOS */

input {
  padding: 0.5rem;  /* Establece el relleno para los campos de entrada. */
  border: 1px solid #ccc;  /* Establece el borde para los campos de entrada. */
  border-radius: 0.3rem;  /* Añade esquinas redondeadas a los campos de entrada. */
}

button {
  padding: 0.5rem;  /* Establece el relleno para los botones. */
  border: 1px solid #ccc;  /* Establece el borde para los botones. */
  border-radius: 0.3rem;  /* Añade esquinas redondeadas a los botones. */
  background-color: #007bff;  /* Establece el color de fondo para los botones. */
  color: #fff;  /* Establece el color del texto para los botones. */

  &:hover {  /* Aplica estilos cuando se desplaza sobre un botón. */
    background-color: #0056b3;  /* Cambia el color de fondo del botón al pasar el ratón. */
  }
}

/* ESTILO DE CUADROS */

.box {
  padding: 1rem;  /* Establece el relleno para los cuadros. */
  border: 1px solid #ccc;  /* Establece el borde para los cuadros. */
  border-radius: 0.3rem;  /* Añade esquinas redondeadas a los cuadros. */
  background-color: #f5f5f5;  /* Establece el color de fondo para los cuadros. */
}

/* ESTILO DE TABLAS */

table {
  width: 100%;  /* Establece el ancho de la tabla. */
  border-collapse: collapse;  /* Fusiona las celdas adyacentes con el mismo borde. */
}

th {
  padding: 0.5rem;  /* Establece el relleno para las celdas del encabezado. */
  border: 1px solid #ccc;  /* Establece el borde para las celdas del encabezado. */
  background-color: #f5f5f5;  /* Establece el color de fondo para las celdas del encabezado. */
}

td {
  padding: 0.5rem;  /* Establece el relleno para las celdas de datos. */
  border: 1px solid #ccc;  /* Establece el borde para las celdas de datos. */
}

/* ESTILO DE RESALTADORES DE SINTAXIS */

.hljs {
  display: block;  /* Hace que el resaltador de sintaxis se muestre como un bloque. */
  overflow-x: auto;  /* Habilita el desplazamiento horizontal dentro del resaltador de sintaxis. */
  background-color: #f8f8f8;  /* Establece el color de fondo para el resaltador de sintaxis. */
}

.hljs-comment {
  color: #666;  /* Establece el color del texto para los comentarios. */
}

.hljs-keyword {
  color: #008000;  /* Establece el color del texto para las palabras clave. */
}

.hljs-string {
  color: #c62828;  /* Establece el color del texto para las cadenas. */
}

.hljs-number {
  color: #0000ff;  /* Establece el color del texto para los números. */
}

/* ESTILO DE MEDIOS */

@media (max-width: 768px) {
  /* Aplica estilos cuando el ancho de la ventana es menor o igual a 768px. */

  .container {
    padding: 0 1rem;  /* Establece el acolchado para el contenedor cuando el ancho de la ventana es menor o igual a 768px. */
  }

  .sidebar {
    display: none;  /* Oculta el menú lateral cuando el ancho de la ventana es menor o igual a 768px. */
  }

  .main {
    width: 100%;  /* Establece el ancho del contenido principal al 100% cuando el ancho de la ventana es menor o igual a 768px. */
  }
}
```

Este código CSS es muy complejo y contiene una variedad de estilos para diferentes elementos HTML. También incluye estilos para diferentes tamaños de pantalla, utilizando la regla de medios @media. Este código es útil para crear sitios web receptivos que se vean bien en diferentes dispositivos.