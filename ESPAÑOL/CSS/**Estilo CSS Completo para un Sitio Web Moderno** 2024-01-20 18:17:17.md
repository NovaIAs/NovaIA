```css
/* Definición de variables de color */
:root {
  --azul-oscuro: #000080;
  --azul-claro: #add8e6;
  --verde-oscuro: #006400;
  --verde-claro: #98fb98;
  --amarillo-oscuro: #ffa500;
  --amarillo-claro: #fffacd;
  --rojo-oscuro: #8b0000;
  --rojo-claro: #ffc0cb;
}

/* Estilo general del body */
body {
  font-family: Arial, Helvetica, sans-serif;
  background-color: var(--azul-claro);
  color: var(--azul-oscuro);
}

/* Encabezado principal */
header {
  text-align: center;
  background-color: var(--azul-oscuro);
  color: var(--blanco);
  padding: 1em;
}

/* Enlaces de navegación */
nav ul {
  list-style-type: none;
  display: flex;
  justify-content: center;
  padding: 0;
  margin: 0;
}

nav li {
  display: inline-block;
  margin-right: 1em;
}

nav a {
  text-decoration: none;
  color: var(--azul-oscuro);
}

/* Sección principal */
main {
  padding: 1em;
  margin: 0 auto;
  max-width: 600px;
}

/* Subtítulos */
h2 {
  font-size: 1.5em;
  margin-bottom: 0.5em;
}

/* Párrafos */
p {
  text-align: justify;
}

/* Elementos de lista */
ul {
  list-style-type: disc;
  padding: 0;
  margin: 0;
}

li {
  margin-bottom: 0.5em;
}

/* Citas */
blockquote {
  background-color: var(--verde-claro);
  padding: 0.5em;
  margin: 0.5em 0;
}

cite {
  font-style: italic;
}

/* Imágenes */
img {
  max-width: 100%;
  height: auto;
  margin: 0 auto;
}

/* Elementos de formulario */
form {
  width: 100%;
}

input[type="text"],
input[type="email"],
input[type="password"] {
  width: 100%;
  padding: 0.5em;
  margin-bottom: 0.5em;
  border: 1px solid var(--azul-oscuro);
}

textarea {
  width: 100%;
  height: 10em;
  padding: 0.5em;
  margin-bottom: 0.5em;
  border: 1px solid var(--azul-oscuro);
}

button {
  background-color: var(--azul-oscuro);
  color: var(--blanco);
  padding: 0.5em 1em;
  border: 1px solid var(--azul-oscuro);
  cursor: pointer;
}

/* Pie de página */
footer {
  text-align: center;
  background-color: var(--azul-oscuro);
  color: var(--blanco);
  padding: 1em;
}
```

Explicación del código:

* **Variables de color:** Se definen las variables de color que se utilizarán en todo el documento.
* **Estilo general del body:** Se establece la fuente, el color de fondo y el color del texto para todo el documento.
* **Encabezado principal:** Se establece el estilo del encabezado principal, incluyendo el color de fondo, el color del texto y el relleno.
* **Enlaces de navegación:** Se establece el estilo de los enlaces de navegación, incluyendo el estilo de la lista, el espaciado entre los enlaces y el estilo de los enlaces individuales.
* **Sección principal:** Se establece el estilo de la sección principal, incluyendo el relleno, el margen y el ancho máximo.
* **Subtítulos:** Se establece el estilo de los subtítulos, incluyendo el tamaño de la fuente y el margen inferior.
* **Párrafos:** Se establece el estilo de los párrafos, incluyendo la alineación del texto.
* **Elementos de lista:** Se establece el estilo de los elementos de lista, incluyendo el tipo de lista, el relleno y el margen.
* **Citas:** Se establece el estilo de las citas, incluyendo el color de fondo, el relleno y el margen.
* **Imágenes:** Se establece el estilo de las imágenes, incluyendo el ancho máximo y la altura automática.
* **Elementos de formulario:** Se establece el estilo de los elementos de formulario, incluyendo el ancho, el relleno, el margen y el borde.
* **Pie de página:** Se establece el estilo del pie de página, incluyendo el color de fondo, el color del texto y el relleno.