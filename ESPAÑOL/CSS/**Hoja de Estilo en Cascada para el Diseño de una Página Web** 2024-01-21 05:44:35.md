```css
/* Estilos generales */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  font-family: Arial, sans-serif;
}

body {
  background-color: #f5f5f5;
  line-height: 1.6rem;
}

/* Encabezado */

#header {
  background-color: #333;
  color: #fff;
  padding: 1rem;
}

#header h1 {
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
}

#header nav {
  float: right;
}

#header nav ul {
  list-style-type: none;
  display: flex;
}

#header nav li {
  margin-right: 1rem;
}

#header nav a {
  color: #fff;
  text-decoration: none;
}

#header nav a:hover {
  color: #ddd;
}

/* Contenido principal */

#main {
  padding: 1rem;
}

#main h2 {
  font-size: 2rem;
  margin-bottom: 1rem;
}

#main p {
  font-size: 1.6rem;
  margin-bottom: 1rem;
}

#main ul {
  list-style-type: disc;
  padding-inline-start: 1rem;
}

#main li {
  margin-bottom: 0.5rem;
}

/* Pie de página */

#footer {
  background-color: #333;
  color: #fff;
  padding: 1rem;
  text-align: center;
}

#footer p {
  font-size: 1.2rem;
  margin-bottom: 0;
}

/* Estilos adicionales */

.destacado {
  background-color: #ff0000;
  color: #fff;
  padding: 0.5rem;
  margin-bottom: 1rem;
}

.oculto {
  display: none;
}

/* Estilos para medios específicos */

@media screen and (max-width: 768px) {
  #header nav {
    float: none;
  }

  #header nav ul {
    flex-direction: column;
  }

  #header nav li {
    margin-bottom: 0.5rem;
  }
}
```

Explicación del código:

* **Estilos generales:** Se definen los estilos generales para todos los elementos de la página, como la fuente, el tamaño de la letra, el espaciado y los márgenes.
* **Encabezado:** Se define el estilo del encabezado de la página, que incluye el logotipo, el título y el menú de navegación.
* **Contenido principal:** Se define el estilo del contenido principal de la página, que incluye los títulos, los párrafos, las listas y otros elementos.
* **Pie de página:** Se define el estilo del pie de página de la página, que incluye el texto de copyright y otros datos de contacto.
* **Estilos adicionales:** Se definen algunos estilos adicionales que se pueden utilizar en cualquier parte de la página, como la clase "destacado" y la clase "oculto".
* **Estilos para medios específicos:** Se definen algunos estilos específicos para medios específicos, como la pantalla de un teléfono móvil.