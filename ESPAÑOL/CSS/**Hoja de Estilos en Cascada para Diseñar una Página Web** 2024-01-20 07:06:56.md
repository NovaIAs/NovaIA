```css
/* Paleta de colores */

:root {
  --color-primario: #FF0000;
  --color-secundario: #00FF00;
  --color-terciario: #0000FF;
  --color-fondo: #FFFFFF;
  --color-texto: #000000;
}

/* Estilos generales */

* {
  font-family: Arial, sans-serif;
}

body {
  background-color: var(--color-fondo);
  color: var(--color-texto);
}

h1 {
  font-size: 2em;
  font-weight: bold;
  color: var(--color-primario);
}

h2 {
  font-size: 1.5em;
  font-weight: bold;
  color: var(--color-secundario);
}

h3 {
  font-size: 1.2em;
  font-weight: bold;
  color: var(--color-terciario);
}

p {
  font-size: 1em;
  line-height: 1.5em;
}

/* Estilos para elementos específicos */

.boton {
  background-color: var(--color-primario);
  color: var(--color-texto);
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

.boton:hover {
  background-color: var(--color-secundario);
}

.enlace {
  color: var(--color-primario);
  text-decoration: none;
}

.enlace:hover {
  text-decoration: underline;
}

/* Estilos para el diseño de la página */

#cabecera {
  background-color: var(--color-primario);
  color: var(--color-texto);
  padding: 20px;
}

#cabecera h1 {
  font-size: 3em;
}

#cabecera nav {
  float: right;
}

#cabecera nav ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

#cabecera nav ul li {
  display: inline-block;
  margin-right: 10px;
}

#cabecera nav ul li a {
  color: var(--color-texto);
  text-decoration: none;
}

#cabecera nav ul li a:hover {
  text-decoration: underline;
}

#contenido {
  padding: 20px;
}

#pie {
  background-color: var(--color-primario);
  color: var(--color-texto);
  padding: 20px;
  text-align: center;
}
```

Este código CSS define una paleta de colores, estilos generales, estilos para elementos específicos y estilos para el diseño de la página.

**La paleta de colores** define los colores principales que se usarán en la página. En este caso, se han definido tres colores: rojo, verde y azul.

**Los estilos generales** se aplican a todos los elementos de la página. En este caso, se ha definido la fuente, el tamaño de fuente y el color de fuente para todos los elementos.

**Los estilos para elementos específicos** se aplican a elementos específicos, como botones, enlaces y encabezados. En este caso, se han definido estilos para botones, enlaces y encabezados de nivel 1, 2 y 3.

**Los estilos para el diseño de la página** se aplican al diseño de la página. En este caso, se han definido estilos para el encabezado, el contenido y el pie de página.

Este código CSS es un ejemplo de cómo se puede usar CSS para crear una página web con un diseño atractivo y fácil de usar.