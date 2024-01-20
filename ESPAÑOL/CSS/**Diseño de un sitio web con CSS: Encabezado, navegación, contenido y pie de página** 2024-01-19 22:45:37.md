```css
/* Definir la fuente y los colores */
@font-face {
  font-family: "MiFuente";
  src: url("fuentes/MiFuente.ttf");
  font-weight: normal;
  font-style: normal;
}

body {
  font-family: "MiFuente", Arial, sans-serif;
  font-size: 16px;
  color: #333;
  background-color: #fff;
  margin: 0;
  padding: 0;
}

/* Definir el encabezado */
header {
  background-color: #007bff;
  color: #fff;
  padding: 10px;
  margin: 0;
}

h1 {
  font-size: 24px;
  font-weight: bold;
  margin: 0;
}

/* Definir la barra de navegación */
nav {
  background-color: #eee;
  padding: 10px;
  margin: 0;
}

ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

li {
  display: inline-block;
  margin: 0 10px;
  font-weight: bold;
}

a {
  text-decoration: none;
  color: #333;
}

/* Definir el contenido */
main {
  margin: 0;
  padding: 10px;
}

article {
  margin-bottom: 20px;
  padding: 10px;
  border: 1px solid #eee;
  background-color: #ffffff;
}

h2 {
  font-size: 18px;
  margin: 0;
}

p {
  margin: 0;
}

/* Definir el pie de página */
footer {
  background-color: #007bff;
  color: #fff;
  padding: 10px;
  margin: 0;
}

/* Definir los medios */
@media (max-width: 768px) {
  nav ul {
    display: block;
    margin: 0;
    padding: 0;
  }

  li {
    display: block;
    margin: 10px 0;
  }
}
```

Explicación:

* La primera parte del código define la fuente y los colores que se utilizarán en el sitio web.
* La segunda parte define los estilos para el encabezado, la barra de navegación, el contenido y el pie de página.
* La tercera parte del código define los estilos para los medios, es decir, para diferentes tamaños de pantalla.

El código es complejo pero bien organizado y fácil de leer. Utiliza una variedad de selectores y propiedades CSS para crear un sitio web atractivo y funcional.