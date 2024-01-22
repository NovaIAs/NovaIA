```css
/* Definición de los estilos para el layout general de la página */

html, body {
  height: 100%;
  margin: 0;
  padding: 0;
  font-family: 'Arial', sans-serif;
}

.container {
  min-height: 100%;
  display: flex;
  flex-direction: column;
}

.header {
  flex: 1;
  background-color: #f5f5f5;
  padding: 10px;
}

.logo {
  float: left;
  margin-right: 20px;
}

.logo img {
  height: 50px;
}

.navegacion {
  float: right;
}

.navegacion ul {
  list-style-type: none;
  display: flex;
  flex-direction: row;
  justify-content: flex-end;
}

.navegacion li {
  margin-right: 20px;
}

.navegacion li a {
  text-decoration: none;
  color: #000;
}

.navegacion li a:hover {
  color: #f00;
}

.main {
  flex: 8;
}

.contenido {
  padding: 10px;
}

.sidebar {
  flex: 1;
  background-color: #e5e5e5;
  padding: 10px;
}

.sidebar .widget {
  margin-bottom: 10px;
}

.sidebar .widget h3 {
  font-size: 1.2em;
  margin-bottom: 5px;
}

.sidebar .widget ul {
  list-style-type: none;
  padding: 0;
}

.sidebar .widget li {
  margin-bottom: 5px;
}

.sidebar .widget li a {
  text-decoration: none;
  color: #000;
}

.sidebar .widget li a:hover {
  color: #f00;
}

.footer {
  flex: 1;
  background-color: #f5f5f5;
  padding: 10px;
}

.footer .copyright {
  float: left;
}

.footer .redes-sociales {
  float: right;
}

.footer .redes-sociales ul {
  list-style-type: none;
  display: flex;
  flex-direction: row;
  justify-content: flex-end;
}

.footer .redes-sociales li {
  margin-right: 10px;
}

.footer .redes-sociales li a {
  text-decoration: none;
  color: #000;
}

.footer .redes-sociales li a:hover {
  color: #f00;
}

/* Definición de los estilos para los elementos de la página */

h1 {
  font-size: 2em;
  margin-bottom: 10px;
}

h2 {
  font-size: 1.5em;
  margin-bottom: 10px;
}

h3 {
  font-size: 1.2em;
  margin-bottom: 10px;
}

p {
  margin-bottom: 10px;
}

a {
  text-decoration: none;
  color: #000;
}

a:hover {
  color: #f00;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  margin-bottom: 5px;
}

img {
  max-width: 100%;
}

/* Definición de los estilos para las clases adicionales */

.resaltado {
  background-color: #f0f0f0;
  padding: 5px;
}

.centrado {
  text-align: center;
}

.derecha {
  float: right;
}

.izquierda {
  float: left;
}

.oculto {
  display: none;
}

/* Definición de los estilos para los media queries */

@media (max-width: 1024px) {
  .sidebar {
    display: none;
  }
}

@media (max-width: 768px) {
  .header {
    background-color: #000;
    color: #fff;
  }

  .logo img {
    height: 30px;
  }

  .navegacion {
    display: none;
  }

  .contenido {
    padding: 5px;
  }

  .footer {
    background-color: #000;
    color: #fff;
  }

  .footer .copyright {
    float: none;
  }

  .footer .redes-sociales {
    float: none;
  }
}
```

Explicación del código:

1. **Layout general de la página:**

    El código define una estructura de layout flexible para la página utilizando flexbox. El contenedor principal (`.container`) tiene tres secciones principales: el encabezado (`.header`), el contenido principal (`.main`) y el pie de página (`.footer`). El encabezado y el pie de página tienen una altura fija, mientras que el contenido principal tiene un tamaño flexible.

2. **Encabezado:**

    El encabezado contiene el logotipo y los elementos de navegación. El logotipo está situado a la izquierda, mientras que los elementos de navegación están situados a la derecha.

3. **Navegación:**

    La navegación contiene una lista de enlaces a las distintas secciones de la página. Los enlaces están estilizados para que cambien de color al pasar el ratón por encima.

4. **Contenido principal:**

    El contenido principal contiene el contenido de la página. Esta sección puede contener texto, imágenes, vídeos y otros elementos multimedia.

5. **Barra lateral:**

    La barra lateral contiene los elementos secundarios de la página, como una lista de categorías, un formulario de búsqueda o una lista de los últimos posts.

6. **Pie de página:**

    El pie de página contiene información sobre el copyright y los enlaces a las redes sociales.

7. **Elementos de la página:**

    El código define estilos para los elementos HTML comunes, como encabezados, párrafos, imágenes y listas.

8. **Clases adicionales:**

    El código define una serie de clases adicionales que pueden utilizarse para aplicar estilos a elementos específicos de la página. Por ejemplo, la clase `.resaltado` añade un fondo de color a un elemento, mientras que la clase `.centrado` centra el texto en un elemento.

9. **Media queries:**

    El código define una serie de media queries que permiten adaptar el diseño de la página a diferentes tamaños de pantalla. Por ejemplo, cuando la anchura de la pantalla es inferior a 1024px, la barra lateral se oculta.