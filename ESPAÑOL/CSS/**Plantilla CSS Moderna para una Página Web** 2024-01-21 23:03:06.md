```
/* Sección de estilos generales */

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  background-color: #f2f2f2;
}

h1 {
  font-size: 36px;
  color: #333;
}

h2 {
  font-size: 24px;
  color: #333;
}

h3 {
  font-size: 18px;
  color: #333;
}

p {
  font-size: 16px;
  color: #333;
}

/* Sección de estilos para el contenedor principal */

.main-container {
  width: 1200px;
  margin: 0 auto;
}

/* Sección de estilos para el encabezado */

header {
  width: 100%;
  height: 80px;
  background-color: #fff;
}

.logo {
  float: left;
  width: 200px;
  height: 80px;
  background-image: url('logo.png');
  background-size: contain;
}

.menu {
  float: right;
  width: 1000px;
  height: 80px;
}

.menu-item {
  float: left;
  width: 100px;
  height: 80px;
  text-align: center;
  line-height: 80px;
}

.menu-item a {
  color: #333;
  text-decoration: none;
}

/* Sección de estilos para el contenido principal */

.content {
  width: 100%;
  margin-top: 20px;
}

.content-left {
  float: left;
  width: 600px;
}

.content-right {
  float: right;
  width: 600px;
}

/* Sección de estilos para el pie de página */

footer {
  width: 100%;
  height: 200px;
  background-color: #f2f2f2;
}

.copyright {
  float: left;
  width: 600px;
  height: 200px;
  line-height: 200px;
  text-align: center;
}

.social-media {
  float: right;
  width: 600px;
  height: 200px;
}

.social-media-item {
  float: left;
  width: 100px;
  height: 200px;
  text-align: center;
  line-height: 200px;
}

/* Sección de estilos para las animaciones */

.fadeInUp {
  animation: fadeInUp 1s ease-in-out;
}

@keyframes fadeInUp {
  0% {
    opacity: 0;
    transform: translate3d(0, 100px, 0);
  }
  100% {
    opacity: 1;
    transform: translate3d(0, 0, 0);
  }
}

/* Sección de estilos para los medios de comunicación */

@media (max-width: 1200px) {
  .main-container {
    width: 90%;
  }
}

@media (max-width: 992px) {
  .content-left,
  .content-right {
    float: none;
    width: 100%;
  }
}

@media (max-width: 768px) {
  .menu {
    width: 100%;
  }

  .menu-item {
    width: 50%;
  }
}
```

Explicación del código CSS:

* La sección de estilos generales contiene los estilos básicos para todos los elementos de la página, como la fuente, el tamaño de la fuente y los colores.
* La sección de estilos para el contenedor principal contiene los estilos para el contenedor principal de la página, que es un div con un ancho de 1200px y un margen automático centrado.
* La sección de estilos para el encabezado contiene los estilos para el encabezado de la página, que incluye el logotipo y el menú.
* La sección de estilos para el contenido principal contiene los estilos para el contenido principal de la página, que está dividido en dos columnas.
* La sección de estilos para el pie de página contiene los estilos para el pie de página de la página, que incluye el copyright y los enlaces a las redes sociales.
* La sección de estilos para las animaciones contiene los estilos para las animaciones de la página, como el efecto de "fade in" al cargar la página.
* La sección de estilos para los medios de comunicación contiene los estilos para los diferentes tamaños de pantalla, como el ancho máximo del contenedor principal y el ancho de las columnas del contenido principal.