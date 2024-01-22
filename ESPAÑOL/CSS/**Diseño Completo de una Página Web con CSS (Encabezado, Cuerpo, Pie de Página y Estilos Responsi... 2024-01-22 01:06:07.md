```CSS
/* Configuración básica */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  font-family: 'Arial', sans-serif;
}

/* Encabezado */

header {
  background-color: #000;
  color: #fff;
  height: 80px;
}

#logo {
  float: left;
  margin: 10px;
  font-size: 2em;
}

.menu {
  float: right;
  margin-right: 10px;
}

.menu li {
  display: inline-block;
  margin-right: 20px;
  font-size: 1.2em;
}

.menu li a {
  color: #fff;
  text-decoration: none;
}

.menu li a:hover {
  color: #f00;
}

/* Cuerpo */

main {
  margin-top: 80px;
}

.container {
  max-width: 1024px;
  margin: 0 auto;
}

.flex-container {
  display: flex;
  flex-wrap: wrap;
}

.flex-item {
  flex: 1 0 auto;
  margin: 10px;
}

/* Pie de página */

footer {
  background-color: #ccc;
  color: #000;
  height: 50px;
  text-align: center;
  font-size: 1.2em;
}

/* Estilos específicos para dispositivos móviles */

@media (max-width: 768px) {
  header {
    height: 60px;
  }

  #logo {
    font-size: 1.5em;
  }

  .menu {
    margin-right: 5px;
  }

  .menu li {
    margin-right: 10px;
    font-size: 1em;
  }

  .container {
    max-width: 960px;
  }

  .flex-item {
    margin: 5px;
  }

  footer {
    height: 40px;
    font-size: 1em;
  }
}

/* Estilos específicos para dispositivos de escritorio */

@media (min-width: 1024px) {
  header {
    height: 100px;
  }

  #logo {
    font-size: 2.5em;
  }

  .menu {
    margin-right: 20px;
  }

  .menu li {
    margin-right: 25px;
    font-size: 1.5em;
  }

  .container {
    max-width: 1280px;
  }

  .flex-item {
    margin: 15px;
  }

  footer {
    height: 60px;
    font-size: 1.5em;
  }
}
```

**Explicación:**

Este código CSS es complejo y detallado, y cubre una amplia gama de aspectos del diseño de una página web. Incluye estilos para el encabezado, el cuerpo y el pie de página, así como estilos específicos para dispositivos móviles y de escritorio.

* **Configuración básica:**

    * Esta sección establece algunos estilos básicos para todos los elementos de la página, como el tamaño de la fuente y el espaciado.

* **Encabezado:**

    * Esta sección define los estilos para el encabezado de la página, incluyendo el logotipo, el menú y el fondo.

* **Cuerpo:**

    * Esta sección define los estilos para el cuerpo de la página, incluyendo el contenedor, los elementos flexibles y los márgenes.

* **Pie de página:**

    * Esta sección define los estilos para el pie de página de la página, incluyendo el color de fondo, el color del texto y el tamaño de la fuente.

* **Estilos específicos para dispositivos móviles:**

    * Esta sección define estilos específicos para dispositivos móviles, como reducir el tamaño de la fuente y los márgenes en pantallas más pequeñas.

* **Estilos específicos para dispositivos de escritorio:**

    * Esta sección define estilos específicos para dispositivos de escritorio, como aumentar el tamaño de la fuente y los márgenes en pantallas más grandes.

Este código CSS es complejo y detallado, pero también es muy completo y proporciona un control total sobre el diseño de una página web.