```css
/* Estilo general del documento */

body {
  font-family: sans-serif;
  background-color: #efefef;
  color: #333;
}

/* Encabezado principal */

header {
  background-color: #222;
  color: #fff;
  padding: 20px;
}

header h1 {
  font-size: 26px;
  font-weight: bold;
}

/* Navegación principal */

nav {
  background-color: #444;
  color: #fff;
  padding: 10px;
}

nav ul {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

nav li {
  display: inline-block;
  margin-right: 10px;
}

nav a {
  color: #fff;
  text-decoration: none;
}

/* Contenido principal */

main {
  padding: 20px;
}

/* Artículos individuales */

article {
  background-color: #fff;
  padding: 20px;
  margin-bottom: 20px;
}

article h2 {
  font-size: 20px;
  font-weight: bold;
}

article p {
  font-size: 16px;
}

/* Pie de página */

footer {
  background-color: #222;
  color: #fff;
  padding: 20px;
}

footer p {
  font-size: 14px;
}

/* Estilos adicionales */

.resaltado {
  background-color: #ff0;
  color: #000;
  padding: 10px;
}

.oculto {
  display: none;
}

/* Medios específicos */

@media (max-width: 768px) {
  /* Cambios de estilo para dispositivos móviles */

  nav {
    background-color: #333;
  }

  nav ul {
    margin-top: 10px;
  }

  nav li {
    margin-right: 5px;
  }

  article {
    margin-bottom: 10px;
  }

  footer {
    padding: 10px;
  }
}

/* Códigos de estilos personalizados para los elementos en la página */

#mi-imagen {
  width: 200px;
  height: 200px;
}

.mi-clase {
  background-color: #f00;
  color: #fff;
  padding: 10px;
}
```

Este código CSS proporciona un estilo completo a un documento HTML, incluyendo encabezado, navegación, contenido principal, pie de página y algunos estilos adicionales comunes. También incluye estilos específicos para dispositivos móviles. Además, incluye ejemplos de códigos de estilos personalizados para elementos específicos en la página.