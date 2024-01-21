```CSS
* {
  /* Define estilos básicos para todos los elementos de la página */
  font-family: Arial, sans-serif;
  font-size: 16px;
  color: #333;
  margin: 0;
  padding: 0;
}

body {
  /* Estilos para el cuerpo de la página */
  background-color: #fff;
}

#cabecera {
  /* Estilos para la cabecera de la página */
  width: 100%;
  height: 100px;
  background-color: #f0f0f0;
  box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);
}

#cabecera h1 {
  /* Estilos para el título de la cabecera */
  font-size: 32px;
  text-align: center;
  padding-top: 30px;
}

#navegacion {
  /* Estilos para la barra de navegación */
  width: 100%;
  height: 50px;
  background-color: #333;
}

#navegacion ul {
  /* Estilos para la lista de enlaces de la barra de navegación */
  display: flex;
  justify-content: center;
  align-items: center;
}

#navegacion li {
  /* Estilos para los elementos de la lista de enlaces */
  list-style: none;
  margin: 0 10px;
}

#navegacion a {
  /* Estilos para los enlaces de la barra de navegación */
  text-decoration: none;
  color: #fff;
  font-size: 18px;
}

#navegacion a:hover {
  /* Estilos para los enlaces al pasar el cursor por encima */
  color: #f0f0f0;
}

#contenido {
  /* Estilos para el contenido de la página */
  width: 100%;
  margin-top: 20px;
}

#contenido h2 {
  /* Estilos para los títulos del contenido */
  font-size: 24px;
  margin-bottom: 20px;
}

#contenido p {
  /* Estilos para los párrafos del contenido */
  font-size: 16px;
  text-align: justify;
}

#pie {
  /* Estilos para el pie de página */
  width: 100%;
  height: 50px;
  background-color: #f0f0f0;
  box-shadow: 0px -2px 4px rgba(0, 0, 0, 0.1);
}

#pie p {
  /* Estilos para el párrafo del pie de página */
  text-align: center;
  padding-top: 15px;
}

/* Estilos para las clases personalizadas */

.resaltado {
  /* Clase para resaltar texto */
  background-color: #ff0;
  padding: 5px;
}

.borde {
  /* Clase para añadir un borde a un elemento */
  border: 1px solid #ccc;
  padding: 5px;
}

.centrado {
  /* Clase para centrar horizontalmente un elemento */
  text-align: center;
}

.derecha {
  /* Clase para alinear a la derecha un elemento */
  float: right;
}

.izquierda {
  /* Clase para alinear a la izquierda un elemento */
  float: left;
}

/* Estilos para los estados de los elementos */

:hover {
  /* Estilos para el estado "hover" de un elemento */
  cursor: pointer;
  background-color: #eee;
}

:active {
  /* Estilos para el estado "active" de un elemento */
  background-color: #ccc;
}

:focus {
  /* Estilos para el estado "focus" de un elemento */
  outline: 2px dashed #f00;
}

/* Estilos para los medios */

@media (max-width: 768px) {
  /* Estilos para pantallas de menos de 768px de ancho */
  #cabecera h1 {
    font-size: 28px;
  }

  #navegacion {
    height: 40px;
  }

  #navegacion a {
    font-size: 16px;
  }

  #contenido h2 {
    font-size: 20px;
  }

  #contenido p {
    font-size: 14px;
  }
}
```

Este código CSS es un ejemplo de cómo crear un diseño complejo y diferenciado para una página web. El código cubre una amplia variedad de aspectos, incluyendo:

* Estilos básicos para todos los elementos de la página
* Estilos para la cabecera, la barra de navegación, el contenido y el pie de página
* Estilos para las clases personalizadas
* Estilos para los estados de los elementos
* Estilos para los medios (pantallas de diferentes tamaños)

El resultado es un diseño web que es visualmente atractivo y fácil de usar. El código es también extensible, lo que significa que se puede agregar fácilmente nuevos elementos y estilos a la página web conforme sea necesario.