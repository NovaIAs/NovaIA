```css
/* Variables */

:root {
  --fuente-principal: Arial, sans-serif;
  --color-principal: #123456;
  --color-secundario: #7890AB;
  --color-terciario: #CDEF01;
  --ancho-contenedor: 1200px;
}

/* Estilos generales */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: var(--fuente-principal);
  background-color: #F0F0F0;
}

/* Encabezado */

header {
  width: 100%;
  height: 60px;
  background-color: var(--color-principal);
  color: #FFFFFF;
}

header h1 {
  float: left;
  margin-left: 20px;
  font-size: 24px;
}

header nav {
  float: right;
  margin-right: 20px;
}

header nav ul {
  list-style-type: none;
  display: flex;
}

header nav ul li {
  margin-right: 20px;
}

header nav ul li a {
  color: #FFFFFF;
  text-decoration: none;
}

header nav ul li a:hover {
  color: var(--color-secundario);
}

/* Sección principal */

main {
  width: 100%;
  margin: 0 auto;
  padding: 20px;
}

main h2 {
  font-size: 24px;
  margin-bottom: 20px;
}

main p {
  font-size: 16px;
  margin-bottom: 20px;
}

main img {
  width: 100%;
  height: auto;
  margin-bottom: 20px;
}

/* Pie de página */

footer {
  width: 100%;
  height: 60px;
  background-color: var(--color-terciario);
  color: #FFFFFF;
}

footer p {
  text-align: center;
  margin-top: 20px;
}

/* Media queries */

@media screen and (max-width: 1024px) {
  header {
    height: 40px;
  }

  header h1 {
    font-size: 18px;
  }

  header nav ul li {
    margin-right: 10px;
  }

  main h2 {
    font-size: 18px;
  }

  main p {
    font-size: 14px;
  }

  footer {
    height: 40px;
  }

  footer p {
    font-size: 12px;
  }
}

@media screen and (max-width: 768px) {
  header nav {
    display: none;
  }

  main {
    padding: 10px;
  }

  footer {
    display: none;
  }
}
```

Este código CSS crea un diseño complejo para una página web, con un encabezado, una sección principal y un pie de página. El código incluye variables para facilitar el cambio de los colores y las fuentes, y utiliza media queries para ajustar el diseño a diferentes tamaños de pantalla.

El encabezado incluye un logotipo y un menú de navegación, que se alinea a la izquierda y a la derecha de la página, respectivamente. La sección principal incluye un título, un párrafo y una imagen, y el pie de página incluye un aviso de copyright.

Las media queries se utilizan para ajustar el diseño de la página a diferentes tamaños de pantalla. Por ejemplo, cuando la pantalla tiene un ancho máximo de 1024 píxeles, el encabezado se hace más pequeño y el texto se vuelve más pequeño. Cuando la pantalla tiene un ancho máximo de 768 píxeles, el menú de navegación se oculta y el pie de página se elimina.