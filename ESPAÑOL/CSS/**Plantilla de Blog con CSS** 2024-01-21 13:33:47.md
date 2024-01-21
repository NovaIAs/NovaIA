```css
/* Restablecimiento de estilos */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Variables */

:root {
  --primary-color: #3498db;
  --secondary-color: #f1c40f;
  --tertiary-color: #27ae60;
  --font-family: "Helvetica", "Arial", sans-serif;
}

/* Elementos básicos */

body {
  font-family: var(--font-family);
  font-size: 16px;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
  margin-bottom: 10px;
}

p {
  margin-bottom: 10px;
}

a {
  color: var(--primary-color);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Contenedor principal */

.container {
  max-width: 1200px;
  padding: 20px;
  margin: 0 auto;
}

/* Encabezado */

header {
  background-color: var(--primary-color);
  color: #fff;
  padding: 20px;
}

header h1 {
  font-size: 32px;
}

header nav {
  float: right;
}

header nav ul {
  list-style: none;
}

header nav li {
  display: inline-block;
  margin-left: 10px;
}

header nav a {
  color: #fff;
}

/* Contenido principal */

main {
  padding-top: 20px;
}

.post {
  background-color: #fff;
  padding: 20px;
  margin-bottom: 20px;
}

.post-title {
  font-size: 24px;
  margin-bottom: 10px;
}

.post-content {
  font-size: 18px;
}

/* Pie de página */

footer {
  background-color: var(--secondary-color);
  color: #fff;
  padding: 20px;
  text-align: center;
}

/* Medios */

@media (max-width: 768px) {
  .container {
    padding: 10px;
  }

  header h1 {
    font-size: 24px;
  }

  header nav li {
    margin-left: 5px;
  }

  .post {
    padding: 10px;
    margin-bottom: 10px;
  }

  .post-title {
    font-size: 18px;
    margin-bottom: 5px;
  }

  .post-content {
    font-size: 16px;
  }

  footer {
    padding: 10px;
  }
}
```

Explicación del código:

* Restablecimiento de estilos: Este código elimina los estilos predeterminados del navegador para todos los elementos, lo que garantiza un punto de partida limpio para el diseño.
* Variables: Este código define variables personalizadas que se pueden utilizar en todo el documento CSS para definir colores, fuentes y otras propiedades de estilo.
* Elementos básicos: Este código define estilos básicos para elementos comunes como encabezados, párrafos y enlaces.
* Contenedor principal: Este código define un contenedor principal para el contenido del sitio web. El contenedor tiene un ancho máximo de 1200 píxeles y está centrado horizontalmente.
* Encabezado: Este código define el encabezado del sitio web, que incluye el logotipo y la navegación. El encabezado tiene un fondo de color azul y texto blanco.
* Contenido principal: Este código define el contenido principal del sitio web, que incluye las entradas del blog y otras páginas.
* Pie de página: Este código define el pie de página del sitio web, que incluye información de copyright y otros enlaces.
* Medios: Este código define estilos para diferentes tamaños de pantalla. Por ejemplo, cuando la pantalla es inferior a 768 píxeles de ancho, el contenedor principal se estrecha y el encabezado y el pie de página se muestran de manera diferente.