```css
/* Reset de estilos */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Elementos generales */

body {
  font-family: Arial, sans-serif;
  font-size: 16px;
  line-height: 1.6em;
  color: #333;
}

a {
  text-decoration: none;
}

/* Encabezado */

header {
  background-color: #f1f1f1;
  padding: 20px 40px;
}

.logo {
  float: left;
  font-size: 24px;
  font-weight: bold;
}

nav {
  float: right;
}

nav ul {
  list-style-type: none;
  display: flex;
}

nav li {
  margin-right: 20px;
}

nav a {
  display: block;
  padding: 10px 15px;
  color: #333;
}

nav a:hover {
  background-color: #eee;
}

/* Contenido */

.container {
  padding: 20px 40px;
}

.main-content {
  float: left;
  width: 70%;
}

.sidebar {
  float: right;
  width: 30%;
}

.post {
  margin-bottom: 20px;
}

.post-title {
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 10px;
}

.post-content {
  line-height: 1.8em;
}

.post-meta {
  margin-top: 10px;
}

.post-meta span {
  margin-right: 10px;
}

/* Pie de página */

footer {
  background-color: #f1f1f1;
  padding: 20px 40px;
}

.copyright {
  float: left;
}

.social-links {
  float: right;
}

.social-links ul {
  list-style-type: none;
  display: flex;
}

.social-links li {
  margin-right: 10px;
}

.social-links a {
  display: block;
  padding: 10px 15px;
  color: #333;
}

.social-links a:hover {
  background-color: #eee;
}

/* Medios responsive */

@media (max-width: 768px) {
  .container {
    padding: 20px;
  }

  .main-content,
  .sidebar {
    float: none;
    width: 100%;
  }

  .sidebar {
    margin-top: 20px;
  }
}
```

Este código CSS es complejo e incluye una variedad de características, lo que lo hace útil para crear una variedad de sitios web. Incluye:

* Un reset de estilos para proporcionar una base consistente para su sitio web.
* Estilos para los elementos generales de su sitio web, como la tipografía y los enlaces.
* Estilos para el encabezado, el contenido y el pie de página de su sitio web.
* Medios responsive para que su sitio web se vea bien en todos los dispositivos.

Este código es un buen punto de partida para crear un sitio web personalizado. Puede agregar o eliminar estilos para adaptarlos a sus necesidades.