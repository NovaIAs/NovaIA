```
/* Restablecer estilos predeterminados */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Tipografía */
body {
  font-family: "Open Sans", sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #333;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

/* Estructura de la página */
#wrapper {
  width: 960px;
  margin: 0 auto;
}

#header {
  background-color: #f8f9fa;
  padding: 20px 0;
}

#logo {
  float: left;
}

#nav {
  float: right;
}

#nav ul {
  list-style: none;
  display: flex;
}

#nav li {
  margin-right: 10px;
}

#nav a {
  display: block;
  padding: 10px 15px;
  border-radius: 5px;
}

#main {
  padding: 20px 0;
}

#content {
  float: left;
  width: 66%;
}

#sidebar {
  float: right;
  width: 33%;
}

#footer {
  background-color: #f8f9fa;
  padding: 20px 0;
}

/* Estilos adicionales */
.btn {
  display: inline-block;
  padding: 10px 15px;
  border-radius: 5px;
  background-color: #007bff;
  color: #fff;
}

.btn:hover {
  background-color: #0056b3;
}

.alert {
  padding: 20px;
  border-radius: 5px;
}

.alert-success {
  background-color: #dff0d8;
}

.alert-danger {
  background-color: #f2dede;
}
```

Este código CSS crea un diseño de página web básico con una cabecera, un menú de navegación, una columna de contenido principal y una columna lateral. También incluye algunos estilos adicionales para botones y alertas.

Explicación del código:

* El código comienza con un restablecimiento de los estilos predeterminados para todos los elementos, lo que garantiza que el diseño sea uniforme en todos los navegadores.
* A continuación, se definen los estilos de tipografía, como la fuente, el tamaño de fuente, el interlineado y el color del texto.
* La estructura de la página se define creando elementos para la cabecera, la navegación, el contenido principal, la columna lateral y el pie de página.
* Se añaden algunos estilos adicionales para botones y alertas.

Este código es muy ampliable y se puede utilizar como base para crear diseños de páginas web más complejos.