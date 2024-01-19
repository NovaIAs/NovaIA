```css
/* Estilo básico */

body {
  font-family: "Helvetica", "Arial", sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: white;
}

h1, h2, h3, h4, h5, h6 {
  font-family: "Georgia", "Times New Roman", serif;
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
  text-decoration: underline;
}

/* Diseño de la página */

#header {
  background-color: #f8f9fa;
  padding: 20px;
}

#header h1 {
  font-size: 24px;
  margin-bottom: 10px;
}

#header nav {
  float: right;
}

#header nav ul {
  list-style-type: none;
  padding: 0;
}

#header nav ul li {
  display: inline-block;
  margin-left: 10px;
}

#header nav ul li a {
  font-size: 18px;
}

#main {
  margin-top: 20px;
}

#main h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

#main p {
  margin-bottom: 20px;
}

#footer {
  background-color: #f8f9fa;
  padding: 20px;
}

#footer p {
  font-size: 14px;
}

/* Medios de pantalla */

@media screen and (max-width: 768px) {
  #header nav ul {
    float: none;
    margin-top: 10px;
  }

  #header nav ul li {
    display: block;
    margin-left: 0;
  }
}
```

Este código CSS crea un diseño básico para una página web con encabezado, navegación, contenido principal y pie de página. También incluye algunas reglas de estilo para los diferentes elementos de la página, como el tamaño y la familia de las fuentes, el color del texto y el espaciado.

El código está bien organizado y utiliza reglas CSS específicas para diferentes elementos y secciones de la página. También utiliza consultas de medios para ajustar el diseño de la página para dispositivos móviles.