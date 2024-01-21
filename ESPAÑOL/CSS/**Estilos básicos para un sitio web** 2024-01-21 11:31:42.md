```css
/* Restablecimiento de estilos */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Tipografía */

html {
  font-size: 62.5%; /* 10px */
}

body {
  font-family: sans-serif;
  font-size: 1.6rem; /* 16px */
  line-height: 1.5;
}

h1, h2, h3, h4, h5, h6 {
  font-family: serif;
  font-weight: bold;
}

h1 {
  font-size: 2.5rem; /* 25px */
}

h2 {
  font-size: 2rem; /* 20px */
}

h3 {
  font-size: 1.75rem; /* 17.5px */
}

h4 {
  font-size: 1.5rem; /* 15px */
}

h5 {
  font-size: 1.25rem; /* 12.5px */
}

h6 {
  font-size: 1rem; /* 10px */
}

p {
  margin-bottom: 1rem; /* 10px */
}

/* Colores */

:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-terciario: #ffffff;
  --color-cuaternario: #212529;
}

/* Encabezado */

header {
  background-color: #f5f5f5;
  padding: 1rem 2rem;
}

header h1 {
  margin-top: 0;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  display: flex;
}

header nav ul li {
  margin-right: 1rem; /* 10px */
}

header nav ul li a {
  text-decoration: none;
  color: #212529;
}

header nav ul li a:hover {
  color: #007bff;
}

/* Contenido */

main {
  margin-top: 1rem; /* 10px */
}

section {
  margin-bottom: 1rem; /* 10px */
}

/* Pie de página */

footer {
  background-color: #f5f5f5;
  padding: 1rem 2rem;
}

footer p {
  margin-bottom: 0;
}

/* Medios de diseño responsivo */

@media (max-width: 768px) {
  header nav {
    float: none;
  }

  header nav ul {
    flex-direction: column;
  }

  header nav ul li {
    margin-right: 0; /* 0px */
    margin-bottom: 1rem; /* 10px */
  }
}
```

Explicación del código:

* **Restablecimiento de estilos:** Las primeras líneas del código restablecen los estilos predeterminados del navegador para garantizar que todos los elementos comiencen con los mismos estilos.
* **Tipografía:** La sección de tipografía define los estilos para los encabezados, párrafos y otros elementos de texto.
* **Colores:** La sección de colores define los colores utilizados en el sitio web.
* **Encabezado:** La sección de encabezado define los estilos para el encabezado del sitio web, que incluye el logotipo, la navegación y otros elementos.
* **Contenido:** La sección de contenido define los estilos para el contenido del sitio web, que incluye las secciones, los artículos y otros elementos.
* **Pie de página:** La sección de pie de página define los estilos para el pie de página del sitio web, que incluye la información de copyright y otros elementos.
* **Medios de diseño responsivo:** La sección de medios de diseño responsivo define los estilos para diferentes tamaños de pantalla.