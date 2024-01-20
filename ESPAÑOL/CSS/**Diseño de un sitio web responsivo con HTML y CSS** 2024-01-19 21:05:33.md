```css
/* Estilo general para el sitio web */

* {
  box-sizing: border-box;
  font-family: Arial, Helvetica, sans-serif;
}

/* Encabezado del sitio web */

header {
  background-color: #333;
  color: #fff;
  padding: 20px;
  text-align: center;
}

header h1 {
  font-size: 24px;
  margin-bottom: 10px;
}

header nav {
  display: flex;
  justify-content: center;
  align-items: center;
}

header nav a {
  color: #fff;
  text-decoration: none;
  padding: 10px;
  margin: 0 10px;
}

header nav a:hover {
  background-color: #555;
}

/* Sección principal del sitio web */

main {
  padding: 20px;
}

/* Artículos del blog */

article {
  background-color: #fff;
  padding: 20px;
  margin-bottom: 20px;
}

article h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

article p {
  font-size: 16px;
  line-height: 1.5;
}

/* Barra lateral del sitio web */

aside {
  background-color: #f5f5f5;
  padding: 20px;
  margin-left: 20px;
}

aside h3 {
  font-size: 18px;
  margin-bottom: 10px;
}

aside ul {
  list-style-type: none;
  padding: 0;
}

aside li {
  margin-bottom: 10px;
}

aside a {
  color: #333;
  text-decoration: none;
}

aside a:hover {
  text-decoration: underline;
}

/* Pie de página del sitio web */

footer {
  background-color: #333;
  color: #fff;
  padding: 20px;
  text-align: center;
}

footer p {
  font-size: 16px;
  margin-bottom: 10px;
}

/* Medios responsivos */

@media (max-width: 768px) {
  main {
    margin-top: 20px;
  }

  aside {
    margin-top: 20px;
  }
}
```

**Explicación del código:**

* El código comienza con los estilos generales para el sitio web, como el tipo de letra predeterminado y el tamaño de la caja.
* La sección del encabezado incluye un título y un menú de navegación.
* La sección principal contiene los artículos del blog.
* La sección de la barra lateral contiene información adicional, como una lista de categorías y enlaces a otros sitios web.
* El pie de página incluye información sobre el copyright y la información de contacto.
* El código también incluye medios responsivos para garantizar que el sitio web se vea bien en dispositivos móviles y tabletas.