```css
/* 1. Estilos globales */

body {
    font-family: Arial, sans-serif;
    margin: 0;
    padding: 0;
    color: #333;
    background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
    font-weight: bold;
}

p {
    line-height: 1.5em;
}

a {
    color: #000;
    text-decoration: none;
}

ul, ol {
    list-style-type: none;
    padding: 0;
    margin: 0;
}

li {
    display: inline-block;
    margin-right: 10px;
}

/* 2. Estilos para el encabezado */

header {
    background-color: #f5f5f5;
    padding: 20px 0;
}

.logo {
    float: left;
    width: 200px;
}

.logo img {
    width: 100%;
}

.navegación {
    float: right;
}

.navegación ul {
    display: flex;
    align-items: center;
}

.navegación li {
    margin-right: 20px;
}

.navegación a {
    font-size: 1.2em;
    font-weight: bold;
}

/* 3. Estilos para el contenido principal */

.contenido {
    padding: 20px 0;
}

.contenido h1 {
    font-size: 2.4em;
}

.contenido p {
    font-size: 1.6em;
}

.contenido img {
    max-width: 100%;
}

/* 4. Estilos para el pie de página */

footer {
    background-color: #f5f5f5;
    padding: 20px 0;
}

.pie-de-página-contenido {
    display: flex;
    align-items: center;
    justify-content: space-between;
}

.pie-de-página-logo {
    width: 200px;
}

.pie-de-página-logo img {
    width: 100%;
}

.pie-de-página-redes-sociales {
    display: flex;
    align-items: center;
}

.pie-de-página-redes-sociales li {
    margin-right: 20px;
}

.pie-de-página-redes-sociales a {
    font-size: 1.2em;
    color: #000;
}

/* 5. Estilos para los medios */

@media (max-width: 768px) {
    .navegación ul {
        display: block;
    }

    .navegación li {
        margin-bottom: 10px;
    }

    .contenido h1 {
        font-size: 2em;
    }

    .contenido p {
        font-size: 1.4em;
    }

    .pie-de-página-contenido {
        flex-direction: column;
        align-items: center;
    }

    .pie-de-página-logo {
        width: 100%;
        margin-bottom: 20px;
    }

    .pie-de-página-redes-sociales {
        justify-content: center;
    }
}
```

Este código CSS define los estilos para un sitio web completo, desde el encabezado hasta el pie de página. Incluye estilos para el texto, los encabezados, las listas, las imágenes, los enlaces y los medios. También incluye estilos para un logotipo, un menú de navegación y redes sociales.

El código está bien organizado y comentado, lo que facilita su lectura y comprensión. También utiliza muchas características de CSS3, como Flexbox y Media Queries, para crear un sitio web moderno y adaptable.