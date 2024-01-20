```css
/* Estilo de etiqueta universal: afecta a todos los elementos HTML */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
    font-family: Arial, Helvetica, sans-serif;
}

/* Estilo de encabezado */
header {
    background-color: #222;
    color: #fff;
    padding: 15px;
}

/* Estilo de título de encabezado */
header h1 {
    font-size: 24px;
    margin-bottom: 10px;
}

/* Estilo de navegación */
nav {
    background-color: #333;
    padding: 10px;
}

/* Estilo de enlaces de navegación */
nav a {
    color: #fff;
    text-decoration: none;
    padding: 5px 10px;
}

/* Estilo de enlaces de navegación activos */
nav a.active {
    background-color: #444;
}

/* Estilo de contenedor principal */
main {
    margin-top: 10px;
    padding: 15px;
}

/* Estilo de sección */
section {
    margin-bottom: 10px;
}

/* Estilo de título de sección */
section h2 {
    font-size: 18px;
    margin-bottom: 5px;
}

/* Estilo de párrafo */
p {
    font-size: 14px;
    line-height: 1.5em;
}

/* Estilo de imagen */
img {
    max-width: 100%;
    height: auto;
}

/* Estilo de formulario */
form {
    margin-top: 10px;
}

/* Estilo de campo de texto */
input[type="text"] {
    width: 100%;
    padding: 5px;
    margin-bottom: 5px;
}

/* Estilo de botón de envío */
input[type="submit"] {
    background-color: #444;
    color: #fff;
    padding: 5px 10px;
    border: none;
    cursor: pointer;
}

/* Estilo de pie de página */
footer {
    background-color: #222;
    color: #fff;
    padding: 10px;
}

/* Estilo de créditos del pie de página */
footer p {
    font-size: 12px;
    margin-bottom: 0;
}

/* Estilo de diseño responsivo */
@media (max-width: 768px) {
    /* Diseño de una sola columna para pantallas pequeñas */
    main {
        margin-top: 0;
    }

    nav {
        display: block;
    }

    nav a {
        display: block;
        padding: 10px;
    }
}
```

Este código CSS proporciona un diseño completo para un sitio web, incluyendo encabezado, navegación, contenido principal, pie de página y estilo para elementos como títulos, párrafos, imágenes y formularios. También incluye estilo para diseño responsivo, que adapta el sitio web a pantallas pequeñas.

A continuación se explican algunos aspectos específicos del código:

* Se utiliza la propiedad `box-sizing: border-box;` para que el ancho y el alto de los elementos incluyan el borde y el relleno. Esto ayuda a evitar problemas de diseño cuando se añaden bordes o rellenos a los elementos.
* La navegación se crea utilizando una lista desordenada (`<ul>`) con enlaces (`<a>`). Los enlaces se colocan en línea utilizando la propiedad `display: inline-block;`.
* El contenido principal se divide en secciones utilizando la etiqueta `section`. Cada sección tiene un título (`<h2>`) y contenido (`<p>`, `<img>`, etc.).
* Se utiliza la propiedad `margin-bottom` para añadir espacio vertical entre los elementos. Esto ayuda a mejorar la legibilidad y la organización del contenido.
* Se utiliza la propiedad `max-width` para limitar el ancho de las imágenes. Esto ayuda a evitar que las imágenes se salgan del diseño.
* Se utiliza pseudo-clase `:active` para cambiar el estilo de los enlaces de navegación cuando están activos. Esto ayuda a los usuarios a saber qué página están visitando actualmente.
* Se utiliza la media query `@media (max-width: 768px)` para adaptar el sitio web a pantallas pequeñas. En este caso, la navegación se muestra en una sola columna y el contenido principal se coloca debajo de la navegación.