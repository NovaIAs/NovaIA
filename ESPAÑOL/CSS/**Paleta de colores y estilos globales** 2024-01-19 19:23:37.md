```css
/* Definición de colores */
:root {
    --color-primario: #007BFF;
    --color-secundario: #6C757D;
    --color-terciario: #28A745;
    --color-cuarto: #17A2B8;
    --color-quinto: #DC3545;
}

/* Estilos para el cuerpo */
body {
    font-family: "Arial", sans-serif;
    background-color: #F5F5F5;
    color: #212529;
    margin: 0;
    padding: 0;
}

/* Estilos para el encabezado */
header {
    background-color: var(--color-primario);
    color: #fff;
    padding: 10px 20px;
}

/* Estilos para el título del encabezado */
h1 {
    font-size: 2em;
    margin-top: 0;
}

/* Estilos para el menú de navegación */
nav {
    background-color: var(--color-secundario);
    padding: 5px 10px;
}

/* Estilos para los enlaces del menú de navegación */
nav a {
    color: #fff;
    text-decoration: none;
    padding: 5px 10px;
}

/* Estilos para el enlace activo del menú de navegación */
nav a.active {
    background-color: var(--color-primario);
}

/* Estilos para el contenido principal */
main {
    padding: 20px;
}

/* Estilos para los artículos del contenido principal */
article {
    background-color: #fff;
    padding: 10px;
    margin-bottom: 10px;
}

/* Estilos para los títulos de los artículos del contenido principal */
article h2 {
    font-size: 1.5em;
    margin-top: 0;
}

/* Estilos para los párrafos de los artículos del contenido principal */
article p {
    margin-bottom: 10px;
}

/* Estilos para las listas del contenido principal */
ul {
    list-style-type: disc;
    padding-left: 15px;
}

/* Estilos para los elementos de las listas del contenido principal */
ul li {
    margin-bottom: 5px;
}

/* Estilos para las imágenes del contenido principal */
img {
    max-width: 100%;
    height: auto;
}

/* Estilos para el pie de página */
footer {
    background-color: var(--color-secundario);
    color: #fff;
    padding: 10px 20px;
}

/* Estilos para el texto del pie de página */
footer p {
    margin-top: 0;
}
```

**Explicación del código:**

* El código define una paleta de colores personalizados en la raíz del documento, utilizando la propiedad `--color-primario`, `--color-secundario`, `--color-terciario`, `--color-cuarto` y `--color-quinto`. Estos colores se pueden utilizar en todo el código para crear un diseño consistente.
* El cuerpo del documento tiene una fuente Arial, un fondo de color #F5F5F5, un color de texto #212529 y márgenes y acolchados cero.
* El encabezado tiene un fondo de color primario, un color de texto blanco y un acolchado de 10px 20px.
* El título del encabezado tiene un tamaño de 2em y un margen superior cero.
* El menú de navegación tiene un fondo de color secundario, un color de texto blanco y un acolchado de 5px 10px.
* Los enlaces del menú de navegación tienen un color de texto blanco, una decoración de texto sin subrayado y un acolchado de 5px 10px.
* El enlace activo del menú de navegación tiene un fondo de color primario.
* El contenido principal tiene un acolchado de 20px.
* Los artículos del contenido principal tienen un fondo de color blanco, un acolchado de 10px y un margen inferior de 10px.
* Los títulos de los artículos del contenido principal tienen un tamaño de 1.5em y un margen superior cero.
* Los párrafos del contenido principal tienen un margen inferior de 10px.
* Las listas del contenido principal tienen un tipo de lista de disco y un acolchado izquierdo de 15px.
* Los elementos de las listas del contenido principal tienen un margen inferior de 5px.
* Las imágenes del contenido principal tienen un ancho máximo del 100% y una altura automática.
* El pie de página tiene un fondo de color secundario, un color de texto blanco y un acolchado de 10px 20px.
* El texto del pie de página tiene un margen superior cero.