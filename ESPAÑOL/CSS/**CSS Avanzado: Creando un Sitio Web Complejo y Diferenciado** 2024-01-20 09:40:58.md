```css
/* Definición de variables */

:root {
--color-primario: #ff0000;
--color-secundario: #00ff00;
--fuente-primaria: Helvetica, Arial, sans-serif;
--fuente-secundaria: Georgia, Times New Roman, serif;
}

/* Estilos generales */

body {
margin: 0;
padding: 0;
font-family: var(--fuente-primaria);
}

h1 {
font-size: 2em;
color: var(--color-primario);
text-align: center;
}

h2 {
font-size: 1.5em;
color: var(--color-secundario);
text-align: left;
}

p {
font-size: 1em;
color: #000;
text-align: justify;
}

/* Estilos específicos */

.container {
width: 960px;
margin: 0 auto;
padding: 20px;
}

.header {
background-color: var(--color-primario);
color: #fff;
padding: 20px;
}

.content {
background-color: #fff;
padding: 20px;
}

.footer {
background-color: var(--color-secundario);
color: #fff;
padding: 20px;
}

/* Media queries */

@media (max-width: 768px) {
.container {
width: 100%;
}

h1 {
font-size: 1.5em;
}

h2 {
font-size: 1.2em;
}

p {
font-size: 0.8em;
}
}

/* Animaciones */

@keyframes ejemplo {
0% {
opacity: 0;
}

100% {
opacity: 1;
}
}

.animado {
animation: ejemplo 2s ease-in-out infinite;
}
```

Este código CSS es complejo y diferenciado porque incluye una gran variedad de características, como:

* Definición de variables: Las variables CSS se utilizan para almacenar valores que se pueden reutilizar en todo el código. Esto hace que sea más fácil cambiar el aspecto general del sitio web modificando unas pocas variables.
* Estilos generales: Los estilos generales se aplican a todos los elementos del sitio web, independientemente de su clase o ID. Esto garantiza que el sitio web tenga un aspecto uniforme.
* Estilos específicos: Los estilos específicos se aplican a elementos específicos del sitio web, como encabezados, párrafos y pies de página. Esto permite personalizar el aspecto de cada elemento individual.
* Media queries: Las media queries se utilizan para responder a diferentes tamaños de pantalla. Esto garantiza que el sitio web se vea bien en todos los dispositivos, independientemente de su tamaño.
* Animaciones: Las animaciones se utilizan para agregar movimiento al sitio web. Esto puede hacer que el sitio web sea más atractivo y atractivo para los visitantes.

En general, este código CSS es un buen ejemplo de cómo crear un sitio web complejo y diferenciado utilizando sólo CSS.