```
/* Este código CSS crea una experiencia de usuario inmersiva y dinámica con fondos animados, efectos de desplazamiento, animaciones de texto y una paleta de colores vibrantes. */

/* Variables */

:root {
  --fondo-primario: #000;
  --fondo-secundario: #333;
  --color-primario: #fff;
  --color-secundario: #666;
  --color-terciario: #999;
  --transicion: 0.3s ease-in-out;
  --duracion-animacion: 1s;
  --retraso-animacion: 0.5s;
}

/* Elementos generales */

body {
  background: var(--fondo-primario);
  color: var(--color-primario);
  font-family: sans-serif;
  text-align: center;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
}

p {
  line-height: 1.8rem;
}

a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  color: var(--color-secundario);
}

/* Encabezado */

header {
  background: var(--fondo-secundario);
  padding: 1rem 2rem;
}

header nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

header nav a {
  margin-right: 1rem;
}

header nav a:last-child {
  margin-right: 0;
}

/* Hero */

.hero {
  background: url(../images/hero-background.jpg) no-repeat center center / cover;
  height: calc(100vh - 5rem);
  display: flex;
  justify-content: center;
  align-items: center;
}

.hero h1 {
  font-size: 3rem;
}

.hero p {
  font-size: 1.8rem;
  max-width: 800px;
  padding: 1rem;
  background: rgba(0, 0, 0, 0.5);
  color: var(--color-primario);
}

/* Sección principal */

main {
  padding: 2rem 4rem;
}

main section {
  margin-bottom: 2rem;
}

/* Efectos de desplazamiento */

.desplazamiento-hacia-arriba {
  animation: desplazamiento-hacia-arriba var(--duracion-animacion) var(--retraso-animacion) ease-in-out forwards;
}

.desplazamiento-hacia-abajo {
  animation: desplazamiento-hacia-abajo var(--duracion-animacion) var(--retraso-animacion) ease-in-out forwards;
}

@keyframes desplazamiento-hacia-arriba {
  0% {
    transform: translate(0, 20%);
    opacity: 0;
  }

  100% {
    transform: translate(0, 0);
    opacity: 1;
  }
}

@keyframes desplazamiento-hacia-abajo {
  0% {
    transform: translate(0, -20%);
    opacity: 0;
  }

  100% {
    transform: translate(0, 0);
    opacity: 1;
  }
}

/* Animaciones de texto */

.animacion-texto-aparecer {
  animation: animacion-texto-aparecer var(--duracion-animacion) var(--retraso-animacion) ease-in-out forwards;
}

@keyframes animacion-texto-aparecer {
  0% {
    opacity: 0;
  }

  100% {
    opacity: 1;
  }
}

/* Pie de página */

footer {
  background: var(--fondo-secundario);
  padding: 1rem 2rem;
}

footer p {
  font-size: 0.8rem;
}

footer a {
  margin-right: 0.5rem;
}

footer a:last-child {
  margin-right: 0;
}

/* Medios */

@media (max-width: 768px) {
  .hero {
    height: calc(100vh - 3rem);
  }

  main {
    padding: 2rem;
  }
}
```

Explicación del código:

* **Variables:** Se definen las variables que se utilizarán en todo el código, como los colores, las fuentes y las transiciones.
* **Elementos generales:** Se configuran los estilos generales del sitio web, como la fuente, el tamaño y el color del texto, así como los enlaces.
* **Encabezado:** Se configura el estilo del encabezado, que incluye el logotipo, la navegación y el banner.
* **Hero:** Se configura el estilo de la sección hero, que incluye la imagen de fondo, el texto y el botón.
* **Sección principal:** Se configura el estilo de la sección principal, que incluye los títulos, el texto y las imágenes.
* **Efectos de desplazamiento:** Se definen los efectos de desplazamiento que se aplicarán a los elementos al desplazarse hacia arriba o hacia abajo.
* **Animaciones de texto:** Se definen las animaciones de texto que se aplicarán a los elementos al cargar la página.
* **Pie de página:** Se configura el estilo del pie de página, que incluye el texto de copyright y los enlaces a las redes sociales.
* **Medios:** Se definen los estilos específicos para los dispositivos móviles.

Este código CSS es complejo y detallado, pero se puede utilizar para crear sitios web visualmente atractivos e interactivos.