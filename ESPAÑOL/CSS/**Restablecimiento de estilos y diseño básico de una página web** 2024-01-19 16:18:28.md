```css
/* Restablecimiento de estilos */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Variables globales */

:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-terciario: #ffffff;
  --fuente-primaria: 'Helvetica', 'Arial', sans-serif;
  --fuente-secundaria: 'Times New Roman', serif;
}

/* Elementos básicos */

body {
  font-family: var(--fuente-primaria);
  background-color: var(--color-terciario);
}

h1, h2, h3, h4, h5, h6 {
  font-family: var(--fuente-secundaria);
  font-weight: bold;
}

p {
  font-size: 16px;
  line-height: 1.5em;
}

a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Encabezado */

header {
  background-color: var(--color-primario);
  color: var(--color-terciario);
  padding: 10px;
}

header h1 {
  font-size: 24px;
}

/* Menú de navegación */

nav {
  background-color: var(--color-secundario);
  padding: 10px;
}

nav ul {
  list-style-type: none;
  display: flex;
  justify-content: space-around;
}

nav li {
  padding: 10px;
}

nav li a {
  color: var(--color-terciario);
}

/* Sección principal */

main {
  padding: 10px;
}

/* Pie de página */

footer {
  background-color: var(--color-primario);
  color: var(--color-terciario);
  padding: 10px;
}

footer p {
  font-size: 12px;
}

/* Medios responsivos */

@media (max-width: 768px) {
  nav ul {
    flex-direction: column;
  }
}
```

Explicación del código:

* **Restablecimiento de estilos:** Este es un conjunto de reglas que se utiliza para restablecer todos los estilos por defecto del navegador. Esto es útil para garantizar que todos los elementos se vean igual en todos los navegadores.
* **Variables globales:** Las variables globales se utilizan para almacenar valores que se pueden utilizar en todo el código CSS. Esto hace que sea más fácil cambiar el aspecto de todo el sitio web cambiando sólo unas pocas variables.
* **Elementos básicos:** Esta sección contiene reglas para los elementos básicos de una página web, como el cuerpo, los encabezados, los párrafos y los enlaces.
* **Encabezado:** Esta sección contiene reglas para el encabezado de la página web.
* **Menú de navegación:** Esta sección contiene reglas para el menú de navegación.
* **Sección principal:** Esta sección contiene reglas para la sección principal de la página web.
* **Pie de página:** Esta sección contiene reglas para el pie de página de la página web.
* **Medios responsivos:** Esta sección contiene reglas que se utilizan para crear un diseño adaptable, lo que significa que el sitio web se adaptará automáticamente a diferentes tamaños de pantalla.