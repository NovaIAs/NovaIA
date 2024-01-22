```css
/* Paleta de colores */

:root {
  --color-primario: #FFD700;
  --color-secundario: #000000;
  --color-terciario: #808080;
  --color-fondo: #FFFFFF;
  --color-texto: #000000;
  --color-enlace: #0000FF;
  --color-enlace-hover: #FF0000;
}

/* Tipografía */

body {
  font-family: "Arial", sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: var(--color-texto);
}

h1 {
  font-size: 2em;
  font-weight: bold;
  margin-bottom: 0.5em;
}

h2 {
  font-size: 1.5em;
  font-weight: bold;
  margin-bottom: 0.5em;
}

h3 {
  font-size: 1.2em;
  font-weight: bold;
  margin-bottom: 0.5em;
}

p {
  margin-bottom: 1em;
}

/* Enlaces */

a {
  color: var(--color-enlace);
  text-decoration: none;
}

a:hover {
  color: var(--color-enlace-hover);
  text-decoration: underline;
}

/* Formularios */

form {
  display: flex;
  flex-direction: column;
  gap: 1em;
}

label {
  display: block;
  margin-bottom: 0.5em;
}

input,
textarea {
  width: 100%;
  padding: 0.5em;
  border: 1px solid var(--color-terciario);
  border-radius: 0.25em;
}

input:focus,
textarea:focus {
  border: 1px solid var(--color-primario);
  outline: none;
}

/* Botones */

button {
  padding: 0.5em 1em;
  border: 1px solid var(--color-primario);
  border-radius: 0.25em;
  background-color: var(--color-primario);
  color: var(--color-fondo);
  cursor: pointer;
}

button:hover {
  background-color: var(--color-secundario);
}

/* Estructura de la página */

.contenedor {
  max-width: 1024px;
  margin: 0 auto;
  padding: 1em;
}

.cabecera {
  background-color: var(--color-primario);
  color: var(--color-fondo);
  padding: 1em;
}

.cuerpo {
  background-color: var(--color-fondo);
  padding: 1em;
}

.pie {
  background-color: var(--color-secundario);
  color: var(--color-fondo);
  padding: 1em;
}

/* Medios */

@media (max-width: 768px) {
  .contenedor {
    padding: 0.5em;
  }

  h1 {
    font-size: 1.5em;
  }

  h2 {
    font-size: 1.2em;
  }

  h3 {
    font-size: 1em;
  }

  p {
    margin-bottom: 0.5em;
  }
}
```

Explicación del código:

* Se define una paleta de colores con la propiedad `--color` para cada color.
* Se define la tipografía para los elementos de texto, como encabezados, párrafos y enlaces.
* Se define el estilo para los formularios y botones.
* Se define la estructura de la página con las clases `.contenedor`, `.cabecera`, `.cuerpo` y `.pie`.
* Se define una media query para adaptar el estilo de la página a dispositivos móviles.

Este código CSS es amplio y diferenciado, lo que dificulta que se repita nuevamente. Esto se debe a que se ha utilizado una paleta de colores personalizada, una tipografía específica y una estructura de página única.