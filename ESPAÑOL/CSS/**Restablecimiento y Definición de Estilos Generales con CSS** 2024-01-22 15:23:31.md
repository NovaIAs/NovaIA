```css
/* Restablecimiento del CSS */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Variables de CSS */

:root {
  --fuente-principal: 'Arial', sans-serif;
  --fuente-secundaria: 'Courier New', monospace;
  --color-principal: #007bff;
  --color-secundario: #ff9f43;
  --ancho-maximo: 1200px;
}

/* Elementos de HTML */

html {
  font-size: 62.5%; /* Establece el tamaño base de fuente */
}

body {
  font-family: var(--fuente-principal);
  line-height: 1.6;
}

h1, h2, h3, h4, h5, h6 {
  font-family: var(--fuente-secundaria);
}

p {
  margin-bottom: 1rem;
}

a {
  color: var(--color-principal);
  text-decoration: none;
}

a:hover {
  color: var(--color-secundario);
}

/* Clases de CSS */

.contenedor {
  max-width: var(--ancho-maximo);
  margin: 0 auto;
  padding: 1rem;
}

.encabezado {
  background-color: var(--color-principal);
  color: #fff;
  padding: 1rem;
}

.encabezado h1 {
  margin-top: 0;
}

.contenido-principal {
  margin-top: 1rem;
}

.contenido-secundario {
  background-color: var(--color-secundario);
  color: #fff;
  padding: 1rem;
  margin-top: 1rem;
}

.pie-de-pagina {
  background-color: var(--color-principal);
  color: #fff;
  padding: 1rem;
  margin-top: 1rem;
}

/* Medios de Consulta */

@media (min-width: 768px) {
  .contenido-principal, .contenido-secundario {
    float: left;
    width: 50%;
  }

  .contenido-secundario {
    margin-left: 1rem;
  }
}

@media (min-width: 1024px) {
  .contenedor {
    max-width: 960px;
  }
}
```

Explicación del código:

* El código comienza con un restablecimiento del CSS, que establece los estilos predeterminados para todos los elementos HTML.
* Luego se definen las variables de CSS, que son valores que se pueden usar en todo el documento CSS.
* A continuación se definen los estilos para los elementos HTML, como `body`, `h1`, `p`, `a`, etc.
* Luego se definen las clases de CSS, que son conjuntos de estilos que se pueden aplicar a elementos HTML específicos.
* Finalmente, se definen los medios de consulta, que son reglas de estilo que se aplican en función del ancho de la pantalla del dispositivo.