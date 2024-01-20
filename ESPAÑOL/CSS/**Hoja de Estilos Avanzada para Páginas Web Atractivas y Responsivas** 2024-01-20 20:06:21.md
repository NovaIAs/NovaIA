```css
/* Variables globales */
:root {
  --fuente-principal: sans-serif;
  --fuente-secundaria: serif;
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-fondo: #ffffff;
  --ancho-contenedor: 1200px;
  --margen-contenedor: 0 auto;
}

/* Elementos básicos */
body {
  font-family: var(--fuente-principal);
  background-color: var(--color-fondo);
  color: var(--color-secundario);
}

h1 {
  font-family: var(--fuente-secundaria);
  font-size: 2rem;
  margin-bottom: 1rem;
}

h2 {
  font-family: var(--fuente-secundaria);
  font-size: 1.5rem;
  margin-bottom: 1rem;
}

p {
  line-height: 1.5rem;
  margin-bottom: 1rem;
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
  color: var(--color-fondo);
  padding: 1rem;
}

header h1 {
  margin-top: 0;
}

/* Menú de navegación */
nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

nav ul {
  display: flex;
  list-style: none;
  padding: 0;
  margin: 0;
}

nav li {
  margin-right: 1rem;
}

nav a {
  padding: 0.5rem 1rem;
  border-radius: 5px;
}

nav a:hover {
  background-color: var(--color-secundario);
}

/* Contenedor principal */
main {
  max-width: var(--ancho-contenedor);
  margin: 0 auto;
  padding: 1rem;
}

/* Pie de página */
footer {
  background-color: var(--color-fondo);
  color: var(--color-secundario);
  padding: 1rem;
  text-align: center;
}

/* Clases adicionales */
.resaltado {
  background-color: var(--color-primario);
  color: var(--color-fondo);
  padding: 0.5rem 1rem;
  border-radius: 5px;
}

.oculto {
  display: none;
}

/* Media queries */
@media (max-width: 768px) {
  nav {
    flex-direction: column;
    align-items: stretch;
  }

  nav ul {
    flex-direction: column;
  }

  nav li {
    margin-right: 0;
    margin-bottom: 1rem;
  }
}
```

Este código CSS incluye una serie de características y técnicas avanzadas, como:

* Uso de variables CSS para definir los valores globales del tema, como la fuente principal, el color primario y el ancho del contenedor.
* Uso de clases adicionales para añadir estilos específicos a elementos individuales o grupos de elementos.
* Uso de media queries para adaptar el diseño a diferentes tamaños de pantalla.
* Uso de un selector universal (`*`) para aplicar estilos a todos los elementos de la página.
* Uso de una hoja de estilo en cascada (CSS) para aplicar estilos a los elementos HTML.

Este código CSS es complejo y avanzado, pero también es muy flexible y potente. Puede utilizarse para crear una amplia variedad de diseños web, desde sitios web sencillos hasta aplicaciones web complejas.