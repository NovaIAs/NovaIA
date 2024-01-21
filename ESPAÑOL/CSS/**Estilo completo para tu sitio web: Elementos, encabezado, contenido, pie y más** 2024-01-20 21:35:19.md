```css
/* 1. Variables */

:root {
  --color-primario: #007bff;
  --color-secundario: #ffc107;
  --fuente-primaria: "Roboto", sans-serif;
  --fuente-secundaria: "Raleway", sans-serif;
}

/* 2. Elementos básicos */

body {
  margin: 0;
  padding: 0;
  font-family: var(--fuente-primaria);
  color: #333;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  font-size: 16px;
  line-height: 1.5;
}

a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* 3. Encabezado */

header {
  background-color: var(--color-primario);
  color: #fff;
  padding: 20px;
}

header h1 {
  font-size: 24px;
  margin-bottom: 10px;
}

header nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

header nav ul {
  list-style: none;
  display: flex;
}

header nav ul li {
  margin-right: 10px;
}

header nav ul li a {
  color: #fff;
}

/* 4. Contenido principal */

main {
  padding: 20px;
}

main h2 {
  font-size: 20px;
  margin-bottom: 10px;
}

main p {
  font-size: 16px;
  line-height: 1.5;
}

main ul {
  list-style: none;
  padding: 0;
}

main ul li {
  margin-bottom: 10px;
}

/* 5. Pie de página */

footer {
  background-color: #f5f5f5;
  color: #333;
  padding: 20px;
}

footer p {
  font-size: 14px;
}

footer a {
  color: var(--color-primario);
}

/* 6. Estilos adicionales */

.destacado {
  background-color: var(--color-secundario);
  color: #fff;
  padding: 10px;
  margin-bottom: 20px;
}

.btn {
  background-color: var(--color-primario);
  color: #fff;
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

.btn:hover {
  background-color: var(--color-secundario);
}

/* 7. Media queries */

@media (max-width: 768px) {
  header nav ul {
    display: block;
  }

  header nav ul li {
    margin-bottom: 10px;
  }
}
```

Este código CSS crea un estilo completo para una página web, incluyendo elementos básicos como tipografía, colores y diseño, así como elementos más complejos como un encabezado, un contenido principal y un pie de página. También incluye algunos estilos adicionales como elementos destacados y botones, y responde a diferentes tamaños de pantalla con media queries.

El código está bien organizado y comentado, lo que hace que sea fácil de entender y modificar. Se utiliza un sistema de variables para definir los colores y las fuentes, lo que facilita el cambio de tema de la página.

En general, este código CSS es una buena base para crear un sitio web elegante y funcional.