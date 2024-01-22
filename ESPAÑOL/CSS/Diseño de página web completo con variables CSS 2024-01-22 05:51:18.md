```css
/* Definición de variables */
:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-terciario: #ffc107;
  --fuente-principal: "Helvetica", "Arial", sans-serif;
  --fuente-secundaria: "Times New Roman", serif;
}

/* Elementos generales */
body {
  font-family: var(--fuente-principal);
  color: var(--color-secundario);
  background-color: #f8f9fa;
}

h1, h2, h3, h4, h5, h6 {
  font-family: var(--fuente-secundaria);
  color: var(--color-primario);
}

p {
  line-height: 1.5;
  text-align: justify;
}

a {
  color: var(--color-primario);
  text-decoration: none;
}

a:hover {
  color: var(--color-terciario);
  text-decoration: underline;
}

/* Encabezado */
header {
  background-color: var(--color-primario);
  color: #fff;
  padding: 20px 0;
}

header h1 {
  font-size: 2.5rem;
  text-align: center;
}

header nav {
  display: flex;
  justify-content: center;
  align-items: center;
}

header nav a {
  margin: 0 10px;
  font-size: 1.2rem;
}

/* Sección principal */
main {
  padding: 40px 0;
}

main article {
  margin-bottom: 20px;
}

main article h2 {
  font-size: 2rem;
  margin-bottom: 10px;
}

main article p {
  font-size: 1.1rem;
}

/* Barra lateral */
aside {
  float: right;
  width: 250px;
  padding: 20px;
  background-color: #f5f5f5;
}

aside h3 {
  font-size: 1.5rem;
  margin-bottom: 10px;
}

aside ul {
  list-style-type: none;
  padding: 0;
}

aside ul li {
  margin-bottom: 10px;
}

aside ul li a {
  font-size: 1.1rem;
}

/* Pie de página */
footer {
  background-color: var(--color-primario);
  color: #fff;
  padding: 20px 0;
}

footer p {
  text-align: center;
}

/* Medias queries */
@media (max-width: 768px) {
  aside {
    float: none;
    width: 100%;
  }
}
```

Este código CSS crea un diseño de página web completo con encabezado, sección principal, barra lateral y pie de página. Utiliza variables para definir los colores, las fuentes y otros estilos globales, lo que hace que sea fácil cambiar el aspecto general de la página simplemente cambiando los valores de las variables.

El encabezado incluye un logotipo y un menú de navegación. La sección principal contiene el contenido principal de la página, como artículos y entradas de blog. La barra lateral contiene información adicional, como listas de enlaces y widgets. El pie de página contiene información de contacto y otros enlaces importantes.

El código utiliza también medios de consulta (media queries) para cambiar el diseño de la página en función del tamaño de la ventana del navegador. Por ejemplo, cuando la ventana del navegador es inferior a 768 píxeles de ancho, la barra lateral se apila debajo de la sección principal en lugar de estar a su lado.

Este código es bastante complejo y difícil de entender para alguien que no está familiarizado con CSS. Sin embargo, proporciona un buen ejemplo de cómo se puede utilizar CSS para crear diseños de página web complejos y sofisticados.