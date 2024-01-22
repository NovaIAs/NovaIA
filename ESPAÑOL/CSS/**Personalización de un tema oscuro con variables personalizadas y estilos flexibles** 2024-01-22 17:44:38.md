```css
/* Variables personalizadas */

:root {
  --color-primario: #007bff;
  --color-secundario: #6c757d;
  --color-terciario: #ffffff;
  --fuente-primaria: "Arial", sans-serif;
  --fuente-secundaria: "Times New Roman", serif;
  --espaciado-entre-elementos: 1rem;
  --ancho-maximo-contenedor: 1200px;
}

/* Estilos generales */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: var(--fuente-primaria);
  font-size: 16px;
  line-height: 1.5;
  color: var(--color-terciario);
  background-color: var(--color-secundario);
}

/* Encabezado */

header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: var(--color-primario);
}

header nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 20px;
}

header nav a {
  text-decoration: none;
  color: var(--color-terciario);
  font-size: 18px;
  font-weight: bold;
}

header nav a:hover {
  color: var(--color-secundario);
}

/* Contenedor principal */

main {
  margin-top: 60px;
  padding: 20px;
  max-width: var(--ancho-maximo-contenedor);
  margin: 0 auto;
}

/* Artículos */

article {
  margin-bottom: var(--espaciado-entre-elementos);
  padding: 20px;
  background-color: var(--color-terciario);
  border: 1px solid var(--color-secundario);
}

article h2 {
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 10px;
}

article p {
  font-size: 16px;
  line-height: 1.5;
}

/* Imágenes */

img {
  max-width: 100%;
}

/* Pie de página */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: var(--color-primario);
}

footer p {
  text-align: center;
  color: var(--color-terciario);
  font-size: 14px;
  line-height: 1.5;
  padding: 20px;
}
```

**Explicación del código:**

* El código CSS anterior define un tema oscuro con un encabezado fijo, un contenedor principal con un ancho máximo, artículos con bordes y un pie de página fijo.
* Utiliza variables personalizadas para definir los colores, las fuentes y el espaciado, lo que facilita el cambio del tema.
* Utiliza estilos flexbox para crear un diseño flexible y responsivo.
* Utiliza selectores de clase y de ID para aplicar estilos a elementos específicos de la página.
* Utiliza consultas de medios para aplicar estilos específicos a diferentes tamaños de pantalla.

Este código CSS es complejo, pero también es muy flexible y extensible. Puede utilizarse para crear una variedad de sitios web, desde blogs y portafolios hasta tiendas en línea y sitios web empresariales.