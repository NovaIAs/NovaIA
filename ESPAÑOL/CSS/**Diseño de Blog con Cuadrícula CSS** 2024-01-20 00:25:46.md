```css
/* Container Principal */

.container {
  display: grid;
  grid-template-columns: repeat(12, 1fr);
  grid-gap: 30px;
  padding: 30px;
  background-color: #f8f8f8;
}

/* Encabezado */

.header {
  grid-column: 1 / span 12;
  display: flex;
  align-items: center;
  justify-content: space-between;
  border-bottom: 1px solid #ccc;
  padding-bottom: 30px;
}

.logo {
  width: 150px;
  height: 50px;
}

.navegacion {
  display: flex;
  align-items: center;
}

.navegacion li {
  margin-right: 30px;
  font-size: 16px;
  font-weight: bold;
}

.navegacion li a {
  text-decoration: none;
  color: #333;
}

.navegacion li a:hover {
  color: #666;
}

/* Banner */

.banner {
  grid-column: 1 / span 12;
  background-image: url(banner.jpg);
  background-size: cover;
  background-position: center;
  height: 300px;
}

/* Contenido Principal */

.main {
  grid-column: 1 / span 8;
}

.articulo {
  margin-bottom: 30px;
}

.articulo h2 {
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 15px;
}

.articulo p {
  font-size: 16px;
  line-height: 1.5;
}

/* Barra Lateral */

.sidebar {
  grid-column: 9 / span 4;
}

.widget {
  margin-bottom: 30px;
}

.widget h3 {
  font-size: 18px;
  font-weight: bold;
  margin-bottom: 15px;
}

.widget ul {
  list-style-type: none;
  padding: 0;
}

.widget li {
  margin-bottom: 15px;
}

.widget li a {
  text-decoration: none;
  color: #333;
}

.widget li a:hover {
  color: #666;
}

/* Pie de Página */

.footer {
  grid-column: 1 / span 12;
  padding-top: 30px;
  border-top: 1px solid #ccc;
  text-align: center;
}

.copyright {
  font-size: 14px;
  color: #999;
}

/* Estilos Adicionales */

body {
  font-family: Arial, Helvetica, sans-serif;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

a {
  text-decoration: none;
  color: #333;
}

a:hover {
  color: #666;
}

img {
  max-width: 100%;
}

/* Media Queries */

@media screen and (max-width: 992px) {
  .container {
    grid-template-columns: repeat(1, 1fr);
    grid-gap: 15px;
    padding: 15px;
  }

  .header {
    grid-column: 1 / span 1;
  }

  .banner {
    grid-column: 1 / span 1;
    height: 200px;
  }

  .main {
    grid-column: 1 / span 1;
  }

  .sidebar {
    grid-column: 1 / span 1;
  }

  .footer {
    grid-column: 1 / span 1;
  }
}
```

**Explicación del código:**

1. **Container:** El contenedor principal de la página web. Está dividido en una cuadrícula de 12 columnas.

2. **Encabezado:** El encabezado de la página web. Contiene el logotipo y los enlaces de navegación.

3. **Banner:** El banner de la página web. Es una imagen de ancho completo que se utiliza para mostrar información importante.

4. **Contenido Principal:** El contenido principal de la página web. Contiene los artículos o entradas del blog.

5. **Barra Lateral:** La barra lateral de la página web. Contiene widgets como el de categorías, el de entradas recientes y el de redes sociales.

6. **Pie de Página:** El pie de página de la página web. Contiene información sobre el copyright y los enlaces a las redes sociales.

7. **Estilos Adicionales:** Estilos adicionales para el cuerpo de la página web, los encabezados, los enlaces y las imágenes.

8. **Media Queries:** Media queries para ajustar el diseño de la página web a diferentes tamaños de pantalla.