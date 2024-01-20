```css
/* 1. Definición de variables personalizadas */

:root {
  --fuente-primaria: 'Roboto', sans-serif;
  --fuente-secundaria: 'Open Sans', sans-serif;
  --color-primario: #1877f2;
  --color-secundario: #f5f5f5;
  --color-acento: #ff4081;
  --ancho-maximo: 1200px;
  --espacio: 10px;
}

/* 2. Restablecimiento de estilos */

html, body, p, h1, h2, h3, h4, h5, h6 {
  margin: 0;
  padding: 0;
}

/* 3. Tipografía */

body {
  font-family: var(--fuente-primaria);
  font-size: 16px;
  line-height: 1.5;
}

h1, h2, h3, h4, h5, h6 {
  font-family: var(--fuente-secundaria);
  font-weight: bold;
}

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.5rem;
}

h4 {
  font-size: 1.2rem;
}

h5 {
  font-size: 1rem;
}

h6 {
  font-size: 0.8rem;
}

/* 4. Colores */

a {
  color: var(--color-primario);
}

a:hover {
  color: var(--color-acento);
}

.boton {
  background-color: var(--color-primario);
  color: var(--color-secundario);
}

.boton:hover {
  background-color: var(--color-acento);
}

/* 5. Diseño */

.contenedor {
  max-width: var(--ancho-maximo);
  padding: var(--espacio);
}

.fila {
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
}

.columna {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: space-between;
}

.texto-centrado {
  text-align: center;
}

.texto-derecha {
  text-align: right;
}

/* 6. Medios */

@media (max-width: 768px) {
  .fila {
    flex-direction: column;
    align-items: stretch;
  }

  .columna {
    flex-direction: row;
    align-items: center;
  }
}
```

**Explicación del código:**

1. **Definición de variables personalizadas:** Definimos variables personalizadas para almacenar valores como fuentes, colores, tamaños y espacios. Esto nos permite cambiar fácilmente estos valores en todo el sitio web.
2. **Restablecimiento de estilos:** Aplicamos un reset de estilos para eliminar los estilos predeterminados del navegador y garantizar una base consistente para nuestro sitio web.
3. **Tipografía:** Definimos la tipografía para diferentes elementos como encabezados, párrafos y enlaces. Utilizamos diferentes fuentes y tamaños para crear una jerarquía visual clara.
4. **Colores:** Definimos los colores principales de nuestro sitio web, incluidos el color primario, el color secundario y el color de acento. Utilizamos estos colores para crear contraste y enfatizar diferentes elementos.
5. **Diseño:** Creamos un contenedor principal con un ancho máximo y agregamos espacio alrededor del contenido. Utilizamos filas y columnas para organizar el contenido en diferentes secciones.
6. **Medios:** Utilizamos consultas de medios para adaptar el diseño de nuestro sitio web a diferentes tamaños de pantalla. Por ejemplo, cambiamos la dirección del flujo de elementos cuando la pantalla es más pequeña.