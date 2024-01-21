```css
/* 1. Establecer el tipo de letra global */
body {
    font-family: 'Open Sans', sans-serif;
}

/* 2. Establecer el esquema de colores global */
:root {
    --color-primario: #ff0000;
    --color-secundario: #00ff00;
    --color-terciario: #0000ff;
}

/* 3. Crear una clase para los encabezados */
.encabezado {
    font-size: 24px;
    font-weight: bold;
    color: var(--color-primario);
    margin-bottom: 10px;
}

/* 4. Crear una clase para los párrafos */
.parrafo {
    font-size: 16px;
    line-height: 1.5;
    margin-bottom: 10px;
}

/* 5. Crear una clase para las listas */
.lista {
    list-style-type: none;
    padding: 0;
    margin: 0;
}

/* 6. Crear una clase para los elementos de la lista */
.lista-elemento {
    display: flex;
    align-items: center;
    margin-bottom: 10px;
}

/* 7. Crear una clase para el ícono de la lista */
.lista-icono {
    margin-right: 10px;
    font-size: 18px;
    color: var(--color-secundario);
}

/* 8. Crear una clase para el texto del elemento de la lista */
.lista-texto {
    flex-grow: 1;
}

/* 9. Crear una clase para los enlaces */
a {
    color: var(--color-terciario);
    text-decoration: none;
}

/* 10. Crear una clase para los botones */
.boton {
    display: inline-block;
    padding: 10px 20px;
    background-color: var(--color-primario);
    color: #fff;
    text-align: center;
    font-size: 16px;
    font-weight: bold;
    border: none;
    cursor: pointer;
}

/* 11. Crear una clase para los formularios */
.formulario {
    width: 100%;
}

/* 12. Crear una clase para los campos de texto del formulario */
.campo-texto {
    width: 100%;
    padding: 10px;
    margin-bottom: 10px;
    border: 1px solid #ccc;
}

/* 13. Crear una clase para los botones de envío del formulario */
.boton-envio {
    display: inline-block;
    padding: 10px 20px;
    background-color: var(--color-primario);
    color: #fff;
    text-align: center;
    font-size: 16px;
    font-weight: bold;
    border: none;
    cursor: pointer;
}

/* 14. Crear una clase para las tablas */
.tabla {
    width: 100%;
    border-collapse: collapse;
}

/* 15. Crear una clase para las filas de la tabla */
.tabla-fila {
    border-bottom: 1px solid #ccc;
}

/* 16. Crear una clase para las columnas de la tabla */
.tabla-columna {
    padding: 10px;
}
```

**Explicación del código:**

1. El código comienza estableciendo el tipo de letra global para todo el documento.
2. A continuación, se establece el esquema de colores global, utilizando variables CSS.
3. Se crea una clase para los encabezados, con un estilo de fuente, tamaño y color específicos.
4. Se crea una clase para los párrafos, con un estilo de fuente, tamaño y espaciado de líneas específicos.
5. Se crea una clase para las listas, con un estilo sin viñetas y sin márgenes ni relleno.
6. Se crea una clase para los elementos de la lista, con un estilo de visualización flexible y un margen inferior.
7. Se crea una clase para el ícono de la lista, con un margen derecho, tamaño de fuente y color específicos.
8. Se crea una clase para el texto del elemento de la lista, con un estilo de crecimiento flexible.
9. Se crea una clase para los enlaces, con un color y un estilo de decoración de texto específicos.
10. Se crea una clase para los botones, con un estilo de visualización en línea, rellenos, color de fondo, color de texto, alineación de texto, tamaño de fuente, peso de fuente, estilo de borde y cursor.
11. Se crea una clase para los formularios, con un ancho del 100%.
12. Se crea una clase para los campos de texto del formulario, con un ancho del 100%, rellenos, margen inferior, estilo de borde y estilo de cursor.
13. Se crea una clase para los botones de envío del formulario, con un estilo de visualización en línea, rellenos, color de fondo, color de texto, alineación de texto, tamaño de fuente, peso de fuente, estilo de borde y cursor.
14. Se crea una clase para las tablas, con un ancho del 100% y un estilo de colapso de bordes.
15. Se crea una clase para las filas de la tabla, con un estilo de borde inferior.
16. Se crea una clase para las columnas de la tabla, con un estilo de relleno.