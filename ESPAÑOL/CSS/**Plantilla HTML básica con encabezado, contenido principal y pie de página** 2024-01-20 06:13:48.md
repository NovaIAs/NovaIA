```css
/* Configuración general */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

html, body {
  height: 100%;
  font-family: 'Helvetica', 'Arial', sans-serif;
}

/* Encabezado */

header {
  background-color: #333;
  color: #fff;
  padding: 15px;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
}

header ul {
  list-style-type: none;
  display: flex;
  justify-content: flex-end;
}

header li {
  margin-right: 15px;
}

header a {
  color: #fff;
  text-decoration: none;
}

header a:hover {
  color: #ccc;
}

/* Contenido principal */

main {
  flex: 1;
  padding: 30px;
}

main h2 {
  font-size: 20px;
  font-weight: bold;
  margin-bottom: 15px;
}

main p {
  font-size: 16px;
  line-height: 1.5;
}

main ul {
  list-style-type: none;
  display: flex;
  flex-direction: column;
  padding: 0;
}

main li {
  margin-bottom: 10px;
}

/* Pie de página */

footer {
  background-color: #333;
  color: #fff;
  padding: 15px;
}

footer p {
  font-size: 14px;
  text-align: center;
}

/* Medios de comunicación */

@media (max-width: 768px) {
  header ul {
    flex-direction: column;
    align-items: center;
  }

  header li {
    margin-right: 0;
    margin-bottom: 10px;
  }

  main ul {
    flex-direction: row;
    flex-wrap: wrap;
    justify-content: center;
  }

  main li {
    margin-right: 10px;
    margin-bottom: 0;
  }
}
```

Explicación del código:

* El código comienza con una sección de configuración general que establece algunos estilos básicos para todos los elementos de la página.
* Luego, se define el encabezado, que incluye un logotipo y un menú de navegación.
* El contenido principal se divide en dos secciones: una para el texto y otra para una lista de elementos.
* El pie de página contiene información de contacto y derechos de autor.
* Finalmente, se incluyen algunas reglas de estilo adicionales para medios de comunicación más pequeños, como teléfonos móviles y tabletas.