```
/* Hoja de estilo de la interfaz web de la empresa "Acme Inc." */

/* Variables globales */

:root {
  --color-primario: #007bff;
  --color-secundario: #ffc107;
  --fuente-primaria: "Helvetica", "Arial", sans-serif;
  --fuente-secundaria: "Georgia", "Times New Roman", serif;
  --ancho-contenedor: 1024px;
  --margen-contenido: 20px;
}

/* Elementos comunes */

body {
  font-family: var(--fuente-primaria);
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

a {
  color: var(--color-primario);
  text-decoration: none;
  transition: color 0.2s ease-in-out;
}

a:hover {
  color: var(--color-secundario);
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-family: var(--fuente-secundaria);
  font-weight: bold;
  margin-top: 0;
}

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.75rem;
}

h4 {
  font-size: 1.5rem;
}

h5 {
  font-size: 1.25rem;
}

h6 {
  font-size: 1rem;
}

p {
  margin-bottom: 1rem;
}

ul,
ol {
  padding-left: 1.5rem;
}

li {
  margin-bottom: 0.5rem;
}

/* Encabezado */

header {
  background-color: var(--color-primario);
  color: #fff;
  padding: 20px 0;
}

header h1 {
  font-size: 2.75rem;
}

header nav {
  float: right;
}

header nav ul {
  display: flex;
  list-style-type: none;
}

header nav li {
  margin-right: 1rem;
}

header nav a {
  color: #fff;
  text-transform: uppercase;
}

/* Sección de contenido */

#contenido {
  width: var(--ancho-contenedor);
  margin: 0 auto;
  padding: var(--margen-contenido);
}

#contenido h2 {
  margin-bottom: 1.5rem;
}

#contenido p {
  text-align: justify;
}

#contenido img {
  max-width: 100%;
  height: auto;
  margin-bottom: 1rem;
}

#contenido .destacado {
  background-color: #f5f5f5;
  padding: 10px;
  margin-bottom: 1.5rem;
}

/* Pie de página */

footer {
  background-color: #f8f9fa;
  color: #6c757d;
  padding: 20px 0;
}

footer p {
  text-align: center;
}

/* Clases y estilos adicionales */

.btn {
  display: inline-block;
  padding: 8px 16px;
  border: 1px solid var(--color-primario);
  border-radius: 4px;
  background-color: var(--color-primario);
  color: #fff;
  text-decoration: none;
  transition: all 0.2s ease-in-out;
}

.btn:hover {
  background-color: var(--color-secundario);
}

.btn-secundario {
  background-color: #fff;
  color: var(--color-primario);
  border-color: var(--color-primario);
}

.btn-secundario:hover {
  background-color: var(--color-secundario);
  color: #fff;
}

.alert {
  padding: 10px;
  margin-bottom: 1.5rem;
  border: 1px solid #dc3545;
  background-color: #f8d7da;
  color: #842029;
}

.alert-success {
  border-color: #28a745;
  background-color: #c3e6cb;
  color: #155724;
}

.table {
  width: 100%;
  border-collapse: collapse;
}

.table th,
.table td {
  padding: 8px;
  border: 1px solid #dee2e6;
}

.table thead th {
  background-color: #f5f5f5;
}

.table tbody tr:nth-child(even) {
  background-color: #f9f9f9;
}

@media (max-width: 768px) {
  #contenido {
    width: calc(100% - 20px);
  }

  header nav {
    float: none;
    text-align: center;
  }

  header nav ul {
    display: block;
  }

  header nav li {
    margin-right: 0;
    margin-bottom: 1rem;
  }
}

```

**Explicación del código:**

* El código está escrito en CSS (Cascading Style Sheets), que es un lenguaje de estilo utilizado para dar formato a los documentos HTML.
* El código está dividido en varias secciones, que incluyen:
    * Variables globales: Estas variables se utilizan para almacenar valores que se utilizan en todo el código, como los colores, las fuentes y los anchos de los elementos.
    * Elementos comunes: Estos estilos se aplican a todos los elementos de la página, como el tipo de letra, el tamaño del texto y el color de los enlaces.
    * Encabezado: Estos estilos se aplican al encabezado de la página, que incluye el logotipo, el título y el menú de navegación.
    * Sección de contenido: Estos estilos se aplican a la sección de contenido de la página, que incluye el título, el texto y las imágenes.
    * Pie de página: Estos estilos se aplican al pie de página de la página, que incluye el texto de copyright y la información de contacto.
    * Clases y estilos adicionales: Estas clases y estilos se pueden utilizar para dar formato a elementos específicos de la página, como los botones, las alertas y las tablas.
* El código también incluye algunos estilos para dispositivos móviles, que se aplican cuando el ancho de la pantalla es inferior a 768 píxeles.