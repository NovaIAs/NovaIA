```css
/* 1. Reset de estilos */

*,
::before,
::after {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

/* 2. Fuentes y tipografía */

body {
  font-family: 'Arial', 'Helvetica', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

a {
  text-decoration: none;
  color: #007bff;
}

/* 3. Colores */

:root {
  --primary-color: #007bff;
  --secondary-color: #6c757d;
  --success-color: #28a745;
  --info-color: #17a2b8;
  --warning-color: #ffc107;
  --danger-color: #dc3545;
  --light-color: #f8f9fa;
  --dark-color: #212529;
}

/* 4. Elementos de la interfaz de usuario */

.btn {
  display: inline-block;
  padding: 0.5em 1em;
  border: 1px solid #007bff;
  border-radius: 0.25em;
  background-color: #007bff;
  color: #ffffff;
  text-align: center;
  text-decoration: none;
}

.btn-primary {
  background-color: var(--primary-color);
}

.btn-secondary {
  background-color: var(--secondary-color);
}

.btn-success {
  background-color: var(--success-color);
}

.btn-info {
  background-color: var(--info-color);
}

.btn-warning {
  background-color: var(--warning-color);
}

.btn-danger {
  background-color: var(--danger-color);
}

.btn-light {
  background-color: var(--light-color);
}

.btn-dark {
  background-color: var(--dark-color);
}

/* 5. Estructura de la página */

.container {
  max-width: 1200px;
  padding: 0 15px;
}

.header {
  background-color: var(--primary-color);
  color: #ffffff;
  padding: 1em 0;
}

.header__logo {
  float: left;
}

.header__nav {
  float: right;
}

.header__nav ul {
  list-style-type: none;
  padding: 0;
}

.header__nav li {
  display: inline-block;
  margin-right: 1em;
}

.header__nav a {
  color: #ffffff;
}

.main {
  padding: 1em 0;
}

.footer {
  background-color: var(--dark-color);
  color: #ffffff;
  padding: 1em 0;
}

.footer__copyright {
  text-align: center;
}

/* 6. Estilos adicionales */

.visually-hidden {
  display: none;
}

.sr-only {
  display: none;
}

@media (max-width: 768px) {
  .header__nav {
    float: none;
    display: block;
  }

  .header__nav ul {
    margin-bottom: 1em;
  }

  .header__nav li {
    display: block;
    margin-right: 0;
  }
}
```

Explicación del código:

* **1. Reset de estilos:** Esta sección restablece los estilos predeterminados de los elementos HTML para que todos los navegadores los muestren de la misma manera.
* **2. Fuentes y tipografía:** Esta sección define las fuentes y la tipografía que se utilizarán en todo el sitio web.
* **3. Colores:** Esta sección define los colores que se utilizarán en todo el sitio web.
* **4. Elementos de la interfaz de usuario:** Esta sección define los estilos para los elementos de la interfaz de usuario, como botones, enlaces y formularios.
* **5. Estructura de la página:** Esta sección define la estructura de la página, como el encabezado, el cuerpo y el pie de página.
* **6. Estilos adicionales:** Esta sección incluye estilos adicionales, como estilos para elementos que se ocultan visualmente o que sólo se muestran en dispositivos móviles.