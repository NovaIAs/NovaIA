```css
/* ESTILOS GENERALES */

/* Restablecimiento de estilos predeterminados */

*,
*::before,
*::after {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

/* Tipografía */

body {
  font-family: 'Open Sans', sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
}

h1 {
  font-size: 2.5em;
}

h2 {
  font-size: 2em;
}

h3 {
  font-size: 1.5em;
}

h4 {
  font-size: 1.25em;
}

h5 {
  font-size: 1em;
}

h6 {
  font-size: 0.875em;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

/* Colores */

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

/* Elementos de bloque */

.container {
  width: 100%;
  max-width: 1200px;
  padding: 15px;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -15px;
  margin-left: -15px;
}

.col {
  flex: 1 0 auto;
  padding-right: 15px;
  padding-left: 15px;
}

/* Elementos en línea */

.mr-1 {
  margin-right: 1px;
}

.mr-2 {
  margin-right: 2px;
}

.mr-3 {
  margin-right: 3px;
}

.ml-1 {
  margin-left: 1px;
}

.ml-2 {
  margin-left: 2px;
}

.ml-3 {
  margin-left: 3px;
}

/* Bordeado */

.border {
  border: 1px solid #ddd;
}

.border-top {
  border-top: 1px solid #ddd;
}

.border-right {
  border-right: 1px solid #ddd;
}

.border-bottom {
  border-bottom: 1px solid #ddd;
}

.border-left {
  border-left: 1px solid #ddd;
}

/* Sombreado */

.shadow {
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
}

.shadow-sm {
  box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2);
}

.shadow-lg {
  box-shadow: 0 15px 30px 0 rgba(0, 0, 0, 0.1);
}

/* Tipografía */

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-justify {
  text-align: justify;
}

.text-uppercase {
  text-transform: uppercase;
}

.text-lowercase {
  text-transform: lowercase;
}

.text-capitalize {
  text-transform: capitalize;
}

.text-muted {
  color: #6c757d;
}

.text-primary {
  color: var(--primary-color);
}

.text-secondary {
  color: var(--secondary-color);
}

.text-success {
  color: var(--success-color);
}

.text-info {