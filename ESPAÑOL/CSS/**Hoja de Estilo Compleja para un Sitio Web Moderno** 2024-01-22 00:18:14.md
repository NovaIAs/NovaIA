```css
/* Estilos generales */

/* Tipografía */

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
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
  text-decoration: underline;
}

/* Encabezado */

header {
  background-color: #f8f9fa;
  padding: 1em;
}

.logo {
  float: left;
  margin-right: 1em;
}

.logo img {
  width: 100px;
}

.navbar {
  float: right;
}

.navbar ul {
  list-style-type: none;
  padding: 0;
}

.navbar li {
  display: inline-block;
  margin-right: 1em;
}

.navbar a {
  padding: 0.5em 1em;
}

/* Contenido */

.content {
  padding: 1em;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.col {
  flex: 1;
  padding: 0.5em;
}

.col-2 {
  flex: 2;
}

.col-3 {
  flex: 3;
}

/* Pie de página */

footer {
  background-color: #f8f9fa;
  padding: 1em;
}

.copyright {
  float: left;
}

.social {
  float: right;
}

.social ul {
  list-style-type: none;
  padding: 0;
}

.social li {
  display: inline-block;
  margin-right: 1em;
}

.social a {
  padding: 0.5em 1em;
}

/* Estilos adicionales */

/* Medios */

@media (max-width: 768px) {
  .navbar {
    float: none;
    width: 100%;
  }

  .navbar ul {
    display: block;
  }

  .navbar li {
    display: block;
    margin-right: 0;
  }
}

/* Colores */

:root {
  --primary-color: #007bff;
  --secondary-color: #6c757d;
  --success-color: #28a745;
  --danger-color: #dc3545;
  --warning-color: #ffc107;
  --info-color: #17a2b8;
  --light-color: #f8f9fa;
  --dark-color: #343a40;
}

/* Componentes */

.btn {
  padding: 0.5em 1em;
  border: 1px solid #ccc;
  border-radius: 0.25em;
}

.btn-primary {
  background-color: var(--primary-color);
  color: #fff;
}

.btn-secondary {
  background-color: var(--secondary-color);
  color: #fff;
}

.btn-success {
  background-color: var(--success-color);
  color: #fff;
}

.btn-danger {
  background-color: var(--danger-color);
  color: #fff;
}

.btn-warning {
  background-color: var(--warning-color);
  color: #fff;
}

.btn-info {
  background-color: var(--info-color);
  color: #fff;
}

.btn-light {
  background-color: var(--light-color);
  color: var(--dark-color);
}

.btn-dark {
  background-color: var(--dark-color);
  color: #fff;
}

.btn-link {
  background-color: transparent;
  color: var(--primary-color);
  text-decoration: none;
}

.btn-link:hover {
  color: var(--primary-color);
  text-decoration: underline;
}

.table {
  width: 100%;
  border-collapse: collapse;
}

.table th, .table td {
  border: 1px solid #ccc;
  padding: 0.5em;
}

.table th {
  text-align: left;
}

.table td {
  text-align: right;
}

/* Formularios */

.form-control {
  width: 100%;
  padding: 0.5em;
  border: 1px solid #ccc;
  border-radius: 0.25em;
}

.form-control:focus {
  border-color: var(--primary-color);
}

.form-group {
  margin-bottom: 1em;
}

.form-label {
  display: block;
  margin-bottom: 0.5em;
}

.form-check {
  margin-bottom: 1em;
}

.form-check-input {
  margin-right: 0.5em;
}

/* Animaciones */

.fade-in {
  animation: fadeIn 1s ease-in;
}

@keyframes fadeIn {
  from {
    opacity: 0;
  }

  to {
    opacity: 1;
  }
}

/* Clases de utilidad */

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-muted {
  color: #6c757d;
}

.bg-primary {
  background-color: var(--primary-color);
}

.bg-secondary {
  background-color: var(--secondary-color);
}

.bg-success {
  background-color: var(--success-color);
}

.bg-danger {
  background-color: var(--danger-color);
}

.bg-warning {
  background-color: var(--warning-color);
}

.bg-info {
  background-color: var(--info-color);
}

.bg-light {
  background-color: var(--light-color);
}

.bg-dark {
  background-color: var(--dark-color);
}
```

Este código CSS es un ejemplo de estilo complejo que se puede aplicar a un sitio web. Incluye una variedad de estilos para diferentes elementos, como tipografía, encabezado, contenido, pie de página, colores, componentes, formularios, animaciones y clases de utilidad.

El código está bien organizado y comentado, lo que facilita su lectura y comprensión. También utiliza variables CSS para definir los colores, lo que permite cambiarlos fácilmente en todo el sitio web.