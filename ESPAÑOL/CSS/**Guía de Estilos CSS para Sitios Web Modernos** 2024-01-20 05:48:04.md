```css
/* Definimos las variables CSS */
:root {
  --color-primario: #2196F3;
  --color-secundario: #4CAF50;
  --color-texto: #222;
  --fuente-primaria: "Arial", sans-serif;
  --fuente-secundaria: "Courier New", monospace;
}

/* Aplicamos las variables CSS a los elementos HTML */
body {
  background-color: var(--color-primario);
  color: var(--color-texto);
  font-family: var(--fuente-primaria);
}

h1 {
  color: var(--color-secundario);
  font-family: var(--fuente-secundaria);
  font-size: 2.5rem;
}

p {
  font-size: 1.2rem;
  line-height: 1.5rem;
}

a {
  color: var(--color-secundario);
  text-decoration: none;
}

/* Creamos una clase para los botones */
.button {
  background-color: var(--color-primario);
  color: var(--color-texto);
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

/* Aplicamos la clase "button" a los elementos HTML */
.button-primario {
  background-color: var(--color-primario);
}

.button-secundario {
  background-color: var(--color-secundario);
}

/* Aplicamos estilos a los elementos de la interfaz de usuario */
.form-group {
  margin-bottom: 15px;
}

.form-control {
  width: 100%;
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
}

.form-control:focus {
  border-color: var(--color-primario);
}

.error {
  color: red;
}

/* Aplicamos estilos a los elementos del menú */
.menu {
  background-color: var(--color-primario);
  padding: 10px;
}

.menu-item {
  display: inline-block;
  margin-right: 10px;
}

.menu-item a {
  color: var(--color-texto);
  text-decoration: none;
}

.menu-item a:hover {
  color: var(--color-secundario);
}

/* Aplicamos estilos a los elementos del pie de página */
.footer {
  background-color: var(--color-primario);
  color: var(--color-texto);
  padding: 10px;
}

.footer-text {
  font-size: 0.8rem;
  text-align: center;
}

/* Aplicamos estilos a los elementos de la página de inicio */
.home-banner {
  background-image: url("banner.jpg");
  background-size: cover;
  background-position: center;
  height: 400px;
}

.home-banner-text {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  color: var(--color-texto);
  text-align: center;
}

.home-banner-text h1 {
  font-size: 3rem;
}

.home-banner-text p {
  font-size: 1.5rem;
}

/* Aplicamos estilos a los elementos de la página de productos */
.product-list {
  display: flex;
  flex-wrap: wrap;
}

.product-item {
  flex: 1;
  padding: 10px;
  margin: 10px;
  background-color: #fff;
  border: 1px solid #ccc;
  border-radius: 5px;
}

.product-item-image {
  width: 100%;
  Height: 200px;
}

.product-item-info {
  padding: 10px;
}

.product-item-title {
  font-size: 1.2rem;
}

.product-item-price {
  font-size: 1.5rem;
  color: var(--color-primario);
}

/* Aplicamos estilos a los elementos del carrito de compras */
.cart-table {
  width: 100%;
  border-collapse: collapse;
}

.cart-table th,
.cart-table td {
  padding: 10px;
  border: 1px solid #ccc;
}

.cart-table th {
  text-align: left;
}

.cart-table td {
  text-align: right;
}

.cart-table tfoot {
  font-weight: bold;
}

/* Aplicamos estilos a los elementos del formulario de pago */
.payment-form {
  width: 100%;
}

.payment-form-group {
  margin-bottom: 15px;
}

.payment-form-control {
  width: 100%;
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
}

.payment-form-control:focus {
  border-color: var(--color-primario);
}
```

Explicación:

* Utilizamos variables CSS para almacenar los valores comunes, como los colores y las fuentes. Esto hace que sea más fácil cambiar el aspecto del sitio web simplemente cambiando los valores de las variables.
* Aplicamos las variables CSS a los elementos HTML para aplicarles los estilos deseados.
* Creamos una clase para los botones y aplicamos estilos a los elementos HTML que utilizan la clase. Esto nos permite crear botones con un aspecto consistente en todo el sitio web.
* Aplicamos estilos a los elementos de la interfaz de usuario, como los formularios y los menús, para que tengan un aspecto más atractivo y fácil de usar.
* Aplicamos estilos a los elementos del pie de página para que tengan un aspecto consistente con el resto del sitio web.
* Aplicamos estilos a los elementos de la página de inicio, como el banner y el texto del banner, para que tengan un aspecto atractivo e informativo.
* Aplicamos estilos a los elementos de la página de productos, como la lista de productos y los elementos de los productos, para que tengan un aspecto organizado y fácil de navegar.
* Aplicamos estilos a los elementos del carrito de compras, como la tabla del carrito y los elementos de la tabla, para que tengan un aspecto organizado y fácil de leer.
* Aplicamos estilos a los elementos del formulario de pago, como los grupos de formularios y los elementos de los formularios, para que tengan un aspecto organizado y fácil de usar.