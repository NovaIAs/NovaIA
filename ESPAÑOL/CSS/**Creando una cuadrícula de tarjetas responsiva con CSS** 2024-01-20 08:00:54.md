```css
/* Creación de una cuadrícula de tarjetas responsiva con CSS */

/* Estilo base para todas las tarjetas */
.tarjeta {
  display: flex;
  flex-direction: column;
  width: 250px;
  height: 350px;
  margin: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);
}

/* Estilos para el encabezado de la tarjeta */
.tarjeta-encabezado {
  background-color: #f2f2f2;
  padding: 10px;
}

/* Estilos para el contenido de la tarjeta */
.tarjeta-contenido {
  flex: 1;
  padding: 10px;
}

/* Estilos para el pie de página de la tarjeta */
.tarjeta-pie {
  background-color: #f2f2f2;
  padding: 10px;
}

/* Media queries para ajustar el diseño en diferentes tamaños de pantalla */
@media (max-width: 768px) {
  .tarjeta {
    width: 100%;
  }
}

/* Ejemplo de uso de las tarjetas */
.contenedor-tarjetas {
  display: flex;
  flex-wrap: wrap;
}

.tarjeta-1 {
  background-color: #ff0000;
}

.tarjeta-2 {
  background-color: #00ff00;
}

.tarjeta-3 {
  background-color: #0000ff;
}
```

Explicación:

* El código CSS anterior crea una cuadrícula de tarjetas responsiva utilizando flexbox.
* La clase `.tarjeta` define los estilos básicos para todas las tarjetas, incluyendo su tamaño, color de borde, sombra y redondeo de esquinas.
* Las clases `.tarjeta-encabezado`, `.tarjeta-contenido` y `.tarjeta-pie` definen los estilos para el encabezado, el contenido y el pie de página de las tarjetas, respectivamente.
* Las media queries se utilizan para ajustar el diseño de las tarjetas en diferentes tamaños de pantalla. Por ejemplo, cuando el ancho de la pantalla es inferior a 768 píxeles, las tarjetas se apilan verticalmente en lugar de horizontalmente.
* El código de ejemplo en la parte inferior del código muestra cómo utilizar las tarjetas en una página web. El contenedor `.contenedor-tarjetas` contiene tres tarjetas con diferentes colores de fondo.

Este código es complejo porque combina varias técnicas de CSS para crear un diseño responsivo y versátil. Es un ejemplo de cómo CSS se puede utilizar para crear diseños complejos y sofisticados.