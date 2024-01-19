```css
/* Restablecer estilos por defecto */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Contenedor principal */
.contenedor-principal {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

/* Elemento hijo del contenedor principal */
.elemento-hijo {
  width: 200px;
  height: 200px;
  background-color: #8350A2;
  margin: 10px;
  border-radius: 50%;
  animation: rotacion 5s infinite linear;
}

/* Animación de rotación */
@keyframes rotacion {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}

/* Medias Queries */
@media (max-width: 768px) {
  .elemento-hijo {
    width: 150px;
    height: 150px;
  }
}

@media (min-width: 1200px) {
  .elemento-hijo {
    width: 250px;
    height: 250px;
  }
}
```

Explicación:

* El código CSS anterior crea un contenedor principal que ocupa toda la altura de la ventana (`height: 100vh`).
* Dentro del contenedor principal, se crea un elemento hijo (`elemento-niño`) que es un círculo de color púrpura (#8350A2) de 200 píxeles de ancho y alto.
* El círculo está animado para rotar continuamente utilizando la animación `rotacion`, que dura 5 segundos y se repite indefinidamente.
* El código también incluye algunas consultas de medios para ajustar el tamaño del círculo en función del ancho de la ventana.

Este código es complejo porque combina múltiples conceptos de CSS, incluyendo el sistema de cuadrícula flexible (`flexbox`), las animaciones y las consultas de medios.