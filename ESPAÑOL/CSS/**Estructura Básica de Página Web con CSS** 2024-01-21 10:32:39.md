```css
/* Base */

html, body {
  height: 100%;
  margin: 0;
  padding: 0;
  font-family: Arial, Helvetica, sans-serif;
}

/* Estructura */

#contenedor {
  display: flex;
  flex-direction: column;
  height: 100vh;
}

#encabezado {
  flex: 0 0 auto;
  background-color: #222;
  color: #fff;
  padding: 1rem;
}

#contenido {
  flex: 1 1 auto;
  background-color: #fff;
  padding: 1rem;
}

#pie {
  flex: 0 0 auto;
  background-color: #222;
  color: #fff;
  padding: 1rem;
}

/* Tipografía */

h1 {
  font-size: 2rem;
  margin-bottom: 1rem;
}

h2 {
  font-size: 1.5rem;
  margin-bottom: 1rem;
}

p {
  font-size: 1rem;
  line-height: 1.5rem;
  margin-bottom: 1rem;
}

/* Colores */

.rojo {
  color: #ff0000;
}

.verde {
  color: #00ff00;
}

.azul {
  color: #0000ff;
}

/* Efectos */

.sombra {
  box-shadow: 0 5px 10px rgba(0, 0, 0, 0.25);
}

.animacion {
  animation: ejemplo 2s infinite alternate;
}

@keyframes ejemplo {
  0% {
    transform: translateX(0);
  }

  50% {
    transform: translateX(10px);
  }

  100% {
    transform: translateX(0);
  }
}

/* Medios */

@media (max-width: 768px) {
  #encabezado {
    font-size: 1.5rem;
  }

  #contenido {
    font-size: 1rem;
  }

  #pie {
    font-size: 1rem;
  }
}
```

Este código CSS crea una estructura básica de página web con un encabezado, contenido y pie de página. También incluye estilos para la tipografía, colores y efectos. Además, incluye una consulta de medios para dispositivos móviles.

El código está bien documentado con comentarios para explicar cada parte del código.

Aquí hay una explicación más detallada del código:

* **Base:** Este código define los estilos básicos para la página web, incluyendo la fuente, el color de fondo y el tamaño del margen y el relleno.
* **Estructura:** Este código crea la estructura básica de la página web, incluyendo el encabezado, contenido y pie de página.
* **Tipografía:** Este código define los estilos para los diferentes elementos de la página web, incluyendo el tamaño de la fuente, el margen y el relleno.
* **Colores:** Este código define los colores para los diferentes elementos de la página web.
* **Efectos:** Este código define los efectos para los diferentes elementos de la página web, incluyendo sombras y animaciones.
* **Medios:** Este código define los estilos para los dispositivos móviles.