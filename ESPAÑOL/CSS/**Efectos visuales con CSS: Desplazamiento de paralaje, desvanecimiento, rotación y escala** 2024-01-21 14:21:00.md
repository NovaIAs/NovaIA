```css
/* Códigos para crear un efecto de desplazamiento de paralaje en la página web */

/**
 * 1. Crear un contenedor para el contenido.
 */
.paralaje {
  position: relative;
  overflow: hidden;
}

/**
 * 2. Crear una capa de fondo que se mueva más lentamente que el contenido.
 */
.paralaje-fondo {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-attachment: fixed;
  background-size: cover;
}

/**
 * 3. Crear una capa de contenido que se mueva más rápido que el fondo.
 */
.paralaje-contenido {
  position: relative;
  z-index: 1;
}

/**
 * 4. Aplicar un efecto de animación de desplazamiento de paralaje al contenido.
 */
.paralaje-contenido {
  -webkit-animation: scroll 10s infinite linear;
  animation: scroll 10s infinite linear;
}

/**
 * 5. Definir la animación de desplazamiento de paralaje.
 */
@keyframes scroll {
  0% {
    transform: translateY(0%);
  }
  100% {
    transform: translateY(-50%);
  }
}

/* Códigos para crear un efecto de desvanecimiento en la página web */

/**
 * 1. Crear un contenedor para el contenido.
 */
.desvanecimiento {
  position: relative;
}

/**
 * 2. Crear una capa de fondo que se desvanezca de forma gradual.
 */
.desvanecimiento-fondo {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: #000;
  opacity: 1;
  -webkit-transition: opacity 1s ease-in-out;
  transition: opacity 1s ease-in-out;
}

/**
 * 3. Crear una capa de contenido que se desvanezca de forma gradual.
 */
.desvanecimiento-contenido {
  position: relative;
  z-index: 1;
  opacity: 0;
  -webkit-transition: opacity 1s ease-in-out;
  transition: opacity 1s ease-in-out;
}

/**
 * 4. Aplicar un efecto de animación de desvanecimiento al contenido.
 */
.desvanecimiento:hover .desvanecimiento-fondo {
  opacity: 0;
}

.desvanecimiento:hover .desvanecimiento-contenido {
  opacity: 1;
}

/* Códigos para crear un efecto de rotación en la página web */

/**
 * 1. Crear un contenedor para el contenido.
 */
.rotacion {
  position: relative;
}

/**
 * 2. Crear una capa de fondo que rote de forma gradual.
 */
.rotacion-fondo {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: #000;
  -webkit-transform: rotate(0deg);
  transform: rotate(0deg);
  -webkit-transition: -webkit-transform 1s ease-in-out;
  transition: transform 1s ease-in-out;
}

/**
 * 3. Crear una capa de contenido que rote de forma gradual.
 */
.rotacion-contenido {
  position: relative;
  z-index: 1;
  -webkit-transform: rotate(0deg);
  transform: rotate(0deg);
  -webkit-transition: -webkit-transform 1s ease-in-out;
  transition: transform 1s ease-in-out;
}

/**
 * 4. Aplicar un efecto de animación de rotación al contenido.
 */
.rotacion:hover .rotacion-fondo {
  -webkit-transform: rotate(360deg);
  transform: rotate(360deg);
}

.rotacion:hover .rotacion-contenido {
  -webkit-transform: rotate(-360deg);
  transform: rotate(-360deg);
}

/* Códigos para crear un efecto de escala en la página web */

/**
 * 1. Crear un contenedor para el contenido.
 */
.escala {
  position: relative;
}

/**
 * 2. Crear una capa de fondo que se escale de forma gradual.
 */
.escala-fondo {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: #000;
  -webkit-transform: scale(1);
  transform: scale(1);
  -webkit-transition: -webkit-transform 1s ease-in-out;
  transition: transform 1s ease-in-out;
}

/**
 * 3. Crear una capa de contenido que se escale de forma gradual.
 */
.escala-contenido {
  position: relative;
  z-index: 1;
  -webkit-transform: scale(1);
  transform: scale(1);
  -webkit-transition: -webkit-transform 1s ease-in-out;
  transition: transform 1s ease-in-out;
}

/**
 * 4. Aplicar un efecto de animación de escala al contenido.
 */
.escala:hover .escala-fondo {
  -webkit-transform: scale(2);
  transform: scale(2);
}

.escala:hover .escala-contenido {
  -webkit-transform: scale(0.5);
  transform: scale(0.5);
}
```

**Explicación:**

- El código CSS anterior crea cuatro efectos diferentes en la página web: desplazamiento de paralaje, desvanecimiento, rotación y escala.

- Para cada efecto, se crea un contenedor para el contenido y una capa de fondo y una capa de contenido.

- Se aplica un efecto de animación a la capa de contenido para crear el efecto deseado.

- Por ejemplo, para el efecto de desplazamiento de paralaje, se aplica una animación de desplazamiento vertical a la capa de contenido. Esto hace que la capa de contenido se mueva más rápido que la capa de fondo, creando un efecto de desplazamiento de paralaje.

- De manera similar, para el efecto de desvanecimiento, se aplica una animación de desvanecimiento a la capa de fondo y a la capa de contenido. Esto hace que la capa de fondo se desvanezca de forma gradual y la capa de contenido aparezca de forma gradual.

- Para el efecto de rotación, se aplica una animación de rotación a la capa de fondo y a la capa de contenido. Esto hace que la capa de fondo y la capa de contenido roten de forma gradual.

- Para el efecto de escala, se aplica una animación de escala a la capa de fondo y a la capa de contenido. Esto hace que la capa de fondo y la capa de contenido se escalen de forma gradual.

- El código CSS anterior es un ejemplo de cómo se pueden utilizar las animaciones CSS para crear efectos visuales complejos en una página web.