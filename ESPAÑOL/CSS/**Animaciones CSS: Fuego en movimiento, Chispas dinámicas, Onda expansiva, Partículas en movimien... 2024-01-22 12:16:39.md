```css
/* Creación de una colorida animación de llama en CSS */

@keyframes llama-animacion {
  from {
    background-color: #ff3838;
    transform: scale(1);
  }

  to {
    background-color: #ff9b38;
    transform: scale(1.5);
  }
}

#llama-contenedor {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#llama-fuego {
  position: absolute;
  width: 20px;
  height: 20px;
  border-radius: 50%;
  background-color: #ff3838;
  animation-name: llama-animacion;
  animation-duration: 1.5s;
  animation-iteration-count: infinite;
  animation-timing-function: ease-in-out;
}

/* Creación de chispas dinámicas en CSS */

@keyframes chispa-animacion {
  0% {
    opacity: 1;
    transform: scale(1);
  }

  50% {
    opacity: 0.5;
    transform: scale(1.5);
  }

  100% {
    opacity: 0;
    transform: scale(2);
  }
}

#chispa-contenedor {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#chispa {
  position: absolute;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background-color: #ffc100;
  animation-name: chispa-animacion;
  animation-duration: 1s;
  animation-iteration-count: infinite;
  animation-timing-function: ease-in-out;
}

/* Posicionamiento aleatorio de las chispas */

.chispa-aleatoria {
  left: calc(rand() * 100%);
  top: calc(rand() * 100%);
  animation-delay: calc(rand() * 2s);
}

/* Creación de un efecto de onda expansiva en CSS */

@keyframes onda-animacion {
  from {
    transform: scale(0);
    opacity: 1;
  }

  to {
    transform: scale(1.5);
    opacity: 0;
  }
}

#onda-contenedor {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#onda {
  position: absolute;
  width: 100px;
  height: 100px;
  border-radius: 50%;
  background-color: #0080ff;
  animation-name: onda-animacion;
  animation-duration: 1s;
  animation-iteration-count: infinite;
  animation-timing-function: ease-in-out;
}

/* Creación de un efecto de partículas en movimiento en CSS */

@keyframes particula-animacion {
  0% {
    transform: translate(0, 0);
  }

  50% {
    transform: translate(calc(rand() * 100px), calc(rand() * 100px));
  }

  100% {
    transform: translate(0, 0);
  }
}

#particula-contenedor {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#particula {
  position: absolute;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background-color: #ffffff;
  animation-name: particula-animacion;
  animation-duration: 3s;
  animation-iteration-count: infinite;
  animation-timing-function: linear;
}

/* Creación de una animación de texto que escribe en CSS */

@keyframes texto-animacion {
  from {
    width: 0;
  }

  to {
    width: 100%;
  }
}

#texto-contenedor {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#texto {
  position: absolute;
  width: 0;
  height: 50px;
  border-bottom: 2px solid #ffffff;
  animation-name: texto-animacion;
  animation-duration: 2s;
  animation-iteration-count: infinite;
  animation-timing-function: ease-in-out;
}

/* Creación de una animación de texto que aparece y desaparece en CSS */

@keyframes texto-aparece-desaparece {
  0% {
    opacity: 1;
  }

  50% {
    opacity: 0;
  }

  100% {
    opacity: 1;
  }
}

#texto-contenedor-aparece-desaparece {
  position: relative;
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 300px;
}

#texto-aparece-desaparece {
  position: absolute;
  width: 200px;
  height: 50px;
  text-align: center;
  color: #ffffff;
  animation-name: texto-aparece-desaparece;
  animation-duration: 3s;
  animation-iteration-count: infinite;
  animation-timing-function: ease-in-out;
}
```

Este código CSS crea cinco animaciones diferentes:

1. Llama en movimiento: Una llama en movimiento que cambia de color y tamaño.
2. Chispas dinámicas: Chispas que aparecen y desaparecen aleatoriamente en la pantalla.
3. Onda expansiva: Una onda expansiva que se propaga desde el centro de la pantalla.
4. Partículas en movimiento: Partículas que se mueven aleatoriamente por la pantalla.
5. Texto que escribe: Un texto que escribe su contenido letra a letra.
6. Texto que aparece y desaparece: Un texto que aparece y desaparece periódicamente.

Todas las animaciones son creadas utilizando keyframes y propiedades de animación CSS. Las animaciones se aplican a elementos HTML mediante la propiedad `animation-name`. El código utiliza `calc()` y `rand()` para generar valores aleatorios para el posicionamiento y el movimiento de los elementos animados.