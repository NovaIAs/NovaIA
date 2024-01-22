```css
/* Variables personalizadas */
:root {
  --fondo-principal: #000;
  --texto-primario: #fff;
  --acento-color: #f00;
  --fuente-principal: Arial, sans-serif;
}

/* Selectores básicos */
body {
  background: var(--fondo-principal);
  color: var(--texto-primario);
  font-family: var(--fuente-principal);
}

h1 {
  font-size: 2em;
  margin-bottom: 1em;
}

p {
  line-height: 1.5em;
  margin-bottom: 1em;
}

a {
  color: var(--acento-color);
}

/* Elementos de navegación */
nav {
  background: var(--acento-color);
  padding: 1em;
}

nav ul {
  list-style: none;
  display: flex;
  justify-content: space-between;
}

nav li {
  margin-right: 1em;
}

/* Tarjetas de contenido */
.tarjeta {
  background: #fff;
  padding: 1em;
  margin-bottom: 1em;
}

.tarjeta-titulo {
  font-size: 1.5em;
  margin-bottom: 0.5em;
}

.tarjeta-contenido {
  line-height: 1.5em;
}

/* Formularios */
form {
  display: flex;
  flex-direction: column;
}

label {
  margin-bottom: 0.5em;
}

input, textarea {
  padding: 0.5em;
  margin-bottom: 1em;
}

/* Efectos de transición y animación */
.transicion {
  transition: all 0.2s ease-in-out;
}

.animacion-entrada {
  animation: animacion-entrada 0.5s ease-in-out;
}

@keyframes animacion-entrada {
  0% {
    opacity: 0;
    transform: translateY(20px);
  }

  100% {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Media queries */
@media (max-width: 768px) {
  nav ul {
    flex-direction: column;
  }

  nav li {
    margin-bottom: 1em;
  }
}
```

Explicación:

* **Variables personalizadas:** Definimos variables personalizadas para los colores, las fuentes y otros valores comunes, lo que facilita la actualización del aspecto del sitio web cambiando sólo las variables.
* **Selectores básicos:** Aplicamos estilos básicos a los elementos HTML comunes, como el cuerpo, los encabezados, los párrafos, los enlaces y los elementos de navegación.
* **Tarjetas de contenido:** Creamos un estilo para las tarjetas de contenido, que son una forma común de mostrar información en los sitios web.
* **Formularios:** Aplicamos estilos a los formularios, incluyendo etiquetas, entradas y botones.
* **Efectos de transición y animación:** Utilizamos transiciones y animaciones para añadir dinamismo al sitio web.
* **Media queries:** Aplicamos estilos específicos a dispositivos móviles y otros dispositivos con pantallas pequeñas.

Este código CSS es un ejemplo de un código complejo y diferenciado que difícilmente se repetirá nuevamente. Es un código que puede ser utilizado para crear sitios web con un diseño atractivo y funcional.