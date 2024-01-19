```
/**
 * Variables
 */

/* Colores */
$color-primario: #FF0000;
$color-secundario: #00FF00;
$color-terciario: #0000FF;

/* Tamaños de fuente */
$fuente-base: 16px;
$fuente-grande: 24px;
$fuente-pequena: 12px;

/* Márgenes y paddings */
$margen-base: 10px;
$margen-grande: 20px;
$margen-pequeno: 5px;

/* Bordes */
$borde-ancho: 1px;
$borde-estilo: sólido;
$borde-color: #000000;

/* Sombras */
$sombra-horizontal: 0px;
$sombra-vertical: 5px;
$sombra-desenfoque: 10px;
$sombra-color: #808080;

/* Transiciones */
$transicion-duracion: 0.5s;
$transicion-facilidad: ease-in-out;

/* Efectos de animación */
$animacion-duracion: 1s;
$animacion-facilidad: ease-in-out;

/**
 * Fuentes
 */

@font-face {
  font-family: 'MiFuente';
  src: url('mif fuente.ttf');
}

/**
 * Selector universal
 */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/**
 * Cuerpo
 */

body {
  font-family: 'MiFuente', sans-serif;
  font-size: $fuente-base;
  background-color: #FFFFFF;
}

/**
 * Encabezado
 */

header {
  width: 100%;
  height: 100px;
  background-color: $color-primario;
  color: #FFFFFF;
}

/**
 * Navegación
 */

nav {
  width: 100%;
  height: 50px;
  background-color: $color-secundario;
  color: #FFFFFF;
}

/**
 * Enlaces de navegación
 */

nav a {
  display: inline-block;
  padding: 10px;
  text-decoration: none;
  color: #FFFFFF;
}

/**
 * Sección principal
 */

main {
  width: 100%;
  height: auto;
  background-color: #FFFFFF;
}

/**
 * Artículos
 */

article {
  width: 100%;
  height: auto;
  background-color: #FFFFFF;
  margin: $margen-base;
  padding: $margen-base;
  border: $borde-ancho $borde-estilo $borde-color;
  border-radius: 5px;
  box-shadow: $sombra-horizontal $sombra-vertical $sombra-desenfoque $sombra-color;
}

/**
 * Título de artículo
 */

article h2 {
  font-size: $fuente-grande;
  margin-bottom: $margen-base;
}

/**
 * Contenido de artículo
 */

article p {
  font-size: $fuente-base;
  margin-bottom: $margen-base;
}

/**
 * Pie de página
 */

footer {
  width: 100%;
  height: 100px;
  background-color: $color-terciario;
  color: #FFFFFF;
}
```

Explicación del código:

* Se definen las variables al inicio del código para facilitar la lectura y el mantenimiento del mismo.
* Se utilizan fuentes personalizadas con la declaración `@font-face`.
* Se aplica el selector universal para resetear todos los estilos por defecto del navegador.
* Se define el estilo del cuerpo de la página.
* Se define el estilo del encabezado de la página.
* Se define el estilo de la navegación de la página.
* Se define el estilo de los enlaces de la navegación.
* Se define el estilo de la sección principal de la página.
* Se define el estilo de los artículos.
* Se define el estilo del título de los artículos.
* Se define el estilo del contenido de los artículos.
* Se define el estilo del pie de página de la página.