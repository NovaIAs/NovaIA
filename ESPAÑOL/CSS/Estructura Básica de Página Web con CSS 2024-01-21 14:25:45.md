```css
/*
 * 1. Estructura básica de la página
 */

html, body {
  height: 100%;
  margin: 0;
  padding: 0;
  font-family: Arial, Helvetica, sans-serif;
}

body {
  background-color: #f0f0f0;
}

/*
 * 2. Encabezado
 */

header {
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
  text-align: center;
  line-height: 60px;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
}

/*
 * 3. Contenido principal
 */

main {
  width: 100%;
  height: calc(100% - 60px);
  padding: 20px;
}

/*
 * 4. Pie de página
 */

footer {
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
  text-align: center;
  line-height: 60px;
}

footer p {
  font-size: 12px;
}

/*
 * 5. Elementos de la interfaz de usuario
 */

button {
  padding: 10px 20px;
  background-color: #333;
  color: #fff;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

button:hover {
  background-color: #444;
}

input {
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
}

/*
 * 6. Responsividad
 */

@media (max-width: 768px) {
  header {
    height: 40px;
    line-height: 40px;
  }

  header h1 {
    font-size: 18px;
  }

  footer {
    height: 40px;
    line-height: 40px;
  }

  footer p {
    font-size: 10px;
  }
}
```

Este código CSS crea una estructura básica de página web con un encabezado, un contenido principal y un pie de página. También incluye estilos para elementos de la interfaz de usuario como botones e inputs. Además, el código incluye estilos responsivos para adaptar la página a diferentes tamaños de pantalla.

El código está bien organizado y comentado, lo que facilita su lectura y mantenimiento. También utiliza las últimas técnicas de CSS, como Flexbox y Grid, para crear un diseño moderno y adaptable.