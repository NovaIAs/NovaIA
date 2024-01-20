```javascript
/**
 * Función principal que inicializa la aplicación y agrega manejadores de eventos
 */
function init() {
  // Obtener elementos del DOM
  const form = document.getElementById("form");
  const resultados = document.getElementById("resultados");

  // Agregar manejador de eventos al formulario
  form.addEventListener("submit", (event) => {
    event.preventDefault();

    // Obtener los valores de los campos del formulario
    const numero1 = parseInt(document.getElementById("numero1").value);
    const numero2 = parseInt(document.getElementById("numero2").value);
    const operacion = document.getElementById("operacion").value;

    // Realizar la operación aritmética indicada
    let resultado;
    switch (operacion) {
      case "+":
        resultado = numero1 + numero2;
        break;
      case "-":
        resultado = numero1 - numero2;
        break;
      case "*":
        resultado = numero1 * numero2;
        break;
      case "/":
        resultado = numero1 / numero2;
        break;
      default:
        alert("Operación no reconocida");
        return;
    }

    // Mostrar el resultado en la sección de resultados
    resultados.innerHTML = `El resultado de la operación es: ${resultado}`;
  });
}

// Inicializar la aplicación
init();
```

Explicación del código:

1. La función `init()` se ejecuta cuando se carga la página y realiza las siguientes tareas:
   - Obtiene los elementos del DOM necesarios para el funcionamiento de la aplicación.
   - Agrega un manejador de eventos al formulario para capturar su envío.

2. El manejador de eventos del formulario se ejecuta cuando se envía el formulario y realiza las siguientes tareas:
   - Previene el comportamiento predeterminado del formulario, que es recargar la página.
   - Obtiene los valores de los campos del formulario.
   - Realiza la operación aritmética indicada utilizando una instrucción `switch`.
   - Muestra el resultado de la operación en la sección de resultados.

3. La aplicación se inicializa llamando a la función `init()`.