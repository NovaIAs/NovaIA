```javascript
// Objetivo: crear una función en JavaScript para calcular el área de una figura geométrica, donde la figura geométrica puede ser un círculo, un cuadrado o un triángulo.

// Función principal: Calcula el área de una figura geométrica.
function calcularArea(figura, dimensiones) {
  // Verificamos qué figura geométrica se está calculando.
  switch (figura) {
    case "círculo":
      // Cálculo del área de un círculo.
      return Math.PI * dimensiones.radio ** 2;

    case "cuadrado":
      // Cálculo del área de un cuadrado.
      return dimensiones.lado ** 2;

    case "triángulo":
      // Cálculo del área de un triángulo.
      return (dimensiones.base * dimensiones.altura) / 2;

    default:
      // Si la figura geométrica no es reconocida, se devuelve un mensaje de error.
      return "Figura geométrica no reconocida.";
  }
}

// Ejemplo de uso de la función para calcular el área de un círculo.
const areaCirculo = calcularArea("círculo", { radio: 5 });
console.log(`Área del círculo: ${areaCirculo} unidades cuadradas`);

// Ejemplo de uso de la función para calcular el área de un cuadrado.
const areaCuadrado = calcularArea("cuadrado", { lado: 4 });
console.log(`Área del cuadrado: ${areaCuadrado} unidades cuadradas`);

// Ejemplo de uso de la función para calcular el área de un triángulo.
const areaTriangulo = calcularArea("triángulo", { base: 6, altura: 8 });
console.log(`Área del triángulo: ${areaTriangulo} unidades cuadradas`);
```

Explicación del código:

* La función `calcularArea` toma dos parámetros: `figura` y `dimensiones`. El parámetro `figura` especifica la figura geométrica cuya área se quiere calcular, mientras que el parámetro `dimensiones` es un objeto que contiene las dimensiones específicas de la figura geométrica.
* Dentro de la función, se utiliza un `switch` para determinar qué figura geométrica se está calculando.
* Cada caso dentro del `switch` contiene la fórmula para calcular el área de la figura geométrica correspondiente.
* Por último, se utilizan ejemplos para demostrar cómo utilizar la función para calcular el área de un círculo, un cuadrado y un triángulo.