```javascript
// Importar las bibliotecas necesarias

// Definir la clase Cuadrado
class Cuadrado {
  constructor(lado) {
    this.lado = lado;
  }

  // Calcular el área del cuadrado
  calcularArea() {
    return this.lado * this.lado;
  }

  // Calcular el perímetro del cuadrado
  calcularPerimetro() {
    return 4 * this.lado;
  }

  // Dibujar el cuadrado en una página web
  dibujar() {
    // Crear un elemento div
    const div = document.createElement("div");

    // Establecer el estilo del div
    div.style.width = `${this.lado}px`;
    div.style.height = `${this.lado}px`;
    div.style.backgroundColor = "red";

    // Añadir el div a la página web
    document.body.appendChild(div);
  }
}

// Crear una instancia de la clase Cuadrado
const cuadrado = new Cuadrado(100);

// Calcular el área del cuadrado
const area = cuadrado.calcularArea();

// Calcular el perímetro del cuadrado
const perimetro = cuadrado.calcularPerimetro();

// Dibujar el cuadrado en la página web
cuadrado.dibujar();

// Mostrar el área y el perímetro del cuadrado en la consola
console.log(`Área del cuadrado: ${area}px²`);
console.log(`Perímetro del cuadrado: ${perimetro}px`);
```

**Explicación del código:**

1. El código comienza importando las bibliotecas necesarias. En este caso, no se importan bibliotecas externas.
2. Se define la clase `Cuadrado`, que representa un cuadrado. La clase tiene un constructor que recibe el lado del cuadrado como argumento y lo almacena en la propiedad `lado`.
3. La clase `Cuadrado` también tiene tres métodos:

    * `calcularArea()`: Calcula el área del cuadrado.
    * `calcularPerimetro()`: Calcula el perímetro del cuadrado.
    * `dibujar()`: Dibuja el cuadrado en una página web.

4. Se crea una instancia de la clase `Cuadrado` con un lado de 100 píxeles.
5. Se calculan el área y el perímetro del cuadrado.
6. Se dibuja el cuadrado en la página web.
7. Se muestran el área y el perímetro del cuadrado en la consola.