```cool
 clase Punto {
      entero x, y;

      crear(entero _x, entero _y) {
            x := _x;
            y := _y;
      }

      cadena a_cadena() {
            cadena cad;
            escribir("(", cad);
            entero_a_cadena(x, cad);
            escribir(", ", cad);
            entero_a_cadena(y, cad);
            escribir(")", cad);
            devolver cad;
      }
 }

 clase Rectangulo {
      Punto superior_izquierdo, inferior_derecho;

      crear(Punto _superior_izquierdo, Punto _inferior_derecho) {
            superior_izquierdo := _superior_izquierdo;
            inferior_derecho := _inferior_derecho;
      }

      entero obtener_ancho() {
            devolver inferior_derecho.x - superior_izquierdo.x;
      }

      entero obtener_alto() {
            devolver inferior_derecho.y - superior_izquierdo.y;
      }

      entero obtener_area() {
            devolver obtener_ancho() * obtener_alto();
      }

      cadena a_cadena() {
            cadena cad;
            escribir("[", cad);
            superior_izquierdo.a_cadena(cad);
            escribir(", ", cad);
            inferior_derecho.a_cadena(cad);
            escribir("]", cad);
            devolver cad;
      }
 }

 clase Programa {
      Rectangulo rectangulo;

      crear() {
            entero x1, y1, x2, y2;
            escribir("Ingrese las coordenadas del punto superior izquierdo del rectángulo (x1, y1): ");
            leer(x1, y1);
            escribir("Ingrese las coordenadas del punto inferior derecho del rectángulo (x2, y2): ");
            leer(x2, y2);
            rectangulo := new Rectangulo(new Punto(x1, y1), new Punto(x2, y2));
      }

      ejecutar() {
            entero ancho, alto, area;
            ancho := rectangulo.obtener_ancho();
            alto := rectangulo.obtener_alto();
            area := rectangulo.obtener_area();
            escribir("El ancho del rectángulo es ", ancho);
            escribir("El alto del rectángulo es ", alto);
            escribir("El área del rectángulo es ", area);
      }
 }

 clase Principal {
      static crear() {
            Programa programa;
            programa := new Programa();
            programa.crear();
            programa.ejecutar();
      }
 }
```

Explicación del código:

* La clase `Punto` representa un punto en el plano, con coordenadas `x` e `y`.
* La clase `Rectangulo` representa un rectángulo en el plano, definido por su punto superior izquierdo y su punto inferior derecho.
* La clase `Programa` crea un rectángulo a partir de las coordenadas proporcionadas por el usuario y luego calcula y muestra el ancho, el alto y el área del rectángulo.
* La clase `Principal` es la clase principal del programa y llama al método `crear` de la clase `Programa` para crear un objeto de tipo `Programa` y ejecutar su método `ejecutar`.

El código utiliza una serie de conceptos de programación avanzada, como clases, objetos, herencia y métodos. También utiliza la biblioteca estándar de COOL para leer y escribir en la consola.