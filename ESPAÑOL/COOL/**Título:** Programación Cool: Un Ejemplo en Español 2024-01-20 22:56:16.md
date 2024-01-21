```cool
clase Programa {
  -- Variables globales y constantes.
  entero x = 0;
  cadena y = "Hola, mundo!";
  boolean z = falso;

  -- Método principal.
  método iniciar() {
    x := 10;
    y := "Adiós, mundo!";
    z := verdadero;

    -- Muestra los valores de las variables.
    escribir_cadena(y);
    escribir_entero(x);
    escribir_booleano(z);

    -- Crea un objeto de la clase Punto.
    Punto p = new Punto(10, 20);

    -- Llama a un método del objeto.
    entero distancia = p.distancia_desde_origen();

    -- Muestra el resultado del método.
    escribir_entero(distancia);

    -- Termina el programa.
    detener();
  }
}

clase Punto {
  -- Variables de instancia.
  entero x;
  entero y;

  -- Constructor.
  método Punto(entero x, entero y) {
    this.x = x;
    this.y = y;
  }

  -- Método para calcular la distancia desde el origen.
  método distancia_desde_origen(): entero {
    entero dx = this.x - 0;
    entero dy = this.y - 0;

    return dx * dx + dy * dy;
  }
}
```

Explicación del código:

* La clase `Programa` define el método `iniciar` que es el punto de entrada del programa.

* La clase `Punto` define un constructor y un método para calcular la distancia desde el origen.

* El método `iniciar` de la clase `Programa` crea un objeto de la clase `Punto`, llama a un método del objeto y muestra el resultado del método.

* La clase `Punto` define un constructor que inicializa las variables de instancia `x` e `y` del objeto.

* El método `distancia_desde_origen` de la clase `Punto` calcula la distancia desde el origen utilizando el teorema de Pitágoras.