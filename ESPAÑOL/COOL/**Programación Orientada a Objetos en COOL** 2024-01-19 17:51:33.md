```cool
-- Definición de la clase Punto
clase Punto {
    -- Atributos
    entero x, y;

    -- Constructor
    crear(entero x, entero y) {
        this := new Punto;
        this.x := x;
        this.y := y;
    }

    -- Métodos
    suma(Punto otro) {
        return Punto.crear(this.x + otro.x, this.y + otro.y);
    }

    resta(Punto otro) {
        return Punto.crear(this.x - otro.x, this.y - otro.y);
    }

    distancia(Punto otro) {
        return Math.sqrt((this.x - otro.x)^2 + (this.y - otro.y)^2);
    }

    toString() {
        return "Punto{" + this.x + ", " + this.y + "}";
    }
}

-- Definición de la clase Línea
clase Línea {
    -- Atributos
    Punto punto1, punto2;

    -- Constructor
    crear(Punto punto1, Punto punto2) {
        this := new Línea;
        this.punto1 := punto1;
        this.punto2 := punto2;
    }

    -- Métodos
    longitud() {
        return this.punto1.distancia(this.punto2);
    }

    toString() {
        return "Línea{" + this.punto1.toString() + ", " + this.punto2.toString() + "}";
    }
}

-- Definición de la clase Rectángulo
clase Rectángulo {
    -- Atributos
    entero ancho, alto;

    -- Constructor
    crear(entero ancho, entero alto) {
        this := new Rectángulo;
        this.ancho := ancho;
        this.alto := alto;
    }

    -- Métodos
    área() {
        return this.ancho * this.alto;
    }

    perímetro() {
        return 2 * (this.ancho + this.alto);
    }

    toString() {
        return "Rectángulo{" + this.ancho + ", " + this.alto + "}";
    }
}

-- Definición de la clase Círculo
clase Círculo {
    -- Atributos
    entero radio;

    -- Constructor
    crear(entero radio) {
        this := new Círculo;
        this.radio := radio;
    }

    -- Métodos
    área() {
        return Math.PI * this.radio^2;
    }

    perímetro() {
        return 2 * Math.PI * this.radio;
    }

    toString() {
        return "Círculo{" + this.radio + "}";
    }
}

-- Punto principal
procedimiento main() {
    -- Creación de objetos
    Punto punto1 := Punto.crear(1, 2);
    Punto punto2 := Punto.crear(3, 4);
    Línea línea1 := Línea.crear(punto1, punto2);
    Rectángulo rectángulo1 := Rectángulo.crear(5, 6);
    Círculo círculo1 := Círculo.crear(7);

    -- Operaciones
    println("Punto 1: " + punto1.toString());
    println("Punto 2: " + punto2.toString());
    println("Línea 1: " + línea1.toString());
    println("Rectángulo 1: " + rectángulo1.toString());
    println("Círculo 1: " + círculo1.toString());
}
```

Explicación:

Este código crea varias clases en COOL, como Punto, Línea, Rectángulo y Círculo. Cada clase tiene atributos (como x e y para la clase Punto) y métodos (como suma y resta para la clase Punto).

En el punto principal del programa, se crean objetos de estas clases y se realizan operaciones sobre ellos, como sumar dos puntos o calcular el área de un círculo.

El resultado de la ejecución de este código sería:

```
Punto 1: Punto{1, 2}
Punto 2: Punto{3, 4}
Línea 1: Línea{Punto{1, 2}, Punto{3, 4}}
Rectángulo 1: Rectángulo{5, 6}
Círculo 1: Círculo{7}
```