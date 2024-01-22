```cool
clase Árbol {
    atributo altura: entero;
    atributo tipo: cadena;
    atributo hojas: lista;

    método imprimir() {
        imprimir("Altura: ", altura);
        imprimir("Tipo: ", tipo);
        imprimir("Hojas: ", hojas);
    }

    método crecer() {
        altura = altura + 1;
    }

    método añadirHoja(hoja) {
        hojas = hojas + [hoja];
    }
}

clase Pino extends Árbol {
    método imprimir() {
        super.imprimir();
        imprimir("Número de conos: ", conos);
    }

    método producirConos() {
        conos = conos + 1;
    }

    atributo conos: entero;
}

clase Roble extends Árbol {
    método imprimir() {
        super.imprimir();
        imprimir("Número de bellotas: ", bellotas);
    }

    método producirBellotas() {
        bellotas = bellotas + 1;
    }

    atributo bellotas: entero;
}

clase Bosque {
    atributo árboles: lista;

    método añadirÁrbol(árbol) {
        árboles = árboles + [árbol];
    }

    método imprimir() {
        para cada árbol en árboles {
            árbol.imprimir();
        }
    }
}

pino1 = new Pino();
pino1.altura = 10;
pino1.tipo = "pino silvestre";
pino1.añadirHoja("hoja1");
pino1.añadirHoja("hoja2");

pino2 = new Pino();
pino2.altura = 15;
pino2.tipo = "pino piñonero";
pino2.añadirHoja("hoja1");
pino2.añadirHoja("hoja2");

roble1 = new Roble();
roble1.altura = 20;
roble1.tipo = "roble común";
roble1.añadirHoja("hoja1");
roble1.añadirHoja("hoja2");

roble2 = new Roble();
roble2.altura = 25;
roble2.tipo = "roble albar";
roble2.añadirHoja("hoja1");
roble2.añadirHoja("hoja2");

bosque1 = new Bosque();
bosque1.añadirÁrbol(pino1);
bosque1.añadirÁrbol(pino2);
bosque1.añadirÁrbol(roble1);
bosque1.añadirÁrbol(roble2);

bosque1.imprimir();
```

**Explicación del código:**

Este código define tres clases: Árbol, Pino y Roble, y una clase Bosque.

La clase Árbol tiene tres atributos: altura, tipo y hojas. También tiene tres métodos: imprimir(), crecer() y añadirHoja().

La clase Pino hereda de la clase Árbol y añade un atributo conos y un método producirConos().

La clase Roble hereda de la clase Árbol y añade un atributo bellotas y un método producirBellotas().

La clase Bosque tiene un atributo árboles y un método añadirÁrbol().

El código crea dos objetos de la clase Pino, dos objetos de la clase Roble y un objeto de la clase Bosque. Luego, añade los objetos de las clases Pino y Roble al objeto de la clase Bosque.

Por último, llama al método imprimir() del objeto de la clase Bosque para imprimir la información de todos los árboles del bosque.

**Salida del código:**

```
Altura: 10
Tipo: pino silvestre
Hojas: [hoja1, hoja2]
Número de conos: 0
Altura: 15
Tipo: pino piñonero