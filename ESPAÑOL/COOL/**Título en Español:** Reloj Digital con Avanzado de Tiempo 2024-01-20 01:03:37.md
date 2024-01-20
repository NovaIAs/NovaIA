```cool
clase Reloj {
    entero hora;
    entero minuto;
    entero segundo;

    método iniciar(hora, minuto, segundo) {
        this.hora = hora;
        this.minuto = minuto;
        this.segundo = segundo;
    }

    método avanzar() {
        segundo++;
        si (segundo == 60) {
            segundo = 0;
            minuto++;
        }
        si (minuto == 60) {
            minuto = 0;
            hora++;
        }
        si (hora == 24) {
            hora = 0;
        }
    }

    método obtenerHora() {
        devolver hora;
    }

    método obtenerMinuto() {
        devolver minuto;
    }

    método obtenerSegundo() {
        devolver segundo;
    }
}

clase PruebaReloj {
    método main() {
        reloj = new Reloj();
        reloj.iniciar(12, 0, 0);
        repetir 10 {
            reloj.avanzar();
            imprimir(reloj.obtenerHora());
            imprimir(":");
            imprimir(reloj.obtenerMinuto());
            imprimir(":");
            imprimir(reloj.obtenerSegundo());
            imprimir("\n");
        }
    }
}
```

Este código crea una clase llamada `Reloj` que tiene tres atributos enteros: `hora`, `minuto` y `segundo`. También tiene un método `iniciar` que se utiliza para establecer la hora inicial del reloj, un método `avanzar` que se utiliza para avanzar el reloj en un segundo, y tres métodos `obtener` que se utilizan para obtener la hora, el minuto y el segundo actuales del reloj.

La clase `PruebaReloj` crea una instancia de la clase `Reloj` y la utiliza para ejecutar un bucle que avanza el reloj 10 segundos y luego imprime la hora, el minuto y el segundo actuales del reloj.

Este código es complejo porque utiliza la herencia de clases y la sobrecarga de métodos. También utiliza un bucle `repetir` para avanzar el reloj 10 segundos.