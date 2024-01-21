```cool
clase ClasePrincipal {
    método principal() {
        si (condición) {
            mientras (condición) {
                hacer {
                    secuencia de instrucciones;
                }
                hasta (condición);
            }
        } sino {
            hacer {
                secuencia de instrucciones;
            }
            hasta (condición);
        }
        para (índice = 0; índice < tamaño; índice++) {
            hacer {
                secuencia de instrucciones;
            }
        }
        devolver expresión;
    }
}

clase Subclase : ClasePrincipal {
    método métodoSubclase() {
        hacer {
            secuencia de instrucciones;
        }
    }
}

método funciónGlobal(argumentos) {
    hacer {
        secuencia de instrucciones;
    }
    devolver expresión;
}
```

Explicación:

* La clase `ClasePrincipal` es la clase principal del programa. Contiene el método `principal()` que se ejecuta cuando se ejecuta el programa.
* El método `principal()` contiene una declaración `si` y un `para` que se utilizan para controlar el flujo de ejecución del programa.
* El método `métodoSubclase()` se declara dentro de la subclase `Subclase` que hereda de la clase `ClasePrincipal`.
* El método `funciónGlobal()` se declara fuera de cualquier clase y puede ser llamado desde cualquier lugar del programa.