```cool
clase Padre {
    atributos Int num;
    métodos {
        void ejemplo() {
            num := 10;  // Asignación de valor al atributo num
            Int suma := num + 20;  // Operación aritmética con el atributo num
            if (suma > 30) {
                print("Suma mayor a 30");
            } else {
                print("Suma menor o igual a 30");
            }
        }
    }
}

clase Hijo : Padre {
    atributos Int num2;
    métodos {
        void ejemplo2() {
            num2 := 5;  // Asignación de valor al atributo num2
            Int producto := num * num2;  // Operación aritmética con los atributos num y num2
            if (producto > 50) {
                print("Producto mayor a 50");
            } else {
                print("Producto menor o igual a 50");
            }
        }
    }
}

clase Principal {
    métodos {
        void main() {
            Padre padre = new Padre();  // Creación de un objeto de la clase Padre
            padre.ejemplo();  // Llamada al método ejemplo() de la clase Padre

            Hijo hijo = new Hijo();  // Creación de un objeto de la clase Hijo
            hijo.ejemplo();  // Llamada al método ejemplo() de la clase Hijo
            hijo.ejemplo2();  // Llamada al método ejemplo2() de la clase Hijo
        }
    }
}
```

Explicación:

- La clase `Padre` define un atributo `num` y un método `ejemplo()`. El método `ejemplo()` asigna un valor al atributo `num`, realiza una operación aritmética con el atributo `num`, y muestra el resultado de la operación.


- La clase `Hijo` hereda de la clase `Padre` y define un atributo `num2` y un método `ejemplo2()`. El método `ejemplo2()` asigna un valor al atributo `num2`, realiza una operación aritmética con los atributos `num` y `num2`, y muestra el resultado de la operación.


- La clase `Principal` define un método `main()`. El método `main()` crea objetos de las clases `Padre` y `Hijo`, y llama a los métodos `ejemplo()` y `ejemplo2()` de cada objeto.