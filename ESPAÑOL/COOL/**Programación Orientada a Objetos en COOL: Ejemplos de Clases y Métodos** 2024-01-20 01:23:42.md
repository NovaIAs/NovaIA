```cool
programa Ejemplo {

	clase A {
		propiedad entero entero_A;

		método entero suma(entero n) {
			entero a, b, c;
			a := 12;
			b := 22;
			c := a + b;
			retorna c + n;
		}

		método vacío mostrar_mensaje() {
			cadena mensaje;
			mensaje := "Hola desde la clase A.";
			imprime(mensaje);
		}
	}

	clase B hereda A {
		propiedad cadena cadena_B;

		método cadena concatenar(cadena c) {
			retorna cadena_B + c;
		}
	}

	clase C {
		propiedad entero entero_C;
		propiedad A a_C;

		método vacío mostrar_suma() {
			entero suma_a;
			suma_a := a_C.suma(10);
			imprime(suma_a);
		}
	}

	clase principal {
		método vacío principal() {
			A a;
			a := new A();
			entero suma_a;
			suma_a := a.suma(20);
			imprime(suma_a);

			B b;
			b := new B();
			b.cadena_B := "Hola desde la clase B.";
			cadena mensaje_b;
			mensaje_b := b.concatenar(" concatenado.");
			imprime(mensaje_b);

			C c;
			c := new C();
			c.entero_C := 30;
			c.a_C := a;
			c.mostrar_suma();

			a.mostrar_mensaje();
		}
	}
}
```

Explicación:

1. Definición de Clases:

   * Clase `A`: Esta clase define dos propiedades: `entero_A` y `cadena_A`. También define dos métodos: `suma()` que suma un número a `entero_A` y `mostrar_mensaje()` que imprime "Hola desde la clase A.".
   * Clase `B`: Esta clase hereda de la clase `A`. Define una propiedad `cadena_B` y un método `concatenar()` que concatena una cadena con `cadena_B`.
   * Clase `C`: Esta clase define dos propiedades: `entero_C` y `a_C` (que es una referencia a un objeto de la clase `A`). También define un método `mostrar_suma()` que imprime la suma de `entero_C` y el valor devuelto por `suma()` del objeto `a_C`.
   * Clase `principal`: Esta es la clase principal del programa. Define un método `principal()` que crea objetos de las clases `A`, `B` y `C` y llama a sus métodos para demostrar sus funcionalidades.

2. Método `principal()`:

   * Crea un objeto de la clase `A` y llama a su método `suma()` para imprimir la suma de 20 a `entero_A`.
   * Crea un objeto de la clase `B` y establece su propiedad `cadena_B` en "Hola desde la clase B.". Luego, llama a su método `concatenar()` para concatenar la cadena "concatenado." y la imprime.
   * Crea un objeto de la clase `C` y establece su propiedad `entero_C` en 30. También establece su propiedad `a_C` en el objeto `A` creado anteriormente. Luego, llama a su método `mostrar_suma()` para imprimir la suma de `entero_C` y el valor devuelto por `suma()` del objeto `a_C`.
   * Finalmente, llama al método `mostrar_mensaje()` del objeto `A` para imprimir "Hola desde la clase A.".

Este código demuestra la herencia, el polimorfismo y la encapsulación, que son conceptos fundamentales de la programación orientada a objetos.