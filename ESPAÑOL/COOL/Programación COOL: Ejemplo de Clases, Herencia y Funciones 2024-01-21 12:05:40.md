```cool
programa {
  clase A {
    variable a: Entero;
    función foo(b: Boolean) : Entero {
      si (b) {
        a := 10;
      } sino {
        a := 20;
      };
      a;
    };
  };

  clase B : A {
    variable b: Entero;
    función bar(c: Cadena) : Entero {
      a := 0;
      mientras (c != "") {
        si (c[1] == 'a') {
          a := a + 1;
        };
        c := c[2:];
      };
      b := a;
      b;
    };
  };

  clase C : B {
    función baz() : Entero {
      variable d: Entero;
      d := 0;
      mientras (d < 10) {
        si (d % 2 == 0) {
          b := b + d;
        };
        d := d + 1;
      };
      b;
    };
  };

  función main() : Void {
    variable a: A;
    variable b: B;
    variable c: C;

    a := new A();
    b := new B();
    c := new C();

    a.foo(true);
    b.bar("hola mundo");
    c.baz();

    print(a.a);
    print(b.b);
    print(c.b);
  };
}
```

Explicación:

* La clase `A` tiene una variable `a` de tipo `Entero` y una función `foo` que recibe un argumento `b` de tipo `Boolean` y devuelve un valor de tipo `Entero`. La función `foo` asigna el valor 10 a `a` si `b` es `true`, y 20 si `b` es `false`.
* La clase `B` es una subclase de `A` y tiene una variable `b` de tipo `Entero` y una función `bar` que recibe un argumento `c` de tipo `Cadena` y devuelve un valor de tipo `Entero`. La función `bar` asigna el valor 0 a `a` y luego recorre la cadena `c` caracter por caracter. Si un caracter es igual a 'a', la función incrementa el valor de `a`. La función devuelve el valor de `b`.
* La clase `C` es una subclase de `B` y tiene una función `baz` que devuelve un valor de tipo `Entero`. La función `baz` asigna el valor 0 a `d` y luego recorre los números del 0 al 9. Si un número es par, la función suma el número a `b`. La función devuelve el valor de `b`.
* La función `main` es la función principal del programa. La función `main` crea objetos de las clases `A`, `B` y `C`, y luego llama a las funciones `foo`, `bar` y `baz` de los objetos. La función `main` también imprime los valores de las variables `a`, `b` y `c`.