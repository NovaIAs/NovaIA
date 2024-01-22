```cool
clase A {
  atributo x: Int;
  atributo y: String;

  método imprimir() {
    IO.escribir_linea(self.x.valor + " " + self.y.valor);
  }
}

clase B {
  atributo a: A;
  atributo b: Int;

  método imprimir() {
    IO.escribir_linea(self.a.x.valor + " " + self.a.y.valor + " " + self.b.valor);
  }
}

programa principal {
  var a: A;
  var b: B;

  a := new A;
  a.x := 10;
  a.y := "Hola Mundo";

  b := new B;
  b.a := a;
  b.b := 20;

  a.imprimir();
  b.imprimir();
}
```

Este código crea dos clases, `A` y `B`, que tienen atributos y métodos diferentes. La clase `A` tiene dos atributos, `x` e `y`, y un método llamado `imprimir()` que imprime los valores de sus atributos. La clase `B` tiene dos atributos, `a` y `b`, y un método llamado `imprimir()` que imprime los valores de sus atributos, así como el valor del atributo `x` de su atributo `a`.

El programa principal crea una instancia de la clase `A` y una instancia de la clase `B`, y luego llama al método `imprimir()` de cada instancia para imprimir sus valores.