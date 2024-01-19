```cool
clase A {
	nombre : String;
	edad : Entero;

	constructora(nombre : String, edad : Entero) {
		self.nombre := nombre;
		self.edad := edad;
	}

	imprimir() {
		print(self.nombre);
		print(self.edad);
	}
}

clase B hereda A {
	promedio : Real;

	constructora(nombre : String, edad : Entero, promedio : Real) {
		super(nombre, edad);
		self.promedio := promedio;
	}

	imprimir() {
		super.imprimir();
		print(self.promedio);
	}
}

clase C hereda B {
	habilidades : Lista[String];

	constructora(nombre : String, edad : Entero, promedio : Real, habilidades : Lista[String]) {
		super(nombre, edad, promedio);
		self.habilidades := habilidades;
	}

	imprimir() {
		super.imprimir();
		print("Habilidades:");
		para (habilidad en self.habilidades) {
			print(habilidad);
		}
	}
}

objeto a = new A("Juan", 20);
objeto b = new B("María", 22, 9.5);
objeto c = new C("Pedro", 24, 8.7, ["programar", "diseñar", "escribir"]);

a.imprimir();
b.imprimir();
c.imprimir();
```

Explicación del código:

* La clase `A` es una clase base que define las propiedades y métodos comunes de todos los objetos que heredan de ella. En este caso, la clase `A` tiene dos propiedades: `nombre` y `edad`, y un método `imprimir()` que imprime el nombre y la edad del objeto.
* La clase `B` hereda de la clase `A` y añade una nueva propiedad llamada `promedio`. El constructor de la clase `B` llama al constructor de la clase `A` para inicializar las propiedades heredadas, y luego inicializa la propiedad `promedio`. El método `imprimir()` de la clase `B` llama al método `imprimir()` de la clase `A` y luego imprime el valor de la propiedad `promedio`.
* La clase `C` hereda de la clase `B` y añade una nueva propiedad llamada `habilidades`. El constructor de la clase `C` llama al constructor de la clase `B` para inicializar las propiedades heredadas, y luego inicializa la propiedad `habilidades`. El método `imprimir()` de la clase `C` llama al método `imprimir()` de la clase `B` y luego imprime el valor de la propiedad `habilidades`.
* Los objetos `a`, `b` y `c` son instancias de las clases `A`, `B` y `C`, respectivamente. Estos objetos se crean utilizando la palabra clave `new`.
* El método `imprimir()` de cada objeto se llama para imprimir los valores de las propiedades del objeto.