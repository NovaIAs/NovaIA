```cool
clase Sistema {
	cadena mensaje;

	método nuevo(cadena nuevo_mensaje) {
		mensaje := nuevo_mensaje;
	}

	método obtener_mensaje() {
		return mensaje;
	}
}

clase Principal {
	entero main() {
		clase sistema := new Sistema();

		sistema.nuevo("¡Hola, mundo!");

		cadena mensaje_obtenido := sistema.obtener_mensaje();

		escribir_linea(mensaje_obtenido);

		return 0;
	}
}

clase Lista {
	entero[] elementos;
	entero tamanio;

	método nuevo(entero nuevo_tamanio) {
		elementos := new entero[nuevo_tamanio];
		tamanio := nuevo_tamanio;
	}

	método obtener_tamanio() {
		return tamanio;
	}

	método obtener_elemento(entero indice) {
		return elementos[indice];
	}

	método establecer_elemento(entero indice, entero nuevo_elemento) {
		elementos[indice] := nuevo_elemento;
	}
}

clase MainLista {
	entero main() {
		clase lista := new Lista(5);

		lista.establecer_elemento(0, 1);
		lista.establecer_elemento(1, 2);
		lista.establecer_elemento(2, 3);
		lista.establecer_elemento(3, 4);
		lista.establecer_elemento(4, 5);

		entero tamanio_lista := lista.obtener_tamanio();

		para (entero i = 0; i < tamanio_lista; i++) {
			entero elemento_actual := lista.obtener_elemento(i);

			escribir_linea(elemento_actual);
		}

		return 0;
	}
}
```

Este código COOL es una implementación de una clase `Sistema` que tiene un atributo `mensaje` de tipo cadena. La clase `Sistema` tiene dos métodos: `nuevo` y `obtener_mensaje`. El método `nuevo` recibe una cadena como argumento y asigna ese valor al atributo `mensaje`. El método `obtener_mensaje` devuelve el valor del atributo `mensaje`.

El código también implementa una clase `Lista` que tiene un atributo `elementos` de tipo entero[] y un atributo `tamanio` de tipo entero. La clase `Lista` tiene cuatro métodos: `nuevo`, `obtener_tamanio`, `obtener_elemento` y `establecer_elemento`. El método `nuevo` recibe un entero como argumento y crea un nuevo arreglo de enteros con ese tamaño. El método `obtener_tamanio` devuelve el tamaño del arreglo de enteros. El método `obtener_elemento` recibe un entero como argumento y devuelve el elemento del arreglo de enteros en esa posición. El método `establecer_elemento` recibe dos enteros como argumentos y establece el elemento del arreglo de enteros en la primera posición con el valor de la segunda posición.

El código también implementa una clase `MainLista` que tiene un método `main` que es el punto de entrada del programa. El método `main` crea una instancia de la clase `Lista` con un tamaño de 5. Luego, utiliza el método `establecer_elemento` para establecer los valores de los elementos del arreglo de enteros. A continuación, utiliza el método `obtener_tamanio` para obtener el tamaño del arreglo de enteros. Luego, utiliza un bucle `para` para recorrer el arreglo de enteros y utilizar el método `obtener_elemento` para obtener el valor de cada elemento. Por último, utiliza el método `escribir_linea` para escribir el valor de cada elemento en una nueva línea.

Este código es un ejemplo de cómo se pueden utilizar las clases y los métodos en el lenguaje de programación COOL.