```cool
objeto App {
	método principal(): entero {
		clase Y: entero {
			método inc(): entero {
				return this + 1;
			}
		}

		Y y = new Y();
		return y.inc();
	}
}

objeto Y: entero {
	método inc(): entero {
		return this + 1;
	}
}
```

Explicación:

Este código en COOL crea una clase llamada Y, que tiene un método llamado inc() que devuelve el valor del objeto incrementado en uno.
Luego crea un objeto de la clase Y, llama al método inc() y devuelve el resultado.
La clase Y es un ejemplo de una clase sencilla, con un solo método.
El método inc() es un ejemplo de un método simple, que devuelve un valor.
El código principal crea un objeto de la clase Y y luego llama al método inc() en el objeto.
El resultado se devuelve como el valor de la función principal.