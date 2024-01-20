```rust
// Definimos una estructura para representar un árbol binario.
#[derive(Debug)]
struct ÁrbolBinario<T> {
    valor: T,
    izquierda: Option<Box<ÁrbolBinario<T>>>,
    derecha: Option<Box<ÁrbolBinario<T>>>>,
}

// Implementamos algunos métodos para el árbol binario.

impl<T: Ord> ÁrbolBinario<T> {
    // Función para insertar un nuevo valor en el árbol.
    fn insertar(&mut self, valor: T) {
        match self.valor.cmp(&valor) {
            std::cmp::Ordering::Less => {
                if let Some(ref mut derecha) = self.derecha {
                    derecha.insertar(valor);
                } else {
                    self.derecha = Some(Box::new(ÁrbolBinario {
                        valor,
                        izquierda: None,
                        derecha: None,
                    }));
                }
            }
            std::cmp::Ordering::Greater => {
                if let Some(ref mut izquierda) = self.izquierda {
                    izquierda.insertar(valor);
                } else {
                    self.izquierda = Some(Box::new(ÁrbolBinario {
                        valor,
                        izquierda: None,
                        derecha: None,
                    }));
                }
            }
            std::cmp::Ordering::Equal => {
                // Si el valor ya existe en el árbol, no lo insertamos.
            }
        }
    }

    // Función para buscar un valor en el árbol.
    fn buscar(&self, valor: T) -> bool {
        match self.valor.cmp(&valor) {
            std::cmp::Ordering::Less => {
                if let Some(ref derecha) = self.derecha {
                    derecha.buscar(valor)
                } else {
                    false
                }
            }
            std::cmp::Ordering::Greater => {
                if let Some(ref izquierda) = self.izquierda {
                    izquierda.buscar(valor)
                } else {
                    false
                }
            }
            std::cmp::Ordering::Equal => {
                true
            }
        }
    }

    // Función para eliminar un valor del árbol.
    fn eliminar(&mut self, valor: T) {
        match self.valor.cmp(&valor) {
            std::cmp::Ordering::Less => {
                if let Some(ref mut derecha) = self.derecha {
                    derecha.eliminar(valor);
                }
            }
            std::cmp::Ordering::Greater => {
                if let Some(ref mut izquierda) = self.izquierda {
                    izquierda.eliminar(valor);
                }
            }
            std::cmp::Ordering::Equal => {
                // Si el valor se encuentra en el nodo actual, lo eliminamos.
                *self = match (self.izquierda.take(), self.derecha.take()) {
                    (None, None) => ÁrbolBinario {
                        valor: valor,
                        izquierda: None,
                        derecha: None,
                    },
                    (Some(izquierda), None) => ÁrbolBinario {
                        valor,
                        izquierda: Some(izquierda),
                        derecha: None,
                    },
                    (None, Some(derecha)) => ÁrbolBinario {
                        valor,
                        izquierda: None,
                        derecha: Some(derecha),
                    },
                    (Some(izquierda), Some(derecha)) => {
                        let mut nodo_minimo = derecha.as_mut().unwrap();
                        while let Some(ref mut izquierda) = nodo_minimo.izquierda {
                            nodo_minimo = izquierda.as_mut().unwrap();
                        }
                        nodo_minimo.izquierda = Some(izquierda);
                        nodo_minimo.derecha = Some(derecha);
                        *nodo_minimo
                    }
                };
            }
        }
    }

    // Función para imprimir el árbol en forma de texto.
    fn imprimir(&self) {
        println!("{:?}", self);
    }
}

// Creamos un árbol binario de enteros.
let mut árbol = ÁrbolBinario {
    valor: 10,
    izquierda: None,
    derecha: None,
};

// Insertamos algunos valores en el árbol.
árbol.insertar(5);
árbol.insertar(15);
árbol.insertar(3);
árbol.insertar(7);
árbol.insertar(12);
árbol.insertar(20);

// Imprimimos el árbol.
árbol.imprimir();

// Buscamos un valor en el árbol.
println!("¿El árbol contiene el valor 12? {}", árbol.buscar(12));

// Eliminamos un valor del árbol.
árbol.eliminar(15);

// Imprimimos el árbol.
árbol.imprimir();
```

Explicación:

* Definimos una estructura `ÁrbolBinario` para representar un árbol binario. La estructura contiene un valor, una referencia a un subárbol izquierdo y una referencia a un subárbol derecho.

* Implementamos algunos métodos para el árbol binario, como `insertar`, `buscar` y `eliminar`. Estos métodos nos permiten manipular el árbol y realizar búsquedas y eliminaciones de valores.

* Creamos un árbol binario de enteros y lo poblamos con algunos valores.

* Imprimimos el árbol en forma de texto utilizando el método `imprimir`.

* Buscamos un valor en el árbol utilizando el método `buscar`.

* Eliminamos un valor del árbol utilizando el método `eliminar`.

* Imprimimos el árbol de nuevo para ver los cambios realizados.

Este código es bastante complejo y contiene varios conceptos avanzados, como estructuras, métodos y recursividad. Sin embargo, es un buen ejemplo de cómo se puede utilizar Rust para crear estructuras de datos complejas y manipularlas de manera eficiente.