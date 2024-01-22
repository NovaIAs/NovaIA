```rust
// Definimos una estructura para representar un nodo de un árbol binario.
#[derive(Debug)]
struct Nodo {
    valor: i32,
    izquierda: Option<Box<Nodo>>,
    derecha: Option<Box<Nodo>>,
}

// Implementamos el rasgo Display para poder imprimir los nodos del árbol.
impl std::fmt::Display for Nodo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.valor)
    }
}

// Definimos una función para crear un nuevo nodo de un árbol binario.
fn nuevo_nodo(valor: i32) -> Nodo {
    Nodo {
        valor,
        izquierda: None,
        derecha: None,
    }
}

// Definimos una función para insertar un nuevo valor en un árbol binario.
fn insertar(nodo: &mut Nodo, valor: i32) {
    if valor < nodo.valor {
        if nodo.izquierda.is_none() {
            nodo.izquierda = Some(Box::new(nuevo_nodo(valor)));
        } else {
            insertar(nodo.izquierda.as_mut().unwrap(), valor);
        }
    } else {
        if nodo.derecha.is_none() {
            nodo.derecha = Some(Box::new(nuevo_nodo(valor)));
        } else {
            insertar(nodo.derecha.as_mut().unwrap(), valor);
        }
    }
}

// Definimos una función para buscar un valor en un árbol binario.
fn buscar(nodo: &Nodo, valor: i32) -> bool {
    if nodo.valor == valor {
        true
    } else if valor < nodo.valor {
        if nodo.izquierda.is_none() {
            false
        } else {
            buscar(nodo.izquierda.as_ref().unwrap(), valor)
        }
    } else {
        if nodo.derecha.is_none() {
            false
        } else {
            buscar(nodo.derecha.as_ref().unwrap(), valor)
        }
    }
}

// Definimos una función para eliminar un valor de un árbol binario.
fn eliminar(nodo: &mut Nodo, valor: i32) {
    if valor < nodo.valor {
        if nodo.izquierda.is_some() {
            eliminar(nodo.izquierda.as_mut().unwrap(), valor);
        }
    } else if valor > nodo.valor {
        if nodo.derecha.is_some() {
            eliminar(nodo.derecha.as_mut().unwrap(), valor);
        }
    } else {
        if nodo.izquierda.is_none() && nodo.derecha.is_none() {
            *nodo = nuevo_nodo(0);
        } else if nodo.izquierda.is_none() {
            *nodo = *nodo.derecha.as_mut().unwrap();
        } else if nodo.derecha.is_none() {
            *nodo = *nodo.izquierda.as_mut().unwrap();
        } else {
            let mut sucesor = nodo.derecha.as_mut().unwrap();
            while sucesor.izquierda.is_some() {
                sucesor = sucesor.izquierda.as_mut().unwrap();
            }
            nodo.valor = sucesor.valor;
            eliminar(sucesor, sucesor.valor);
        }
    }
}

// Definimos una función para imprimir un árbol binario en forma de árbol.
fn imprimir_arbol(nodo: &Nodo, nivel: usize) {
    if nodo.derecha.is_some() {
        imprimir_arbol(nodo.derecha.as_ref().unwrap(), nivel + 1);
    }
    for _ in 0..nivel {
        print!("    ");
    }
    println!("{}", nodo);
    if nodo.izquierda.is_some() {
        imprimir_arbol(nodo.izquierda.as_ref().unwrap(), nivel + 1);
    }
}

// Creamos un nuevo árbol binario.
let mut arbol = nuevo_nodo(10);

// Insertamos algunos valores en el árbol.
insertar(&mut arbol, 5);
insertar(&mut arbol, 15);
insertar(&mut arbol, 2);
insertar(&mut arbol, 7);
insertar(&mut arbol, 12);
insertar(&mut arbol, 20);

// Imprimimos el árbol.
imprimir_arbol(&arbol, 0);

// Buscamos un valor en el árbol.
println!("¿El árbol contiene el valor 12? {}", buscar(&arbol, 12));

// Eliminamos un valor del árbol.
eliminar(&mut arbol, 15);

// Imprimimos el árbol nuevamente.
imprimir_arbol(&arbol, 0);
```

Este código crea un árbol binario de búsqueda y lo manipula.

El código primero define una estructura para representar un nodo de un árbol binario. La estructura tiene tres campos:

* `valor`: El valor del nodo.
* `izquierda`: Un puntero al nodo hijo izquierdo.
* `derecha`: Un puntero al nodo hijo derecho.

El código luego implementa el rasgo `Display` para poder imprimir los nodos del árbol.

A continuación, el código define una función para crear un nuevo nodo de un árbol binario. La función toma un valor como argumento y devuelve un nuevo nodo con ese valor.

El código luego define una función para insertar un nuevo valor en un árbol binario. La función toma un nodo y un valor como argumentos y inserta el nuevo valor en el árbol en el lugar correcto.

El código luego define una función para buscar un valor en un árbol binario. La función toma un nodo y un valor como argumentos y devuelve `true` si el valor se encuentra en el