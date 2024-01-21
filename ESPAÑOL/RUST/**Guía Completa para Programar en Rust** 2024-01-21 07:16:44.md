```
// Definimos el módulo principal.
mod principal {

    // Esta función es el punto de entrada del programa.
    fn main() {

        // Definimos y llamamos a una función para imprimir un mensaje de bienvenida.
        bienvenida();

        // Definimos un vector con los números del 1 al 10.
        let numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

        // Recorremos el vector e imprimimos cada elemento.
        for numero in &numeros {
            println!("{}", numero);
        }

        // Definimos una matriz de 3x3 con los números del 1 al 9.
        let matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

        // Recorremos la matriz e imprimimos cada elemento.
        for fila in &matriz {
            for columna in fila {
                println!("{}", columna);
            }
        }

        // Definimos un diccionario con los números del 1 al 10 como claves y su cuadrado como valores.
        let diccionario = [(1, 1), (2, 4), (3, 9), (4, 16), (5, 25), (6, 36), (7, 49), (8, 64), (9, 81), (10, 100)];

        // Recorremos el diccionario e imprimimos cada elemento.
        for (clave, valor) in &diccionario {
            println!("{}: {}", clave, valor);
        }

        // Definimos una función para calcular el factorial de un número.
        fn factorial(numero: u64) -> u64 {
            if numero == 1 {
                return 1;
            } else {
                return numero * factorial(numero - 1);
            }
        }

        // Llamamos a la función para calcular el factorial de 10.
        let factorial_10 = factorial(10);

        // Imprimimos el factorial de 10.
        println!("El factorial de 10 es {}", factorial_10);

        // Definimos un struct para representar un punto en el espacio.
        struct Punto {
            x: f64,
            y: f64,
            z: f64,
        }

        // Creamos un objeto de tipo Punto.
        let punto = Punto { x: 1.0, y: 2.0, z: 3.0 };

        // Imprimimos las coordenadas del punto.
        println!("El punto es ({}, {}, {})", punto.x, punto.y, punto.z);

        // Creamos una lista enlazada con los números del 1 al 10.
        let mut lista = Lista::new();
        for numero in 1..11 {
            lista.push(numero);
        }

        // Recorremos la lista enlazada e imprimimos cada elemento.
        println!("La lista enlazada es:");
        for nodo in lista.iter() {
            println!("{}", nodo);
        }

        // Definimos una función para ordenar una lista enlazada.
        fn ordenar_lista(lista: &mut Lista) {
            // Recorremos la lista enlazada con dos punteros.
            for nodo in lista.iter_mut() {
                for otro_nodo in nodo.next.iter_mut() {
                    // Si el elemento del primer puntero es mayor que el elemento del segundo puntero, los intercambiamos.
                    if nodo.data > otro_nodo.data {
                        let temp = nodo.data;
                        nodo.data = otro_nodo.data;
                        otro_nodo.data = temp;
                    }
                }
            }
        }

        // Ordenamos la lista enlazada.
        ordenar_lista(&mut lista);

        // Imprimimos la lista enlazada ordenada.
        println!("La lista enlazada ordenada es:");
        for nodo in lista.iter() {
            println!("{}", nodo);
        }

        // Creamos un árbol binario de búsqueda con los números del 1 al 10.
        let mut arbol = ArbolBinario::new();
        for numero in 1..11 {
            arbol.insertar(numero);
        }

        // Buscamos el número 5 en el árbol binario de búsqueda.
        if arbol.buscar(5) {
            println!("El número 5 está en el árbol binario de búsqueda");
        } else {
            println!("El número 5 no está en el árbol binario de búsqueda");
        }

        // Eliminamos el número 5 del árbol binario de búsqueda.
        arbol.eliminar(5);

        // Buscamos el número 5 en el árbol binario de búsqueda.
        if arbol.buscar(5) {
            println!("El número 5 está en el árbol binario de búsqueda");
        } else {
            println!("El número 5 no está en el árbol binario de búsqueda");
        }
    }

    // Esta función imprime un mensaje de bienvenida.
    fn bienvenida() {
        println!("¡Bienvenido al programa en Rust!");
    }

    // Esta struct representa un nodo de una lista enlazada.
    struct Nodo {
        data: u64,
        next: Option<Box<Nodo>>,
    }

    // Esta struct representa una lista enlazada.
    struct Lista {
        head: Option<Box<Nodo>>,
    }

    // Este método crea una lista enlazada vacía.
    impl Lista {
        fn new() -> Lista {
            Lista { head: None }
        }

        // Este método añade un elemento al final de la lista enlazada.
        fn push(&mut self, data: u64) {
            let mut nodo = Box::new(Nodo { data, next: None });
            if let Some(ref mut head) = self.head {
                let mut curr = head;
                while let Some(ref mut next) = curr.next {
                    curr = next;
                }
                curr.next = Some(nodo);
            } else {
                self.head = Some(nodo);
            }
        }

        // Este método itera sobre los elementos de la lista enlazada.
        fn iter(&self) -> Iter