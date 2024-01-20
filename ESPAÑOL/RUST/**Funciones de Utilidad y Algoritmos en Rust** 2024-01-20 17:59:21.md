```rust
// Una función recursiva para encontrar el máximo común divisor de dos números.
fn mcd(mut a: u32, mut b: u32) -> u32 {
    // Si b es 0, entonces a es el máximo común divisor.
    if b == 0 {
        a
    } else {
        // De lo contrario, llama recursivamente a la función con b como el nuevo a
        // y el resto de a dividido por b como el nuevo b.
        mcd(b, a % b)
    }
}

// Una función para generar una tabla de multiplicar para un número dado.
fn generar_tabla_de_multiplicar(n: u32) -> Vec<Vec<u32>> {
    // Crea una tabla vacía.
    let mut tabla = Vec::new();

    // Itera sobre los números del 1 al n.
    for i in 1..=n {
        // Crea una fila vacía.
        let mut fila = Vec::new();

        // Itera sobre los números del 1 al n.
        for j in 1..=n {
            // Añade el producto de i y j a la fila.
            fila.push(i * j);
        }

        // Añade la fila a la tabla.
        tabla.push(fila);
    }

    // Devuelve la tabla.
    tabla
}

// Una función para generar todas las permutaciones de una lista.
fn permutaciones<T>(lista: &Vec<T>) -> Vec<Vec<T>>
where
    T: Clone,
{
    // Si la lista está vacía, devuelve una lista vacía.
    if lista.is_empty() {
        return vec![];
    }

    // De lo contrario, crea una lista vacía para almacenar las permutaciones.
    let mut permutaciones = Vec::new();

    // Itera sobre los elementos de la lista.
    for i in 0..lista.len() {
        // Crea una sublista de la lista sin el elemento actual.
        let sublista = lista.iter().filter(|&x| x != &lista[i]).cloned().collect();

        // Genera las permutaciones de la sublista.
        let subpermutaciones = permutaciones(&sublista);

        // Para cada permutación de la sublista, añade el elemento actual al principio de la permutación.
        for permutacion in subpermutaciones {
            let mut permutacion_con_elemento = permutacion.clone();
            permutacion_con_elemento.insert(0, lista[i].clone());
            permutaciones.push(permutacion_con_elemento);
        }
    }

    // Devuelve las permutaciones.
    permutaciones
}

// Una función para ordenar una lista de números.
fn ordenar_lista(lista: &mut Vec<i32>) {
    // Ordena la lista usando el algoritmo de ordenación rápida.
    lista.sort();
}

// Una función para buscar un elemento en una lista ordenada.
fn buscar_elemento_en_lista_ordenada(lista: &[i32], elemento: i32) -> Option<usize> {
    // Utiliza la búsqueda binaria para encontrar el elemento en la lista.
    lista.binary_search(&elemento)
}

// Una función para imprimir una lista de números.
fn imprimir_lista(lista: &Vec<i32>) {
    // Imprime cada elemento de la lista en una línea.
    for elemento in lista {
        println!("{}", elemento);
    }
}

// Una función para imprimir una tabla de multiplicar.
fn imprimir_tabla_de_multiplicar(tabla: &Vec<Vec<u32>>) {
    // Imprime cada fila de la tabla en una línea.
    for fila in tabla {
        for elemento in fila {
            print!("{:>3}", elemento);
        }
        println!();
    }
}

// Una función para imprimir todas las permutaciones de una lista.
fn imprimir_permutaciones<T>(permutaciones: &Vec<Vec<T>>)
where
    T: Clone,
{
    // Imprime cada permutación en una línea.
    for permutacion in permutaciones {
        for elemento in permutacion {
            print!("{:>3}", elemento);
        }
        println!();
    }
}

// Una función para pedir un número al usuario.
fn pedir_numero() -> u32 {
    // Crea un nuevo objeto Scanner.
    let scanner = Scanner::new();

    // Pide al usuario que introduzca un número.
    println!("Introduzca un número:");

    //