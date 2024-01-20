```rust
// Crea una función que reciba un vector de números enteros y devuelva el máximo común divisor (MCD) de todos los números del vector.

fn mcd(numeros: &Vec<i32>) -> i32 {
    // Si el vector está vacío, devuelve 0.
    if numeros.is_empty() {
        return 0;
    }

    // Inicializa el MCD con el primer elemento del vector.
    let mut mcd = numeros[0];

    // Itera sobre los elementos restantes del vector.
    for numero in &numeros[1..] {
        // Calcula el MCD del MCD actual y el número actual.
        mcd = mcd_dos_numeros(mcd, *numero);
    }

    // Devuelve el MCD final.
    mcd
}

// Crea una función que reciba dos números enteros y devuelva el máximo común divisor (MCD) de los dos números.

fn mcd_dos_numeros(a: i32, b: i32) -> i32 {
    // Si b es 0, devuelve a.
    if b == 0 {
        return a;
    }

    // Calcula el resto de la división de a entre b.
    let resto = a % b;

    // Llama a la función recursivamente con b y el resto.
    mcd_dos_numeros(b, resto)
}

// Crea un vector de números enteros.

let numeros = vec![12, 18, 24, 30];

// Calcula el MCD de los números del vector.

let mcd = mcd(&numeros);

// Imprime el MCD.

println!("El MCD de los números {} es {}", numeros.iter().map(|n| n.to_string()).collect::<String>(), mcd);
```

Explicación del código:

* La función `mcd` recibe un vector de números enteros y devuelve el máximo común divisor (MCD) de todos los números del vector.

* La función `mcd` primero comprueba si el vector está vacío. Si está vacío, devuelve 0.

* Si el vector no está vacío, la función `mcd` inicializa el MCD con el primer elemento del vector.

* A continuación, la función `mcd` itera sobre los elementos restantes del vector. Para cada elemento, calcula el MCD del MCD actual y el elemento actual.

* La función `mcd` utiliza la función `mcd_dos_numeros` para calcular el MCD de dos números.

* La función `mcd_dos_numeros` recibe dos números enteros y devuelve el máximo común divisor (MCD) de los dos números.

* La función `mcd_dos_numeros` primero comprueba si el segundo número es 0. Si es 0, devuelve el primer número.

* Si el segundo número no es 0, la función `mcd_dos_numeros` calcula el resto de la división del primer número entre el segundo número.

* A continuación, la función `mcd_dos_numeros` llama a sí misma recursivamente con el segundo número y el resto.

* La función `mcd_dos_numeros` devuelve el MCD final.

* La función `mcd` devuelve el MCD de todos los números del vector.

* El código crea un vector de números enteros y llama a la función `mcd` para calcular el MCD de los números del vector.

* El código imprime el MCD.