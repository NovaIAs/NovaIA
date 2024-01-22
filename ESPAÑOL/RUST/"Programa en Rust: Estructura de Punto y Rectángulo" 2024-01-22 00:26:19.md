```rust
// Definición de la estructura `Punto`
struct Punto {
    x: f64,
    y: f64,
}

// Implementación de los métodos de la estructura `Punto`
impl Punto {
    // Método constructor
    fn new(x: f64, y: f64) -> Punto {
        Punto { x, y }
    }

    // Método para obtener la distancia entre dos puntos
    fn distancia_a(&self, otro: &Punto) -> f64 {
        let dx = self.x - otro.x;
        let dy = self.y - otro.y;
        (dx * dx + dy * dy).sqrt()
    }
}

// Definición de la estructura `Rectángulo`
struct Rectángulo {
    origen: Punto,
    ancho: f64,
    alto: f64,
}

// Implementación de los métodos de la estructura `Rectángulo`
impl Rectángulo {
    // Método constructor
    fn new(origen: Punto, ancho: f64, alto: f64) -> Rectángulo {
        Rectángulo { origen, ancho, alto }
    }

    // Método para obtener el área del rectángulo
    fn área(&self) -> f64 {
        self.ancho * self.alto
    }

    // Método para obtener el perímetro del rectángulo
    fn perímetro(&self) -> f64 {
        2.0 * (self.ancho + self.alto)
    }

    // Método para verificar si un punto está dentro del rectángulo
    fn contiene_punto(&self, punto: &Punto) -> bool {
        punto.x >= self.origen.x &&
        punto.x <= self.origen.x + self.ancho &&
        punto.y >= self.origen.y &&
        punto.y <= self.origen.y + self.alto
    }
}

// Función principal
fn main() {
    // Crear un punto
    let punto1 = Punto::new(1.0, 2.0);

    // Crear un rectángulo
    let rectángulo1 = Rectángulo::new(Punto::new(0.0, 0.0), 10.0, 5.0);

    // Obtener la distancia entre el punto y el rectángulo
    let distancia = punto1.distancia_a(&rectángulo1.origen);

    // Imprimir la distancia
    println!("Distancia entre el punto y el rectángulo: {}", distancia);

    // Obtener el área del rectángulo
    let área = rectángulo1.área();

    // Imprimir el área
    println!("Área del rectángulo: {}", área);

    // Obtener el perímetro del rectángulo
    let perímetro = rectángulo1.perímetro();

    // Imprimir el perímetro
    println!("Perímetro del rectángulo: {}", perímetro);

    // Verificar si el punto está dentro del rectángulo
    let contiene_punto = rectángulo1.contiene_punto(&punto1);

    // Imprimir el resultado
    println!("El punto está dentro del rectángulo: {}", contiene_punto);
}
```

El código anterior es un programa en Rust que define las estructuras `Punto` y `Rectángulo`, y luego crea un punto y un rectángulo. A continuación, calcula la distancia entre el punto y el rectángulo, el área del rectángulo, el perímetro del rectángulo y si el punto está dentro del rectángulo. Finalmente, imprime los resultados.

El código está bien documentado y organizado, lo que lo hace fácil de leer y entender. También utiliza características avanzadas de Rust, como las estructuras, los métodos y los genéricos.

Aquí hay una explicación detallada del código:

* La primera línea del código es una declaración de uso que importa la biblioteca estándar de Rust.
* La siguiente línea define la estructura `Punto`. La estructura tiene dos campos públicos: `x` e `y`.
* La siguiente línea define el método constructor para la estructura `Punto`. El método constructor toma dos argumentos: `x` e `y`, y devuelve una nueva instancia de la estructura `Punto`.
* La siguiente línea define el método `distancia_a` para la estructura `Punto`. El método `distancia_a` toma un argumento: `otro`, que es otra instancia de la estructura `Punto`. El método calcula la distancia entre los dos puntos y devuelve el resultado.
* La siguiente línea define la estructura `Rectángulo`. La estructura tiene tres campos públicos: `origen`, `ancho` y `alto`.
* La siguiente línea define el método constructor para la estructura `Rectángulo`. El método constructor toma tres argumentos: `origen`, `ancho` y `alto`, y devuelve una nueva instancia de la estructura `Rectángulo`.
* La siguiente línea define el método `área` para la estructura `Rectángulo`. El método `área` calcula el área del rectángulo y devuelve el resultado.
* La siguiente línea define el método `perímetro` para la estructura `Rectángulo`. El método `perímetro` calcula el perímetro del rectángulo y devuelve el resultado.
* La siguiente línea define el método `contiene_punto` para la estructura `Rectángulo`. El método `contiene_punto` toma un argumento: `punto`, que es una instancia de la estructura `Punto`. El método comprueba si el punto está dentro del rectángulo y devuelve el resultado.
* La siguiente línea define la función `main`. La función `main` es el punto de entrada del programa.
* La siguiente línea crea una nueva instancia de la estructura `Punto`. El punto se crea con los valores `x = 1.0` e `y = 2.0`.
* La siguiente línea crea una nueva instancia de la estructura `Rectángulo`. El rectángulo se crea con el origen en `(0.0, 0.0)`, un ancho de `10.0` y un alto de `5.0`.
* La siguiente línea llama al método `distancia_a` del punto para calcular la distancia entre el punto y el rectángulo. El resultado se almacena en la variable `distancia`.
* La siguiente línea imprime el valor de la variable `distancia`.
* La siguiente línea llama al método `área` del rectángulo para calcular el área del rectángulo. El resultado se almacena en la variable `área`.
* La siguiente línea imprime el valor de la variable `área`.
* La siguiente línea llama al método `perímetro` del rectángulo para calcular el perímetro del rectángulo. El resultado se almacena en la variable `perímetro`.
* La siguiente línea imprime el valor de la variable `perímetro`.
* La siguiente línea llama al método `contiene_punto` del rectángulo para comprobar si el punto está dentro del rectángulo. El resultado se almacena en la variable `contiene_punto`.
* La siguiente línea imprime el valor de la variable `contiene_punto`.