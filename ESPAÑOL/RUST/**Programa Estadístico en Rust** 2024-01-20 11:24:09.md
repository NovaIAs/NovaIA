```rust
// Estructura principal del programa
struct Programa {
    opciones: Opciones,
    datos: Datos,
    resultados: Resultados,
}

// Estructura de las opciones
struct Opciones {
    entrada: String,
    salida: String,
    verbose: bool,
}

// Estructura de los datos
struct Datos {
    numeros: Vec<f64>,
    nombres: Vec<String>,
}

// Estructura de los resultados
struct Resultados {
    promedio: f64,
    desviacion_estandar: f64,
    mediana: f64,
}

// Función principal del programa
fn main() {
    // Crear una instancia del programa
    let programa = Programa::new();

    // Leer los datos del archivo de entrada
    let datos = programa.leer_datos();

    // Procesar los datos
    let resultados = programa.procesar_datos(datos);

    // Escribir los resultados en el archivo de salida
    programa.escribir_resultados(resultados);
}

// Implementar la estructura Programa
impl Programa {
    // Crear una nueva instancia del programa
    fn new() -> Programa {
        let opciones = Opciones::new();
        let datos = Datos::new();
        let resultados = Resultados::new();

        Programa {
            opciones,
            datos,
            resultados,
        }
    }

    // Leer los datos del archivo de entrada
    fn leer_datos(&self) -> Datos {
        let mut datos = Datos::new();

        let archivo_entrada = File::open(self.opciones.entrada).unwrap();
        let mut lector = BufReader::new(archivo_entrada);

        let mut linea = String::new();
        while lector.read_line(&mut linea).unwrap() > 0 {
            let campos: Vec<&str> = linea.split(',').collect();
            datos.numeros.push(campos[0].parse::<f64>().unwrap());
            datos.nombres.push(campos[1].to_string());
        }

        datos
    }

    // Procesar los datos
    fn procesar_datos(&self, datos: Datos) -> Resultados {
        let mut resultados = Resultados::new();

        // Calcular el promedio
        resultados.promedio = datos.numeros.iter().sum::<f64>() / datos.numeros.len() as f64;

        // Calcular la desviación estándar
        let desviacion_estandar = datos.numeros.iter()
            .map(|x| (x - resultados.promedio).powf(2.0))
            .sum::<f64>() / datos.numeros.len() as f64;
        resultados.desviacion_estandar = desviacion_estandar.sqrt();

        // Calcular la mediana
        resultados.mediana = datos.numeros.sort().get(datos.numeros.len() / 2).unwrap();

        resultados
    }

    // Escribir los resultados en el archivo de salida
    fn escribir_resultados(&self, resultados: Resultados) {
        let archivo_salida = File::create(self.opciones.salida).unwrap();
        let mut escritor = BufWriter::new(archivo_salida);

        writeln!(escritor, "Promedio: {}", resultados.promedio);
        writeln!(escritor, "Desviación estándar: {}", resultados.desviación_estandar);
        writeln!(escritor, "Mediana: {}", resultados.mediana);
    }
}

// Implementar la estructura Opciones
impl Opciones {
    // Crear nuevas opciones
    fn new() -> Opciones {
        Opciones {
            entrada: "datos.csv".to_string(),
            salida: "resultados.txt".to_string(),
            verbose: false,
        }
    }
}

// Implementar la estructura Datos
impl Datos {
    // Crear nuevos datos
    fn new() -> Datos {
        Datos {
            numeros: Vec::new(),
            nombres: Vec::new(),
        }
    }
}

// Implementar la estructura Resultados
impl Resultados {
    // Crear nuevos resultados
    fn new() -> Resultados {
        Resultados {
            promedio: 0.0,
            desviacion_estandar: 0.0,
            mediana: 0.0,
        }
    }
}
```

Este código es un programa en Rust que lee datos de un archivo CSV, procesa los datos para calcular el promedio, la desviación estándar y la mediana, y escribe los resultados en un archivo de texto.

El programa se organiza en varias estructuras:

* `Programa`: Esta estructura representa el programa principal y contiene las opciones, los datos y los resultados.
* `Opciones`: Esta estructura representa las opciones del programa, como el archivo de entrada y el archivo de salida.
* `Datos`: Esta estructura representa los datos que se procesan, como los números y los nombres.
* `Resultados`: Esta estructura representa los resultados del procesamiento de los datos, como el promedio, la desviación estándar y la mediana.

El programa primero crea una instancia de la estructura `Programa`, luego lee los datos del archivo de entrada, procesa los datos para calcular los resultados y finalmente escribe los resultados en el archivo de salida.

El código utiliza varias técnicas avanzadas de Rust, como estructuras, tipos genéricos, métodos y cierres. También utiliza la biblioteca estándar de Rust para leer y escribir archivos, y para procesar los datos.

Este código es un ejemplo de cómo escribir un programa complejo en Rust que puede realizar cálculos estadísticos y presentar los resultados en un formato legible.