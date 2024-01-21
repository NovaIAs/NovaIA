```
// Módulo principal
mod area {
    // Estructura para almacenar las dimensiones de una figura geométrica
    struct Dimensiones {
        ancho: f64,
        alto: f64,
    }

    // Función para calcular el área de un rectángulo
    fn calcular_area_rectangulo(dimensiones: Dimensiones) -> f64 {
        dimensiones.ancho * dimensiones.alto
    }

    // Función para calcular el área de un triángulo
    fn calcular_area_triangulo(dimensiones: Dimensiones) -> f64 {
        0.5 * dimensiones.ancho * dimensiones.alto
    }

    // Función para calcular el área de un círculo
    fn calcular_area_circulo(radio: f64) -> f64 {
        std::f64::consts::PI * radio.powi(2)
    }
}

// Módulo de pruebas
#[cfg(test)]
mod pruebas {
    use super::area::*;

    // Prueba para el rectángulo
    #[test]
    fn prueba_rectangulo() {
        let dimensiones = Dimensiones { ancho: 5.0, alto: 10.0 };
        let area = calcular_area_rectangulo(dimensiones);
        assert_eq!(area, 50.0);
    }

    // Prueba para el triángulo
    #[test]
    fn prueba_triangulo() {
        let dimensiones = Dimensiones { ancho: 5.0, alto: 10.0 };
        let area = calcular_area_triangulo(dimensiones);
        assert_eq!(area, 25.0);
    }

    // Prueba para el círculo
    #[test]
    fn prueba_circulo() {
        let radio = 5.0;
        let area = calcular_area_circulo(radio);
        let diferencia_tolerable = 0.0001;
        assert!((area - std::f64::consts::PI * radio.powi(2)).abs() < diferencia_tolerable);
    }
}
```

Explicación del código:

* Módulo `area`: Este módulo contiene las funciones para calcular el área de varias figuras geométricas.


* Estructura `Dimensiones`: Esta estructura se utiliza para almacenar las dimensiones de una figura geométrica.


* Función `calcular_area_rectangulo`: Esta función calcula el área de un rectángulo.


* Función `calcular_area_triangulo`: Esta función calcula el área de un triángulo.


* Función `calcular_area_circulo`: Esta función calcula el área de un círculo.


* Módulo `pruebas`: Este módulo contiene las pruebas para las funciones de cálculo de áreas.


* Prueba `prueba_rectangulo`: Esta prueba comprueba que la función `calcular_area_rectangulo` calcula correctamente el área de un rectángulo.


* Prueba `prueba_triangulo`: Esta prueba comprueba que la función `calcular_area_triangulo` calcula correctamente el área de un triángulo.


* Prueba `prueba_circulo`: Esta prueba comprueba que la función `calcular_area_circulo` calcula correctamente el área de un círculo.