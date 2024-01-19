```cool
clase Edificio {
    propiedades:
        plantas: entero
        habitaciones: entero
        baños: entero
        metros_cuadrados: real

    métodos:
        constructor(plantas, habitaciones, baños, metros_cuadrados) {
            self.plantas = plantas
            self.habitaciones = habitaciones
            self.baños = baños
            self.metros_cuadrados = metros_cuadrados
        }

        obtener_plantas() {
            return self.plantas
        }

        obtener_habitaciones() {
            return self.habitaciones
        }

        obtener_baños() {
            return self.baños
        }

        obtener_metros_cuadrados() {
            return self.metros_cuadrados
        }
}

clase Casa extends Edificio {
    propiedades:
        jardín: boolean

    métodos:
        constructor(plantas, habitaciones, baños, metros_cuadrados, jardín) {
            super(plantas, habitaciones, baños, metros_cuadrados)
            self.jardín = jardín
        }

        tiene_jardín() {
            return self.jardín
        }
}

clase Departamento extends Edificio {
    propiedades:
        piso: entero

    métodos:
        constructor(plantas, habitaciones, baños, metros_cuadrados, piso) {
            super(plantas, habitaciones, baños, metros_cuadrados)
            self.piso = piso
        }

        obtener_piso() {
            return self.piso
        }
}

clase Main {
    métodos:
        main() {
            casa = Casa(2, 3, 2, 120.0, true)
            departamento = Departamento(1, 2, 1, 80.0, 5)

            print("Casa:")
            print("Plantas:", casa.obtener_plantas())
            print("Habitaciones:", casa.obtener_habitaciones())
            print("Baños:", casa.obtener_baños())
            print("Metros cuadrados:", casa.obtener_metros_cuadrados())
            print("Tiene jardín:", casa.tiene_jardín())

            print("Departamento:")
            print("Plantas:", departamento.obtener_plantas())
            print("Habitaciones:", departamento.obtener_habitaciones())
            print("Baños:", departamento.obtener_baños())
            print("Metros cuadrados:", departamento.obtener_metros_cuadrados())
            print("Piso:", departamento.obtener_piso())
        }
}

Main.main()
```

Este código crea dos clases, `Casa` y `Departamento`, que heredan de la clase `Edificio`. La clase `Edificio` define los atributos y métodos comunes a todos los edificios, como el número de plantas, habitaciones, baños y metros cuadrados. Las clases `Casa` y `Departamento` añaden atributos y métodos específicos para cada tipo de edificio, como el jardín en el caso de las casas y el piso en el caso de los departamentos.

La clase `Main` contiene el método `main`, que se ejecuta cuando se ejecuta el programa. En este método se crean dos objetos, uno de tipo `Casa` y otro de tipo `Departamento`, y se imprimen sus atributos y métodos.