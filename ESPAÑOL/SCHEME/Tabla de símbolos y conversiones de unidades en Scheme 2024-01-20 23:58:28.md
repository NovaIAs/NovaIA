```scheme
;; Tabla de símbolos

(define (tabla-simbolos)
  '(
    (RESISTENCIA    +      OHM)
    (CAPACIDAD      *      FARADIO)
    (INDUCTANCIA    /      HENRY)
    (VOLTAJE        +      VOLT)
    (CORRIENTE      /      AMPER)
    (POTENCIA       *      WATT)
    (ENERGIA        *      JULIO)
    (CARGA          *      COULOMB)
    (FLUJO-MAGNETICO /      WEBER)
    (DENSIDAD-DE-FLUJO-MAGNETICO *      TESLA)
    (CAMPO-MAGNETICO /      AMPERE-POR-METRO)
    (PERMEABILIDAD  /      HENRY-POR-METRO)
    (SUSCEPTIBILIDAD-MAGNETICA  *      HENRYS-POR-METRO)
    (CAMPO-ELECTRICO /      VOLT-POR-METRO)
    (PERMITIVIDAD   /      FARADIO-POR-METRO)
    (SUSCEPTIBILIDAD-ELECTRICA  *      FARADIOS-POR-METRO)
    (VELOCIDAD-DE-LA-LUZ 299792458 (METRO-POR-SEGUNDO))
    (CONSTANTE-DE-BOLTZMANN 1.38064852 (JULIO-POR-KELVIN))
  ))

;; Conversiones de unidades

(define (convertir unidades valor factor)
  (* valor factor))

(define (convertir-ohmios-a-k-ohmios valor)
  (convertir unidades valor 0.001))

(define (convertir-k-ohmios-a-ohmios valor)
  (convertir unidades valor 1000))

(define (convertir-faradios-a-micro-faradios valor)
  (convertir unidades valor 1000000))

(define (convertir-micro-faradios-a-faradios valor)
  (convertir unidades valor 0.000001))

(define (convertir-henrys-a-micro-henrys valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-henrys-a-henrys valor)
  (convertir unidades valor 1000000))

(define (convertir-voltios-a-micro-voltios valor)
  (convertir unidades valor 1000000))

(define (convertir-micro-voltios-a-voltios valor)
  (convertir unidades valor 0.000001))

(define (convertir-amperios-a-micro-amperios valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-amperios-a-amperios valor)
  (convertir unidades valor 1000000))

(define (convertir-vatios-a-mili-vatios valor)
  (convertir unidades valor 0.001))

(define (convertir-mili-vatios-a-vatios valor)
  (convertir unidades valor 1000))

(define (convertir-julios-a-micro-julios valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-julios-a-julios valor)
  (convertir unidades valor 1000000))

(define (convertir-coulombs-a-micro-coulombs valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-coulombs-a-coulombs valor)
  (convertir unidades valor 1000000))

(define (convertir-webers-a-micro-webers valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-webers-a-webers valor)
  (convertir unidades valor 1000000))

(define (convertir-teslas-a-nano-teslas valor)
  (convertir unidades valor 0.000000001))

(define (convertir-nano-teslas-a-teslas valor)
  (convertir unidades valor 1000000000))

(define (convertir-amperios-por-metro-a-micro-amperios-por-metro valor)
  (convertir unidades valor 0.000001))

(define (convertir-micro-amperios-por-metro-a-amperios-por-metro valor)
  (convertir unidades valor 1000000))

(define (convertir-henrys-por-metro-a-nano-henrys-por-metro valor)
  (convertir unidades valor 0.000000001))

(define (convertir-nano-henrys-por-metro-a-henrys-por-metro valor)
  (convertir unidades valor 10