```smalltalk
"Clase 'Tiempo' que representa un momento en el tiempo."
Tiempo subclass: HoraVariada [
  instanceVariableNames: 'horas minutos segundos'

  initialize: horas minutos segundos [
    horas := horas.
    minutos := minutos.
    segundos := segundos.
  ]

  imprimir [
    ^String streamContents: [ |str|
        str nextPutAll: horas asString.
        str next: $: space.
        str nextPutAll: minutos asString.
        str next: $: space.
        str nextPutAll: segundos asString.
      ]
  ]

  sumarHora: otraHora [
    segundos := segundos + otraHora segundos.
    minutos := minutos + otraHora minutos.
    horas := horas + otraHora horas + (segundos // 60).
    segundos := segundos mod 60.
    minutos := minutos mod 60.
  ]

  sumarMinutos: minutos [
    minutos := minutos + minutos.
    horas := horas + (minutos // 60).
    minutos := minutos mod 60.
  ]

  sumarSegundos: segundos [
    segundos := segundos + segundos.
    minutos := minutos + (segundos // 60).
    segundos := segundos mod 60.
  ]
]

"Clase principal de nuestra aplicación."
TiempoDeLlegada [
  instanceVariableNames: 'horaLlegada horaSalida tiempoTotal'

  initialize [
    horaLlegada := HoraVariada new.
    horaSalida := HoraVariada new.
    tiempoTotal := HoraVariada new.
  ]

  establecerHoraLlegada: hora [
    horaLlegada := hora.
  ]

  establecerHoraSalida: hora [
    horaSalida := hora.
  ]

  calcularTiempoTotal [
    tiempoTotal := horaSalida - horaLlegada.
  ]

  imprimirTiempoTotal [
    ^String streamContents: [ |str|
        str nextPutAll: 'Tiempo total: '.
        str nextLine.
        str nextPutAll: tiempoTotal imprimir.
      ]
  ]
]

tiempoDeLlegada := TiempoDeLlegada new.
tiempoDeLlegada establecerHoraLlegada: (HoraVariada new initialize: 10 minutos: 30 segundos: 0).
tiempoDeLlegada establecerHoraSalida: (HoraVariada new initialize: 12 minutos: 45 segundos: 0).
tiempoDeLlegada calcularTiempoTotal.
Transcript show: tiempoDeLlegada imprimirTiempoTotal.
```

Explicación del código:

* La clase 'Tiempo' representa un momento en el tiempo. Tiene tres variables de instancia: 'horas', 'minutos' y 'segundos'. También define métodos para inicializar un objeto, imprimirlo, sumarle otra hora, minutos o segundos.
* La clase 'HoraVariada' es una subclase de 'Tiempo'. Representa un momento en el tiempo que puede variar. Define métodos para inicializar un objeto, imprimirlo, sumarle otra hora, minutos o segundos.
* La clase 'TiempoDeLlegada' es la clase principal de nuestra aplicación. Tiene tres variables de instancia: 'horaLlegada', 'horaSalida' y 'tiempoTotal'. También define métodos para inicializar un objeto, establecer la hora de llegada y salida, calcular el tiempo total y imprimir el tiempo total.
* El código crea un objeto 'tiempoDeLlegada' y establece la hora de llegada y salida. Luego, calcula el tiempo total y lo imprime en la consola.