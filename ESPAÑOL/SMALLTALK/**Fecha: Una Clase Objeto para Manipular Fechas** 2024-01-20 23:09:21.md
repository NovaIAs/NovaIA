```smalltalk

"Objeto Fecha"

ObjetoFecha subclass: Fecha [
    "Métodos Privados"
    
    díasDelMes: unAño unMes [
        | días |
        días := {
            1 => 31,
            2 => 28,
            3 => 31,
            4 => 30,
            5 => 31,
            6 => 30,
            7 => 31,
            8 => 31,
            9 => 30,
            10 => 31,
            11 => 30,
            12 => 31
        };
        días at: unMes
    ].
    
    esBisiesto: unAño [
        unAño rem: 4 = 0
    ].

    "Métodos Públicos"
    
    newDate: unDía unMes unAño [
        self new setDía: unDía setMes: unMes setAño: unAño
    ].
    
    setDía: unDía [
        día := unDía
    ].
    
    día [
        día
    ].
    
    setMes: unMes [
        mes := unMes
    ].
    
    mes [
        mes
    ].
    
    setAño: unAño [
        año := unAño
    ].
    
    año [
        año
    ].
    
    comoCadena [
        ^ día asString, '/', mes asString, '/', año asString
    ].
    
    esIgualA: otraFecha [
        día = otraFecha día and: [
            mes = otraFecha mes and: [
                año = otraFecha año
            ]
        ]
    ].
    
    esMayorA: otraFecha [
        año > otraFecha año or: [
            año = otraFecha año and: [
                mes > otraFecha mes or: [
                    mes = otraFecha mes and: [
                        día > otraFecha día
                    ]
                ]
            ]
        ]
    ].
    
    esMenorA: otraFecha [
        año < otraFecha año or: [
            año = otraFecha año and: [
                mes < otraFecha mes or: [
                    mes = otraFecha mes and: [
                        día < otraFecha día
                    ]
                ]
            ]
        ]
    ].
    
    díasTranscurridos [
        | días |
        días := 0.
        año.timesRepeat: [
            días := días + díasDelMes: año año
        ].
        mes.timesRepeat: [
            días := días + díasDelMes: año mes
        ].
        días := días + día
    ].

    díasHasta: otraFecha [
        otraFecha díasTranscurridos - díasTranscurridos
    ].
    
    díasDesde: otraFecha [
        díasHasta: otraFecha negado
    ].
    
    sumarDías: días [
        día := día + días
    ].
    
    restarDías: días [
        día := día - días
    ].
    
    añosTranscurridos [
        | años |
        años := 0.
        año.timesRepeat: [
            años := años + 1
        ].
        años
    ]

    añosHasta: otraFecha [
        otraFecha añosTranscurridos - añosTranscurridos
    ]

    añosDesde: otraFecha [
        añosHasta: otraFecha negado
    ]
].

```

"Ejemplo de uso"

Fecha newDate: 15 mayo 2023.
Fecha newDate: 16 junio 2023.

unaFecha comoCadena.
otraFecha comoCadena.

unaFecha esIgualA: otraFecha.
unaFecha esMayorA: otraFecha.
unaFecha esMenorA: otraFecha.

unaFecha díasTranscurridos.
unaFecha díasHasta: otraFecha.
unaFecha díasDesde: otraFecha.

unaFecha sumarDías: 10.
unaFecha restarDías: 5.

unaFecha añosTranscurridos.
unaFecha añosHasta: otraFecha.
unaFecha añosDesde: otraFecha.