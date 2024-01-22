```
Encendido:
    ; Reiniciar el microcontrolador
    JMP Reiniciar

Reiniciar:
    ; Configurar el oscilador a 8 MHz
    OSCCAL = 0x80

    ; Desactivar el watchdog timer
    WDTCR = 0x00

    ; Configurar los pines de E/S
    DDRB = 0xFF ; Todos como salidas
    PORTB = 0x00 ; Todos en 0

    ; Habilitar las interrupciones
    SEI

Bucle principal:
    ; Encender el LED
    PORTB.0 = 1

    ; Esperar 1 segundo
    Delay_1s

    ; Apagar el LED
    PORTB.0 = 0

    ; Esperar 1 segundo
    Delay_1s

    ; Ir al bucle principal
    JMP Bucle principal

Delay_1s:
    ; Configurar el contador de tiempo 1 a 65536
    TCNT1 = 0x0000

    ; Configurar el modo de conteo a CTC
    TCCR1A = 0x00

    ; Configurar el prescaler a 1024
    TCCR1B = 0x05

    ; Esperar a que el contador alcance 65536
    while (TCNT1 < 65536);

    ; Volver a la instrucción que llamó a esta función
    RET

ISR:
    ; Leer el registro de estado de la interrupción
    SREG = SREG_IO

    ; Leer el vector de interrupción
    IVEC = IVEC_IO

    ; Saltar al manejador de interrupción adecuado
    CALL IVEC_TABLE[IVEC]

IVEC_TABLE:
    ; Direcciones de los manejadores de interrupción
    .long Manejador_interrupcion_1
    .long Manejador_interrupcion_2
    .long Manejador_interrupcion_3

Manejador_interrupcion_1:
    ; Código para manejar la interrupción 1
    ; ...

    ; Volver al bucle principal
    JMP Bucle principal

Manejador_interrupcion_2:
    ; Código para manejar la interrupción 2
    ; ...

    ; Volver al bucle principal
    JMP Bucle principal

Manejador_interrupcion_3:
    ; Código para manejar la interrupción 3
    ; ...

    ; Volver al bucle principal
    JMP Bucle principal
```

Explicación del código:

1. Encendido:
    * Reinicia el microcontrolador y configura el oscilador a 8 MHz.
    * Desactiva el watchdog timer y configura los pines de E/S.
    * Habilita las interrupciones.

2. Bucle principal:
    * Enciende el LED durante 1 segundo, luego lo apaga durante 1 segundo.
    * Repite este proceso indefinidamente.

3. Delay_1s:
    * Configura el contador de tiempo 1 para que cuente hasta 65536.
    * Espera a que el contador alcance 65536.
    * Devuelve el control a la instrucción que llamó a esta función.

4. ISR:
    * Lee el registro de estado de la interrupción y el vector de interrupción.
    * Salta al manejador de interrupción adecuado.

5. IVEC_TABLE:
    * Contiene las direcciones de los manejadores de interrupción.

6. Manejadores de interrupción:
    * Cada manejador de interrupción contiene el código para manejar una interrupción específica.
    * Los manejadores de interrupción simplemente vuelven al bucle principal después de procesar la interrupción.