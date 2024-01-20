```verilog
// Código Verilog en Español: Controlador de Luces de Tráfico

// Definición de módulos
module ControladorLucesTrafico(
    input clock,  // Señal de reloj
    input botonPeatonal,  // Botón peatonal
    output luzRoja,  // Luz roja
    output luzAmarilla,  // Luz amarilla
    output luzVerde  // Luz verde
);

// Definición de estados
localparam ROJO = 0;
localparam AMARILLO = 1;
localparam VERDE = 2;
localparam ANARANJADO = 3;
localparam ESPERA_PEATONAL = 4;

// Registro de estado
reg [2:0] estadoActual;

// Salida del controlador
assign luzRoja = (estadoActual == ROJO || estadoActual == ANARANJADO);
assign luzAmarilla = (estadoActual == AMARILLO || estadoActual == ANARANJADO);
assign luzVerde = (estadoActual == VERDE);

// Lógica del controlador
always @(posedge clock) begin
    // Actualización del estado
    case (estadoActual)
        ROJO:
            if (botonPeatonal) begin
                estadoActual <= ESPERA_PEATONAL;
            end else begin
                estadoActual <= AMARILLO;
            end
        AMARILLO:
            estadoActual <= VERDE;
        VERDE:
            estadoActual <= ROJO;
        ESPERA_PEATONAL:
            if (!botonPeatonal) begin
                estadoActual <= ANARANJADO;
            end
        ANARANJADO:
            estadoActual <= ROJO;
    endcase
end

endmodule

// Código de prueba
module ControladorLucesTrafico_Test;

// Instanciación del controlador
wire clock;
wire botonPeatonal;
wire luzRoja, luzAmarilla, luzVerde;
ControladorLucesTrafico controlador(
    clock,
    botonPeatonal,
    luzRoja,
    luzAmarilla,
    luzVerde
);

// Generación de señales de prueba
initial begin
    // Inicialización de señales
    clock = 0;
    botonPeatonal = 0;

    // Secuencia de señales de prueba
    #10 botonPeatonal = 1;
    #20 botonPeatonal = 0;
    #30 botonPeatonal = 1;
    #40 botonPeatonal = 0;
    #50 botonPeatonal = 1;
    #60 botonPeatonal = 0;
    #70 botonPeatonal = 1;
    #80 botonPeatonal = 0;
end

// Generación de reloj
always begin
    #5 clock = !clock;
end

// Observación de señales
initial begin
    $monitor("Tiempo: %d, Estado: %d, Luz Roja: %d, Luz Amarilla: %d, Luz Verde: %d",
        $time, controlador.estadoActual, controlador.luzRoja, controlador.luzAmarilla, controlador.luzVerde);
end

endmodule
```

Explicación del código:

* **Módulo ControladorLucesTrafico:** Este módulo define el controlador de luces de tráfico. Tiene como entradas la señal de reloj y el botón peatonal, y como salidas las luces roja, amarilla y verde.
* **Estados:** El controlador tiene cinco estados diferentes: ROJO, AMARILLO, VERDE, ANARANJADO y ESPERA_PEATONAL.
* **Registro de estado:** El registro de estado almacena el estado actual del controlador.
* **Salida del controlador:** Las salidas del controlador son las luces roja, amarilla y verde.
* **Lógica del controlador:** La lógica del controlador actualiza el estado actual en función de la entrada del botón peatonal y del estado actual.
* **Módulo ControladorLucesTrafico_Test:** Este módulo define el código de prueba para el controlador de luces de tráfico.
* **Instanciación del controlador:** Se instancia el controlador de luces de tráfico en el módulo de prueba.
* **Generación de señales de prueba:** El código de prueba genera señales de prueba para el botón peatonal.
* **Generación de reloj:** El código de prueba genera la señal de reloj.
* **Observación de señales:** El código de prueba observa las señales del controlador de luces de tráfico.