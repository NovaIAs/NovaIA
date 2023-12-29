module contador_aleatorio (
    input wire clk,
    input wire rst,
    output reg [7:0] led
);

reg [15:0] contador;

always @(posedge clk or posedge rst) begin
    if (rst)
        contador <= 0;
    else if (contador == 16'd65535)
        contador <= 0;
    else
        contador <= contador + 1;
end

always @(posedge clk or posedge rst) begin
    if (rst)
        led <= 8'b00000000;
    else begin
        case (contador[3:0])
            4'b0000: led <= 8'b00000001;
            4'b0001: led <= 8'b00000011;
            4'b0010: led <= 8'b00000111;
            4'b0011: led <= 8'b00001111;
            4'b0100: led <= 8'b00011111;
            4'b0101: led <= 8'b00111111;
            4'b0110: led <= 8'b01111111;
            4'b0111: led <= 8'b11111111;
            default: led <= 8'b00000000;
        endcase
    end
end

endmodule

O código acima é um contador aleatório em Verilog, uma linguagem de descrição de hardware. O módulo "contador_aleatorio" possui uma entrada para o clock (clk) e para o reset (rst), e uma saída para o LED (led).

O contador interno é representado pela variável "contador", que é um registrador de 16 bits. No sempre @(posedge clk or posedge rst), o contador é incrementado de 1 a cada borda de subida do clock, exceto quando o sinal de reset (rst) está em estado alto. Quando o contador atinge o valor máximo de 65535 (16'd65535), ele é reiniciado para 0.

No segundo sempre @(posedge clk or posedge rst), o valor do contador é utilizado para determinar o padrão de iluminação do LED. Dependendo dos 4 bits menos significativos do contador (contador[3:0]), um padrão diferente é atribuído ao LED (led). Para cada valor do contador, o caso (case) é usado para definir o valor do LED.

O padrão do LED começa com um único LED aceso (led = 8'b00000001), e a cada incremento do contador, um LED é adicionado à direita (led <<= 1). Quando todos os LEDs estão acesos (led = 8'b11111111), o contador é reiniciado e o padrão recomeça. Quando o sinal de reset (rst) está em estado alto, todos os LEDs são apagados (led = 8'b00000000).

Esse código pode ser utilizado para criar um módulo em um FPGA (Field-Programmable Gate Array) ou outro dispositivo digital, controlando o padrão de iluminação de um conjunto de LEDs.