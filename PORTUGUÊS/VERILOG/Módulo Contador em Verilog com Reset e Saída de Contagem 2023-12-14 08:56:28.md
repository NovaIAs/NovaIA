module contador (
    input clk,
    input reset,
    output reg [15:0] contagem
);

reg [15:0] contador_interno;

always @(posedge clk or posedge reset) begin
    if (reset) begin
        contador_interno <= 0;
    end else begin
        contador_interno <= contador_interno + 1;
    end
end

assign contagem = contador_interno;

endmodule

Neste código em Verilog, é criado um módulo chamado "contador" que possui uma entrada de clock ("clk"), uma entrada de reset ("reset") e uma saída de contagem ("contagem") de 16 bits.

Dentro do módulo, é declarado um registrador ("reg") de 16 bits chamado "contador_interno", que será responsável por armazenar o valor da contagem.

Em um bloco "always" sensível à borda de subida do clock ("posedge clk") ou à borda de subida do sinal de reset ("posedge reset"), é definido o comportamento do contador.

Se o sinal de reset estiver ativo (valor lógico alto), o registrador contador_interno é resetado para zero.

Caso contrário, o registrador contador_interno é incrementado em uma unidade a cada borda de subida do clock.

Por fim, a atribuição "assign" é utilizada para conectar a saída "contagem" ao valor do registrador contador_interno.

Assim, este módulo "contador" pode ser utilizado para criar um contador de 16 bits que realiza a contagem a cada borda de subida do clock, reiniciando para zero quando o sinal de reset está ativo.