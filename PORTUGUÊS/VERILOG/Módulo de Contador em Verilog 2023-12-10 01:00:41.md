module contador (
    input clk,
    input reset,
    output reg [7:0] count
);

reg [7:0] temp_count;

always @(posedge clk or posedge reset)
begin
    if (reset)
        temp_count <= 0;
    else
        temp_count <= temp_count + 1;
end

always @(posedge clk)
begin
    if (reset)
        count <= 0;
    else
        count <= temp_count;
end

endmodule

// Módulo de contador em Verilog
// Este código implementa um contador de 8 bits que incrementa a cada pulso de clock.
// Ele tem uma entrada de reset para redefinir o contador para 0.
// O contador é armazenado em um registrador temporário "temp_count" e, em seguida, transferido para o registrador de saída "count" no próximo pulso de clock.
// O valor do contador é incrementado em 1 a cada pulso de clock, a menos que haja um sinal de reset ativo.
// Quando o sinal de reset é ativado, o contador é redefinido para 0.
// O módulo possui uma entrada de clock "clk", uma entrada de reset "reset" e uma saída de registrador de 8 bits "count".