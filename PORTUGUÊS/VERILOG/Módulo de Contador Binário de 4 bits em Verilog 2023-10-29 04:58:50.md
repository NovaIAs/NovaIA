module contador_binario (
    input wire clk,
    input wire reset,
    output wire [3:0] count
);

reg [3:0] count_reg;

always @(posedge clk or posedge reset) begin
    if (reset)
        count_reg <= 4'b0000; // reinicia o contador para 0
    else
        count_reg <= count_reg + 1; // incrementa o contador em 1
end

assign count = count_reg;

endmodule

/*
Este código implementa um contador binário de 4 bits em Verilog. O módulo "contador_binario" possui uma entrada para o sinal de clock (clk), uma entrada para o sinal de reset (reset) e uma saída para o contador (count).

O contador é implementado usando um registrador (count_reg) que armazena o valor atual do contador. No processo always, o contador é incrementado em 1 a cada flanco de subida do sinal de clock (posedge clk) ou flanco de subida do sinal de reset (posedge reset). Quando o sinal de reset é acionado, o contador é reiniciado para 0.

A saída count é atribuída com o valor do registrador count_reg, representando o valor atual do contador.

Esse contador binário pode ser utilizado em projetos de circuitos digitais, como por exemplo, em sistemas de contagem ou sequenciamento.