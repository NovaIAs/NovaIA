module contador (
    input wire clk,
    input wire reset,
    output wire [7:0] count
);

reg [7:0] count_reg;

always @(posedge clk or posedge reset)
begin
    if (reset)
        count_reg <= 8'b00000000;
    else
        count_reg <= count_reg + 1;
end

assign count = count_reg;

endmodule

// Neste exemplo, criei um módulo Verilog chamado "contador" que possui uma entrada de clock (clk), uma entrada de reset (reset) e uma saída de contagem (count) de 8 bits.

// Dentro do módulo, declarei um registrador (reg) chamado count_reg de 8 bits para armazenar a contagem.

// Usei um bloco always @(posedge clk or posedge reset) para controlar a atualização da contagem. Sempre que ocorrer uma borda de subida do sinal de clock ou uma borda de subida do sinal de reset, o bloco sempre será executado.

// Dentro do bloco, usei uma estrutura de controle if-else para verificar se o sinal de reset está ativado ou desativado. Se o sinal de reset estiver ativado, a contagem será reiniciada para zero (8'b00000000). Caso contrário, a contagem será incrementada em 1.

// Finalmente, atribuí a saída "count" ao valor atual do registrador count_reg.

// Este código cria um contador de 8 bits que incrementa sua contagem a cada borda de subida do clock, a menos que o sinal de reset seja ativado, reiniciando a contagem para zero. A saída "count" representa a contagem atual do contador.