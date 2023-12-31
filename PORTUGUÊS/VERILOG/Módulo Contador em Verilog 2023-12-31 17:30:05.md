module contador (
    input wire clk,
    input wire rst,
    output wire [3:0] cont
);

reg [3:0] cont;

always @(posedge clk or posedge rst) begin
    if (rst)
        cont <= 0;
    else if (cont == 9)
        cont <= 0;
    else
        cont <= cont + 1;
end

endmodule

// Neste código, estou criando um módulo em Verilog chamado "contador". O módulo possui três entradas (clk, rst) e uma saída (cont).
// O sinal de clk é utilizado como clock para sincronizar a operação do contador. O sinal de rst é utilizado como um sinal de reset para reiniciar o contador.
// A saída cont é um sinal de 4 bits que representa o valor atual do contador.

// A partir da linha 5, estou declarando um registrador (reg) de 4 bits chamado "cont" que será usado para armazenar o valor atual do contador.

// A partir da linha 7, estou utilizando a estrutura "always" para criar um bloco de código que será executado sempre que ocorrer uma borda de subida na entrada clk ou rst.

// Na linha 8, estou verificando se o sinal rst está ativo. Se estiver, o valor do contador será resetado para zero.

// Na linha 10, estou verificando se o valor atual do contador é igual a 9. Se sim, o valor do contador será resetado para zero.

// Na linha 12, estou incrementando o valor do contador em 1 caso nenhuma das condições anteriores seja satisfeita.

// Ao final do código, estou fechando o módulo utilizando a palavra-chave "endmodule".