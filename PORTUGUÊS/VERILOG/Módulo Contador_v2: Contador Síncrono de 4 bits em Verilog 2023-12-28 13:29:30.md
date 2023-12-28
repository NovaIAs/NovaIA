module contador_v2 (
    input wire clk,
    input wire reset,
    output wire [3:0] count
);

    reg [3:0] count_reg;

    always @(posedge clk or posedge reset) begin
        if (reset)
            count_reg <= 4'b0000;
        else if (count_reg == 4'b1001)
            count_reg <= 4'b0000;
        else
            count_reg <= count_reg + 1;
    end

    assign count = count_reg;

endmodule

// Este código é um exemplo de um contador síncrono de 4 bits em Verilog. Ele utiliza uma máquina de estados para contar de 0 a 9 e reiniciar a contagem novamente.

// O módulo "contador_v2" possui três portas: "clk" (entrada), "reset" (entrada) e "count" (saída). A porta "clk" é o sinal de clock que sincroniza a operação do contador. A porta "reset" é um sinal de reset ativo em nível alto que zera a contagem. A porta "count" é a saída de 4 bits que representa o valor atual do contador.

// Dentro do módulo, temos a declaração de um registrador "count_reg" de 4 bits que armazena o valor atual do contador. 

// O bloco "always" é um bloco sequencial que é executado a cada borda de subida do sinal de clock ou ao ser acionado o sinal de reset. 

// Na condição "if", verificamos se o sinal de reset está ativo. Se estiver, o contador é reiniciado para o valor 0. 

// Na condição "else if", verificamos se o valor atual do contador é igual a 9 (4'b1001). Se for, o contador é reiniciado para 0. 

// Caso contrário, o contador é incrementado em 1 (count_reg <= count_reg + 1).

// Por fim, a saída "count" é atribuída ao valor atual do contador "count_reg".

// Esse código é uma implementação simples de um contador síncrono de 4 bits em Verilog. Ele pode ser utilizado em diversos projetos onde a contagem é necessária, como em displays de sete segmentos, por exemplo.