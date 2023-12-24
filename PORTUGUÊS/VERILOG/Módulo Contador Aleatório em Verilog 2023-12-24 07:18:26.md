module contador_aleatorio(
    input wire clk,
    input wire reset,
    output wire [7:0] contador
);

reg [7:0] contador;
reg [7:0] aleatorio;

always @(posedge clk or posedge reset) begin
    if (reset) begin
        contador <= 8'b0;
        aleatorio <= 8'b0;
    end else begin
        if (contador == 8'b0) begin
            aleatorio <= $random;
            contador <= aleatorio;
        end else begin
            contador <= contador - 1;
        end
    end
end

endmodule

// Neste código em Verilog, estamos criando um módulo chamado "contador_aleatorio" que tem uma entrada de clock ("clk"), uma entrada de reset ("reset") e uma saída de contador ("contador") de 8 bits.

// O contador é inicializado em 0 quando o reset é ativado. Quando o clock é detectado em uma borda de subida e o reset está desativado, o código verifica se o contador é igual a 0. Se sim, isso significa que precisamos gerar um novo número aleatório.

// Para gerar um número aleatório, usamos a função "$random" do Verilog, que gera um número aleatório de 32 bits. Neste caso, estamos armazenando os 8 bits menos significativos desse número na variável "aleatorio".

// Em seguida, atribuímos o valor de "aleatorio" a "contador", para que o contador comece a contar a partir desse número aleatório.

// Caso o contador não seja zero, ele é decrementado em 1 a cada borda de subida do clock.

// Assim, temos um contador que conta de forma decrescente de um número aleatório até zero, reiniciando e gerando um novo número aleatório quando chega a zero novamente.