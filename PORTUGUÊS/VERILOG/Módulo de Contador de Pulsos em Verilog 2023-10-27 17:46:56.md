module contador_de_pulsos (
    input clk,
    input reset,
    output reg [7:0] saida
);

reg [7:0] contador;

always @(posedge clk or posedge reset)
begin
    if (reset)
        contador <= 0;
    else
        contador <= contador + 1;
end

assign saida = contador;

endmodule

// Neste código em Verilog, temos um módulo chamado "contador_de_pulsos" que recebe dois sinais de entrada e um sinal de saída.
// A entrada "clk" é um sinal de clock que será usado para sincronizar as operações no circuito.
// A entrada "reset" é um sinal de reset que será usado para redefinir o contador para zero.
// A saída "saida" é um sinal de 8 bits que representa o valor atual do contador.

// O contador é representado por um registrador de 8 bits chamado "contador".
// O registrador é atualizado em cada borda de subida do sinal de clock ou borda de subida do sinal de reset.
// Se o sinal de reset estiver em estado alto, o contador será redefinido para zero.
// Caso contrário, o contador será incrementado em 1.

// A saída do contador é atribuída ao sinal de saída "saida".

// Este código implementa um contador de pulsos simples em Verilog. Cada vez que o sinal de clock tem uma borda de subida, o contador é incrementado em 1, a menos que o sinal de reset esteja em estado alto, nesse caso o contador é redefinido para zero. A saída do contador representa o valor atual do contador.