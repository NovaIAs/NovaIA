module somador (
    input [7:0] a,
    input [7:0] b,
    output [8:0] soma
);
 
reg [8:0] temp;
 
always @(*) begin
    temp = {1'b0, a} + {1'b0, b}; // Adiciona zero à esquerda para garantir o carry-in adequado
end
 
assign soma = temp;
 
endmodule

// Neste código, estamos criando um módulo VERILOG chamado "somador" que recebe dois números de 8 bits (a e b) e retorna a soma deles em um registrador de 9 bits chamado "soma".

// O registrador "temp" é declarado como um registrador de 9 bits que armazenará temporariamente o resultado da soma.

// Em seguida, usamos uma instrução always @(*) begin para definir a lógica de funcionamento do somador.

// O operador "+" é usado para adicionar o valor de "a" com o valor de "b", mas adicionamos um zero à esquerda de ambos os valores (usando {1'b0, a} e {1'b0, b}) antes de realizar a soma. Isso é feito para garantir que o carry-in adequado seja considerado durante a soma.

// Por fim, atribuímos o valor do registrador "temp" ao sinal de saída "soma" usando a instrução assign soma = temp.

// Com este código, podemos criar um módulo somador em VERILOG que soma dois números de 8 bits.