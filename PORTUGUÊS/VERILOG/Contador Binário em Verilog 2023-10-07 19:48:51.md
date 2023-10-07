module contador_binario(
  input wire clk, // Sinal de clock
  input wire reset, // Sinal de reset
  output reg [7:0] count // Saída do contador de 8 bits
);
  
  always @(posedge clk or posedge reset) begin
    if (reset) // Se o sinal de reset for ativado
      count <= 8'b00000000; // Reinicia o contador para 0
    else // Caso contrário
      count <= count + 1; // Incrementa o contador em 1 a cada ciclo de clock
  end
  
endmodule

// Módulo principal
module top;
  
  reg clk; // Sinal de clock
  reg reset; // Sinal de reset
  wire [7:0] count; // Saída do contador
  
  // Gerador de clock
  always begin
    clk = 0; // Inicializa o sinal de clock
    #5; // Espera 5 unidades de tempo
    clk = 1; // Ativa o sinal de clock
    #5; // Espera mais 5 unidades de tempo
  end
  
  // Módulo do contador binário
  contador_binario contador(
    .clk(clk),
    .reset(reset),
    .count(count)
  );
  
  // Testbench
  initial begin
    reset = 1; // Ativa o sinal de reset
    #10; // Espera 10 unidades de tempo
    reset = 0; // Desativa o sinal de reset
    #100; // Espera 100 unidades de tempo
    $finish; // Termina a simulação
  end
  
endmodule

Neste código, é criado um contador binário de 8 bits em Verilog. O contador é incrementado a cada ciclo de clock, a menos que o sinal de reset seja ativado, nesse caso, o contador é reiniciado para 0. 

O módulo principal "top" é responsável por instanciar o contador_binario e gerar o sinal de clock. O sinal de clock é gerado por meio de um gerador de clock que alterna entre 0 e 1 a cada 5 unidades de tempo.

O testbench inicializa o sinal de reset, espera por 10 unidades de tempo, desativa o sinal de reset e depois espera por mais 100 unidades de tempo antes de finalizar a simulação.

Este código é um exemplo simples de um contador binário em Verilog e pode ser utilizado como ponto de partida para projetos mais complexos.