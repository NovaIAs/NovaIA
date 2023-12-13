module contador (
   input wire clk,
   input wire rst,
   output wire [7:0] count
   );

   reg [7:0] contador;

   always @(posedge clk or posedge rst)
   begin
      if (rst)
         contador <= 0;
      else
         contador <= contador + 1;
   end

   assign count = contador;

endmodule

// Módulo do contador

// Este código em Verilog implementa um contador de 8 bits. O contador incrementa seu valor em 1 a cada pulso de clock. O sinal de reset (rst) permite reiniciar o contador para zero. O valor atual do contador é representado pelo sinal de saída count, que é uma palavra de 8 bits.

// O código começa definindo o módulo "contador" com três portas: clk (pulso de clock), rst (reset) e count (saída).

// Em seguida, declaramos um registrador de 8 bits chamado "contador" que armazenará o valor atual do contador.

// O bloco "always" define o comportamento do contador. Ele é sensível a borda de subida do sinal de clock (posedge clk) e também à borda de subida do sinal de reset (posedge rst). Dentro do bloco, há uma estrutura condicional que verifica se o sinal de reset está ativo (rst). Se estiver, o contador é reiniciado para zero (contador <= 0). Caso contrário, o contador é incrementado em 1 (contador <= contador + 1).

// Finalmente, o sinal de saída count é atribuído ao valor atual do contador usando a declaração "assign count = contador". Isso permite que o valor do contador seja acessado externamente.

// Esse código pode ser utilizado em projetos de circuitos digitais, como em um contador de eventos ou em componentes de CPUs.