module contador (
    input wire clk,
    input wire reset,
    output reg [3:0] count
  );
  
  reg [3:0] next_count;
  
  always @(posedge clk or posedge reset) begin
    if (reset) begin
      next_count <= 4'b0000;
    end else begin
      case (count)
        4'b0000: next_count <= 4'b0001;
        4'b0001: next_count <= 4'b0010;
        4'b0010: next_count <= 4'b0011;
        4'b0011: next_count <= 4'b0100;
        4'b0100: next_count <= 4'b0101;
        4'b0101: next_count <= 4'b0110;
        4'b0110: next_count <= 4'b0111;
        4'b0111: next_count <= 4'b1000;
        4'b1000: next_count <= 4'b1001;
        4'b1001: next_count <= 4'b1010;
        4'b1010: next_count <= 4'b1011;
        4'b1011: next_count <= 4'b1100;
        4'b1100: next_count <= 4'b1101;
        4'b1101: next_count <= 4'b1110;
        4'b1110: next_count <= 4'b1111;
        4'b1111: next_count <= 4'b0000;
      endcase
    end
  end
  
  always @(posedge clk or posedge reset) begin
    if (reset) begin
      count <= 4'b0000;
    end else begin
      count <= next_count;
    end
  end
  
endmodule

Neste código em Verilog, estamos criando um módulo chamado "contador". O contador possui uma entrada "clk" (clock) e uma entrada "reset" para reiniciar o contador. Ele também possui uma saída "count" que armazena o valor atual do contador.

Dentro do módulo, declaramos uma variável reg [3:0] chamada "next_count" para armazenar o próximo valor do contador.

Em seguida, utilizamos dois blocos "always" para definir o comportamento do contador. O primeiro bloco "always" é acionado na borda de subida do clock ou no reset. Dentro deste bloco, temos uma estrutura condicional "if-else" para verificar se o reset está ativo. Se estiver, atribuímos o valor 4'b0000 à variável "next_count". Caso contrário, usamos uma estrutura "case" para determinar o próximo valor do contador com base no valor atual de "count".

O segundo bloco "always" também é acionado na borda de subida do clock ou no reset. Neste bloco, novamente verificamos se o reset está ativo. Se estiver, atribuímos o valor 4'b0000 diretamente a "count". Caso contrário, atribuímos o valor de "next_count" a "count".

Dessa forma, o módulo contador é capaz de contar de 0 a 15, reiniciando para 0 após atingir 15, sempre que o clock estiver ativo e o reset estiver desativado.