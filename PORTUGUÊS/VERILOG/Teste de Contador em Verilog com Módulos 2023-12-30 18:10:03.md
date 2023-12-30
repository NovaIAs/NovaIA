module contador (input clk, input rst, output reg [3:0] cont);

  always @(posedge clk or posedge rst) begin
    if (rst)
      cont <= 0;
    else
      cont <= cont + 1;
  end
  
endmodule

module somador_subtrator (input [3:0] a, input [3:0] b, input sel, output reg [3:0] result, output reg overflow);

  always @(a, b, sel) begin
    if (sel) // sel = 1, subtrai
      result <= a - b;
    else // sel = 0, soma
      result <= a + b;
    
    if (result > 15)
      overflow <= 1;
    else
      overflow <= 0;
  end
  
endmodule

module teste_contador;

  reg clk;
  reg rst;
  wire [3:0] cont;
  
  initial begin
    clk = 0;
    rst = 0;
    
    #10 rst = 1;
    #10 rst = 0;
    
    #40 $finish;
  end
  
  always #5 clk = ~clk;
  
  contador contador_inst (.clk(clk), .rst(rst), .cont(cont));
  
endmodule

module teste_somador_subtrator;

  reg [3:0] a;
  reg [3:0] b;
  reg sel;
  wire [3:0] result;
  wire overflow;
  
  initial begin
    a = 5;
    b = 3;
    sel = 0;
    
    #10 sel = 1;
    #10 sel = 0;
    
    #40 $finish;
  end
  
  somador_subtrator somador_subtrator_inst (.a(a), .b(b), .sel(sel), .result(result), .overflow(overflow));
  
endmodule

module teste_completo;

  reg clk;
  reg rst;
  reg [3:0] a;
  reg [3:0] b;
  reg sel;
  wire [3:0] cont;
  wire [3:0] result;
  wire overflow;
  
  initial begin
    clk = 0;
    rst = 0;
    a = 5;
    b = 3;
    sel = 0;
    
    #10 rst = 1;
    #10 rst = 0;
    
    #10 sel = 1;
    #10 sel = 0;
    
    #40 $finish;
  end
  
  always #5 clk = ~clk;
  
  contador contador_inst (.clk(clk), .rst(rst), .cont(cont));
  
  somador_subtrator somador_subtrator_inst (.a(a), .b(b), .sel(sel), .result(result), .overflow(overflow));
  
endmodule

Neste código em Verilog, temos 3 módulos: contador, somador_subtrator e teste_completo.

O módulo contador é responsável por implementar um contador de 4 bits. Ele possui uma entrada de clock (clk), uma entrada de reset (rst) e uma saída (cont) de 4 bits. O contador começa em 0 e a cada ciclo de clock ele incrementa seu valor em 1, a menos que o sinal de reset (rst) esteja em nível alto, o que zera o contador.

O módulo somador_subtrator é responsável por realizar a operação de soma ou subtração entre dois números de 4 bits. Ele possui duas entradas (a e b) de 4 bits que representam os operandos, uma entrada (sel) para selecionar se a operação é de soma ou subtração, uma saída (result) de 4 bits para o resultado da operação e uma saída (overflow) para indicar se houve overflow na operação. O resultado da operação é obtido através de uma estrutura condicional que verifica o valor de sel. Se sel for igual a 1, a operação é de subtração, caso contrário, é de soma. O resultado é atribuído ao sinal result e o sinal overflow é definido como 1 caso o resultado seja maior que 15, caso contrário, é definido como 0.

O módulo teste_contador é responsável por testar o módulo contador. Ele possui um sinal de clock (clk), um sinal de reset (rst) e uma saída (cont) de 4 bits. Ele inicializa o sinal de clock e de reset, e em seguida, a cada 10 unidades de tempo, o sinal de reset é alternado entre 1 e 0. O contador_inst é uma instância do módulo contador que recebe os sinais de clk, rst e cont.

O módulo teste_somador_subtrator é responsável por testar o módulo somador_subtrator. Ele possui dois sinais (a e b) de 4 bits que representam os operandos, um sinal (sel) para selecionar a operação, uma saída (result) de 4 bits para o resultado da operação e um sinal (overflow) para indicar se houve overflow. Ele inicializa os sinais a, b e sel e, a cada 10 unidades de tempo, o sinal sel é alternado entre 1 e 0. O somador_subtrator_inst é uma instância do módulo somador_subtrator que recebe os sinais a, b, sel, result e overflow.

O módulo teste_completo é responsável por testar tanto o módulo contador quanto o módulo somador_subtrator. Ele possui um sinal de clock (clk), um sinal de reset (rst), dois sinais (a e b) de 4 bits para os operandos, um sinal (sel) para selecionar a operação, duas saídas (cont e result) de 4 bits para os resultados e um sinal (overflow) para indicar se houve overflow. Ele inicializa todos os sinais e executa os testes do contador e do somador_subtrator. O contador_inst e o somador_subtrator_inst são instâncias dos módulos contador e somador_subtrator, respectivamente, que recebem os sinais corretos.