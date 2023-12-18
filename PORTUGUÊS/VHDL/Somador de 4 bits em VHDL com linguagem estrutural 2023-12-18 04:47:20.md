Claro! Aqui está um exemplo de código em VHDL que implementa um somador de 4 bits utilizando a linguagem estrutural:

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity somador_4bits is
  port (
    a, b: in std_logic_vector(3 downto 0);
    cin: in std_logic;
    sum: out std_logic_vector(3 downto 0);
    cout: out std_logic
  );
end somador_4bits;

architecture estrutural of somador_4bits is
  component full_adder is
    port (
      a, b, cin: in std_logic;
      sum, cout: out std_logic
    );
  end component;
  
  signal c: std_logic_vector(3 downto 0);
  
begin
  sum(0) <= a(0) xor b(0) xor cin;
  
  FA1: full_adder port map (a(1), b(1), c(0), sum(1), c(1));
  FA2: full_adder port map (a(2), b(2), c(1), sum(2), c(2));
  FA3: full_adder port map (a(3), b(3), c(2), sum(3), cout);
  
end estrutural;

architecture comportamental of full_adder is
begin
  sum <= a xor b xor cin;
  cout <= (a and b) or (cin and (a xor b));
end comportamental;
```

Neste código, é criada uma entidade `somador_4bits` que possui as entradas `a`, `b` e `cin`, representando os operandos e o bit de carry-in, respectivamente. As saídas são `sum`, que representa o resultado da soma, e `cout`, que representa o carry-out.

Dentro da arquitetura `estrutural`, é utilizado um componente `full_adder` para realizar a soma de cada bit individualmente. O sinal `c` é um vetor de carry que é propagado entre os componentes `full_adder`. O resultado final é obtido através das saídas dos componentes `full_adder`.

Por fim, o componente `full_adder` é definido na arquitetura `comportamental`, utilizando operações lógicas para calcular o resultado e o carry-out.