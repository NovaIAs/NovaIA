module complexo_vhdl is

-- Declaração dos sinais e variáveis
signal a, b, c: std_logic;
signal resultado: std_logic_vector(7 downto 0);
signal somador_out: std_logic_vector(7 downto 0);
signal multiplicador_out: std_logic_vector(7 downto 0);
signal subtrator_out: std_logic_vector(7 downto 0);

-- Componentes
component somador is
    port(a, b: in std_logic_vector(7 downto 0);
         soma: out std_logic_vector(7 downto 0));
end component;

component multiplicador is
    port(a, b: in std_logic_vector(7 downto 0);
         multiplicacao: out std_logic_vector(7 downto 0));
end component;

component subtrator is
    port(a, b: in std_logic_vector(7 downto 0);
         subtracao: out std_logic_vector(7 downto 0));
end component;

-- Sinais internos
signal sinal_intermediario: std_logic_vector(7 downto 0);

begin

-- Conexões internas
sinal_intermediario <= a;

-- Instâncias dos componentes
somador1: somador port map(a => a, b => b, soma => somador_out);
multiplicador1: multiplicador port map(a => b, b => c, multiplicacao => multiplicador_out);
subtrator1: subtrator port map(a => somador_out, b => multiplicador_out, subtracao => subtrator_out);

-- Processo principal
process(a, b, c)
begin
    if a = '1' then
        resultado <= subtrator_out;
    else
        resultado <= multiplicador_out;
    end if;
end process;

end module;

-- Componente Somador
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity somador is
    port(a, b: in std_logic_vector(7 downto 0);
         soma: out std_logic_vector(7 downto 0));
end entity;

architecture behavior of somador is
begin
    process(a, b)
    begin
        soma <= std_logic_vector(unsigned(a) + unsigned(b));
    end process;
end architecture;

-- Componente Multiplicador
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplicador is
    port(a, b: in std_logic_vector(7 downto 0);
         multiplicacao: out std_logic_vector(7 downto 0));
end entity;

architecture behavior of multiplicador is
begin
    process(a, b)
    begin
        multiplicacao <= std_logic_vector(unsigned(a) * unsigned(b));
    end process;
end architecture;

-- Componente Subtrator
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity subtrator is
    port(a, b: in std_logic_vector(7 downto 0);
         subtracao: out std_logic_vector(7 downto 0));
end entity;

architecture behavior of subtrator is
begin
    process(a, b)
    begin
        subtracao <= std_logic_vector(unsigned(a) - unsigned(b));
    end process;
end architecture;
```

Este código em VHDL implementa um circuito complexo que realiza operações de soma, multiplicação e subtração com base nas entradas `a`, `b` e `c`. O resultado dessas operações é determinado por meio de uma estrutura de controle condicional. Se `a` for igual a '1', o resultado será a saída da subtração entre `somador_out` e `multiplicador_out`. Caso contrário, o resultado será o resultado da multiplicação entre `b` e `c`.

O código é dividido em três partes principais. A primeira parte é o módulo `complexo_vhdl`, que contém os sinais, variáveis, componentes e instâncias necessários para a implementação do circuito. O módulo possui um processo principal que controla a saída `resultado` com base na entrada `a`.

A segunda parte do código é o componente `somador`, que realiza a adição de dois números de 8 bits. Ele é implementado como um processo que atualiza a saída `soma` com a soma dos sinais de entrada `a` e `b`.

A terceira parte do código é o componente `multiplicador`, que realiza a multiplicação de dois números de 8 bits. Ele também é implementado como um processo que atualiza a saída `multiplicacao` com o produto dos sinais de entrada `a` e `b`.

O quarto e último componente é o `subtrator`, que realiza a subtração de dois números de 8 bits. Da mesma forma que os outros componentes, ele é implementado como um processo que atualiza a saída `subtracao` com a diferença entre os sinais de entrada `a` e `b`.

Esses componentes são então instanciados no módulo `complexo_vhdl` e interconectados de acordo com a lógica do circuito. O resultado final é determinado pelo processo principal, que seleciona o resultado da subtração ou da multiplicação com base no valor do sinal `a`.

Espero que este código atenda às suas necessidades complexas em VHDL. Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.