Claro! Aqui está um código em VHDL que realiza uma multiplicação de dois números inteiros de 8 bits, utilizando multiplicação paralela e uma estrutura de somadores paralelos.

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplicacao_paralela is
    port (
        a, b: in std_logic_vector(7 downto 0);
        resultado: out std_logic_vector(15 downto 0)
    );
end multiplicacao_paralela;

architecture behavioral of multiplicacao_paralela is
    signal multiplicador: std_logic_vector(7 downto 0);
    signal multiplicando: std_logic_vector(7 downto 0);
    signal somador: std_logic_vector(15 downto 0);
    signal produto_parcial: std_logic_vector(15 downto 0);
    
begin
    process(a, b)
    begin
        multiplicador <= a;
        multiplicando <= b;
        
        for i in 0 to 7 loop
            if multiplicador(i) = '1' then
                produto_parcial <= (multiplicando & (others => '0')) * (2**i);
                somador <= somador + produto_parcial;
            end if;
        end loop;
        
        resultado <= somador;
    end process;
end behavioral;
```

Neste código, temos a entidade `multiplicacao_paralela` com as portas de entrada `a` e `b`, que são os dois números a serem multiplicados, e a porta de saída `resultado` que é o resultado da multiplicação.

A arquitetura `behavioral` contém um processo sensível às variáveis `a` e `b`. Dentro do processo, temos a declaração de sinais `multiplicador`, `multiplicando`, `somador` e `produto_parcial`, que serão utilizados para a multiplicação e soma dos números.

Em seguida, temos um loop `for` que percorre os bits do multiplicador (de 0 a 7). Se o bit do multiplicador for igual a 1, multiplicamos o multiplicando por 2^i (onde i é a posição do bit no multiplicador). O resultado parcial é armazenado em `produto_parcial` e somado ao `somador`. Ao final do loop, o resultado final é atribuído à saída `resultado`.

Espero que este exemplo atenda às suas expectativas e seja útil para você!