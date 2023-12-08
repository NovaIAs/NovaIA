Claro! Vou gerar um código em VHDL que implementa um contador ascendente de 0 a 9 com um sinal de saída quando o valor atingir 5. Aqui está o código:

```vhdl
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Contador is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           saida : out  STD_LOGIC);
end Contador;

architecture Behavioral of Contador is

    signal count : unsigned(3 downto 0);
    
begin

    process(clk, reset)
    begin
        if reset = '1' then
            count <= (others => '0');
        elsif rising_edge(clk) then
            if count = 4 then
                saida <= '1';
            else
                saida <= '0';
            end if;
            
            if count = 9 then
                count <= (others => '0');
            else
                count <= count + 1;
            end if;
        end if;
    end process;

end Behavioral;
```

Neste código, temos uma entidade chamada "Contador" que possui três sinais:

- `clk` - um sinal de clock utilizado para sincronizar o contador
- `reset` - um sinal de reset utilizado para reiniciar o contador
- `saida` - um sinal de saída que é ativado quando o contador atinge o valor 5

Na arquitetura "Behavioral", temos um processo que é sensível tanto ao `clk` quanto ao `reset`. Dentro do processo, temos a lógica para incrementar o contador e verificar se o valor atual é igual a 4 (5 em binário). Se for igual a 4, o sinal de saída é ativado. Se o valor atual for igual a 9, o contador é reiniciado para 0, caso contrário, ele é incrementado em 1.

Este código gera um contador de 0 a 9 e ativará o sinal de saída quando o valor do contador atingir 5. Ele usa uma estrutura de máquina de estados simples para controlar o fluxo do contador.