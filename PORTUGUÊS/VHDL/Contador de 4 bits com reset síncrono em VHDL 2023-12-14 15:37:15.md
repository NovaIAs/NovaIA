Claro! Aqui está um exemplo de código em VHDL que implementa um contador de 4 bits com reset síncrono:

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity contador is
    port (
        clk : in std_logic;
        reset : in std_logic;
        enable : in std_logic;
        count : out unsigned(3 downto 0)
    );
end contador;

architecture behavioral of contador is
    signal internal_count : unsigned(3 downto 0);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                internal_count <= (others => '0');
            elsif enable = '1' then
                internal_count <= internal_count + 1;
            end if;
        end if;
    end process;
    
    count <= internal_count;
end behavioral;
```

Aqui, declaramos uma entidade chamada "contador" com quatro portas: "clk" para o sinal do clock, "reset" para o sinal de reset, "enable" para o sinal de habilitação e "count" como saída para o contador de 4 bits.

Na arquitetura "behavioral", criamos um sinal interno chamado "internal_count" do tipo "unsigned" de 4 bits para armazenar o valor do contador.

Em seguida, criamos um processo sensível à borda de subida do sinal "clk". Dentro do processo, verificamos se o sinal "reset" está ativo. Se estiver, definimos o "internal_count" como todos os bits '0'. Caso contrário, se o sinal "enable" estiver ativo, incrementamos o "internal_count" em 1.

Por fim, atribuímos o valor de "internal_count" ao sinal "count" para que possa ser acessado externamente.

Espero que este exemplo seja útil para você! Se tiver alguma dúvida, estou à disposição para ajudar.