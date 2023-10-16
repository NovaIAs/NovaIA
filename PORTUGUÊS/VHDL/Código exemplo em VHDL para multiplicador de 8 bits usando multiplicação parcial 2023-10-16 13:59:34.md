Claro! Aqui está um exemplo de código em VHDL que implementa um multiplicador de 8 bits utilizando a técnica de multiplicação parcial:

```vhdl
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity multiplicador is
    port (
        clk : in std_logic;
        reset : in std_logic;
        a : in std_logic_vector(7 downto 0);
        b : in std_logic_vector(7 downto 0);
        resultado : out std_logic_vector(15 downto 0)
    );
end multiplicador;

architecture behavioral of multiplicador is
    signal multiplicando : std_logic_vector(7 downto 0);
    signal multiplicador : std_logic_vector(7 downto 0);
    signal soma_parcial : std_logic_vector(15 downto 0);
    signal carry : std_logic;

begin
    process (clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                multiplicando <= (others => '0');
                multiplicador <= (others => '0');
                soma_parcial <= (others => '0');
                carry <= '0';
            else
                multiplicando <= a;
                multiplicador <= b;
                carry <= '0';

                for i in 0 to 7 loop
                    if multiplicando(i) = '1' then
                        soma_parcial(i downto 0) <= std_logic_vector(unsigned(soma_parcial(i downto 0)) + unsigned(multiplicador));
                    end if;
                    carry <= soma_parcial(i);
                    soma_parcial(i) <= carry;
                end loop;
            end if;
        end if;
    end process;

    resultado <= soma_parcial;
end behavioral;
```

Este código VHDL implementa um multiplicador que recebe dois operandos, `a` e `b`, ambos de 8 bits, e gera o resultado da multiplicação em `resultado`, que é um vetor de 16 bits. O multiplicador utiliza a técnica de multiplicação parcial, que é uma abordagem eficiente para multiplicação de números binários.

O processo principal é sensível à borda de subida do sinal `clk`. Quando ocorre uma borda de subida, o processo verifica se o sinal `reset` está em nível alto. Se sim, todos os sinais são resetados para seus valores iniciais. Caso contrário, os sinais `multiplicando` e `multiplicador` recebem os valores dos operandos `a` e `b`, respectivamente, e o sinal `carry` é resetado.

Em seguida, o loop `for` é executado oito vezes, uma para cada bit do multiplicando. Dentro do loop, verificamos se o bit atual do multiplicando é igual a '1'. Se sim, somamos o multiplicador ao valor parcial armazenado em `soma_parcial`. O resultado da soma é armazenado novamente em `soma_parcial`. Além disso, atualizamos o valor do sinal `carry` para permitir o acúmulo correto dos bits menos significativos.

Finalmente, o resultado final da multiplicação é atribuído ao sinal `resultado`.

Espero que este exemplo atenda às suas expectativas!