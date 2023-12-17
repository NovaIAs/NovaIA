Claro! Aqui está um exemplo de código em VHDL que implementa um multiplicador de números inteiros. Este código utiliza a técnica de multiplicação parcial, onde cada bit do multiplicador é multiplicado pelo multiplicando e somado ao resultado parcial.

```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplicador is
    generic (
        N_BITS : positive := 8  -- Número de bits do multiplicador e multiplicando
    );
    port (
        a : in std_logic_vector(N_BITS - 1 downto 0);  -- Multiplicando
        b : in std_logic_vector(N_BITS - 1 downto 0);  -- Multiplicador
        p : out std_logic_vector(2*N_BITS - 1 downto 0)  -- Produto
    );
end multiplicador;

architecture behav of multiplicador is
    signal parciais : std_logic_vector(2*N_BITS - 1 downto 0);  -- Resultado parcial
    signal carry : std_logic_vector(N_BITS - 1 downto 0);  -- Carry para a soma
begin
    process(a, b)
    begin
        -- Inicializa os sinais
        parciais <= (others => '0');
        carry <= (others => '0');
        
        -- Realiza a multiplicação parcial
        for i in 0 to N_BITS - 1 loop
            if b(i) = '1' then  -- Se o bit do multiplicador for 1
                parciais(i) <= a(0);  -- Copia o bit menos significativo do multiplicando
                carry <= ('0' & parciais(N_BITS - 1 downto 1)) & '0';  -- Shift para a esquerda do resultado parcial e do carry
            end if;
            a <= '0' & a(N_BITS - 1 downto 1);  -- Shift para a direita do multiplicando
        end loop;
        
        -- Realiza a soma dos resultados parciais
        for i in 1 to N_BITS - 1 loop
            parciais(N_BITS + i) <= parciais(N_BITS + i) xor carry(i - 1);  -- Soma XOR do resultado parcial com o carry
            carry <= carry(0) & carry(N_BITS - 1 downto 1);  -- Shift para a esquerda do carry
        end loop;
        
        -- Copia o resultado parcial para o sinal de saída
        p <= parciais;
    end process;
end behav;
```

Neste código, a entidade `multiplicador` possui dois sinais de entrada: `a` representando o multiplicando e `b` representando o multiplicador. O sinal de saída `p` representa o produto da multiplicação.

A arquitetura `behav` implementa a lógica do multiplicador. O processo utiliza loops para realizar a multiplicação parcial e a soma dos resultados parciais. No final, o resultado parcial é copiado para o sinal de saída `p`.

Este código implementa um multiplicador genérico com um número configurável de bits para o multiplicador e multiplicando. Você pode ajustar o valor de `N_BITS` na declaração da entidade para alterar o tamanho dos operandos.

Espero que este código atenda às suas necessidades! Se precisar de mais alguma ajuda, é só me falar.