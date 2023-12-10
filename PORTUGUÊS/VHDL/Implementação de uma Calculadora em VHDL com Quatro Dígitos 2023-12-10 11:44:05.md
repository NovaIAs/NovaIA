Claro! Aqui está um exemplo de código em VHDL que implementa uma calculadora simples de quatro dígitos. O código está comentado para facilitar a compreensão:

```vhdl
-- Definindo uma entidade para a calculadora
entity Calculadora is
    Port (
        -- Entradas da calculadora
        A, B : in std_logic_vector(3 downto 0);
        Op : in std_logic_vector(1 downto 0);
        
        -- Saídas da calculadora
        Resultado : out std_logic_vector(7 downto 0);
        Overflow : out std_logic
    );
end Calculadora;

-- Arquitetura da calculadora
architecture Behavioral of Calculadora is
    -- Sinais internos
    signal Soma, Subtracao, Multiplicacao, Divisao : std_logic_vector(7 downto 0);
    
begin
    -- Processo responsável por realizar as operações
    process(A, B, Op)
    begin
        case Op is
            when "00" => -- Soma
                Soma <= A + B;
                
            when "01" => -- Subtração
                Subtracao <= A - B;
                
            when "10" => -- Multiplicação
                Multiplicacao <= A * B;
                
            when "11" => -- Divisão
                if B /= "0000" then
                    Divisao <= A / B;
                else
                    Divisao <= (others => 'X'); -- Valor indefinido em caso de divisão por zero
                end if;
        end case;
    end process;
    
    -- Saída do resultado
    Resultado <= Soma when Op = "00" else
                 Subtracao when Op = "01" else
                 Multiplicacao when Op = "10" else
                 Divisao;
                 
    -- Sinal de overflow
    Overflow <= '1' when (Op = "10" and (Multiplicacao > 15 or Multiplicacao < -15)) or
                         (Op = "11" and Divisao = (others => 'X')) else
                '0';
end Behavioral;
```

Este código VHDL define uma entidade chamada "Calculadora" com entradas "A", "B" e "Op" representando os operandos e a operação a ser realizada, e também saídas "Resultado" e "Overflow" representando o resultado da operação e um sinal de overflow, respectivamente.

A arquitetura "Behavioral" contém um processo sensível às mudanças nas entradas "A", "B" e "Op". Dependendo do valor de "Op", o processo realiza a operação adequada (soma, subtração, multiplicação ou divisão) e armazena o resultado em sinais internos.

Em seguida, o resultado é selecionado de acordo com o valor de "Op" e atribuído à saída "Resultado". Além disso, é verificado se ocorreu um overflow durante a operação e o sinal correspondente é gerado na saída "Overflow".

Este código VHDL é apenas um exemplo de como uma calculadora básica pode ser implementada em VHDL. É importante mencionar que este código pode ser otimizado e melhorado para atender às necessidades específicas do projeto.