Claro! Aqui está um exemplo de um código complexo em VHDL para implementar um processador simples de 8 bits:

```vhdl
-- Definindo a entidade principal
entity processador_8bits is
    Port ( clk : in std_logic;                 -- Sinal de clock
           reset : in std_logic;               -- Sinal de reset
           in_data : in std_logic_vector(7 downto 0);  -- Dados de entrada
           out_data : out std_logic_vector(7 downto 0); -- Dados de saída
           out_valid : out std_logic;          -- Sinal de validade dos dados de saída
           out_ready : in std_logic);          -- Sinal de pronto para receber novos dados de entrada
end processador_8bits;

architecture Behavioral of processador_8bits is
    -- Definição dos registradores internos
    signal reg_a, reg_b, reg_out : std_logic_vector(7 downto 0);
    signal alu_result : std_logic_vector(7 downto 0);
    signal data_valid : std_logic := '0';

begin
    -- Processo principal
    process(clk, reset)
    begin
        if reset = '1' then
            -- Reseta todos os registradores
            reg_a <= (others => '0');
            reg_b <= (others => '0');
            reg_out <= (others => '0');
            data_valid <= '0';
        elsif rising_edge(clk) then
            -- Lógica do processador
            if out_ready = '1' then
                -- Registra o dado de entrada quando estiver pronto para receber novos dados
                reg_a <= in_data;
            end if;
            
            -- Operações da Unidade Lógico-Aritmética (ALU)
            alu_result <= reg_a + reg_b;  -- Soma dos registradores A e B
            
            -- Saída do processador
            if data_valid = '1' then
                out_data <= reg_out;
            else
                out_data <= (others => '0');
            end if;
            
            out_valid <= data_valid;
        end if;
    end process;

    -- Lógica para atualizar o registrador B quando estiver pronto para receber novos dados
    process(clk, reset)
    begin
        if reset = '1' then
            reg_b <= (others => '0');
        elsif rising_edge(clk) then
            if out_ready = '1' then
                reg_b <= in_data;
            end if;
        end if;
    end process;

    -- Lógica para atualizar o registrador de saída quando um dado válido é gerado
    process(clk, reset)
    begin
        if reset = '1' then
            reg_out <= (others => '0');
            data_valid <= '0';
        elsif rising_edge(clk) then
            if alu_result /= (others => '0') then
                reg_out <= alu_result;
                data_valid <= '1';
            else
                reg_out <= (others => '0');
                data_valid <= '0';
            end if;
        end if;
    end process;

end Behavioral;
```

Neste exemplo, temos a implementação de um processador simples de 8 bits em VHDL. Ele possui uma unidade lógico-aritmética (ALU) que realiza a soma dos registradores A e B. O dado de entrada é registrado no registrador A quando o sinal de pronto para receber novos dados (out_ready) está em nível lógico alto. O registrador B é atualizado quando também estiver pronto para receber novos dados. O resultado da ALU é armazenado no registrador de saída (reg_out) e é considerado válido (data_valid = '1') quando for diferente de zero. Os sinais de saída (out_data, out_valid) são atualizados de acordo com o valor do registrador de saída e o sinal de validade dos dados.

Este é apenas um exemplo básico de um processador em VHDL. A complexidade pode variar dependendo da funcionalidade desejada e das instruções a serem implementadas.