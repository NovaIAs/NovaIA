Claro, vou gerar um código em VHDL para você. Este código é um exemplo de um contador de 4 bits ascendente em VHDL. Vou explicar o código em detalhes para que você possa compreendê-lo.

```vhdl
-- Declaração da entidade do contador
entity contador is
    port (
        reset : in std_logic;    -- Sinal de reset
        clk   : in std_logic;    -- Sinal de clock
        out   : out std_logic_vector(3 downto 0)   -- Saída do contador de 4 bits
    );
end contador;

-- Implementação da arquitetura do contador
architecture Behavioral of contador is
    signal counter : std_logic_vector(3 downto 0);    -- Sinal de contador interno
    
begin
    
    -- Processo para controlar o contador
    process (clk, reset)
    begin
        
        -- Caso o sinal de reset esteja ativo, reinicia o contador
        if reset = '1' then
            counter <= (others => '0');
        
        -- Caso o sinal de clock tenha uma borda de subida, incrementa o contador
        elsif rising_edge(clk) then
            counter <= counter + 1;
        end if;
        
    end process;
    
    -- Atribuição da saída do contador
    out <= counter;
    
end Behavioral;
```

Explicação do código:
- A entidade `contador` é declarada com três portas: `reset`, `clk` e `out`. A porta `reset` é um sinal de entrada para reiniciar o contador, `clk` é um sinal de entrada para o clock e `out` é a saída do contador de 4 bits.
- A arquitetura `Behavioral` implementa o comportamento do contador.
- Dentro da arquitetura, um sinal interno chamado `counter` é declarado como um vetor de 4 bits, representando o contador.
- Em seguida, um processo é definido para controlar o funcionamento do contador. Esse processo é sensível às mudanças nos sinais `clk` e `reset`.
- Se o sinal `reset` estiver ativo (valor '1'), o contador é reiniciado para zero.
- Se ocorrer uma borda de subida no sinal `clk`, o contador é incrementado em 1.
- A saída do contador é atribuída ao sinal `out`.
- Assim, sempre que o sinal `clk` tiver uma borda de subida, o contador irá incrementar e o valor atual será refletido na saída `out`.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação ou dúvida, por favor, me avise.