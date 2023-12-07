Claro! Vou criar um código em VHDL para um processador simples de 8 bits, explicando passo a passo. Este código é apenas um exemplo e não representa um processador funcional completo, mas servirá para ilustrar o uso do VHDL.

```vhdl
-- Definição da entidade do processador
entity ProcessadorSimples is
    port (
        clk: in std_logic;            -- Entrada do sinal de clock
        reset: in std_logic;          -- Entrada do sinal de reset
        entrada: in std_logic_vector(7 downto 0);  -- Entrada dos dados
        saida: out std_logic_vector(7 downto 0)   -- Saída dos dados
    );
end ProcessadorSimples;

architecture Behavioral of ProcessadorSimples is
    -- Declaração dos componentes internos do processador
    component Registrador is
        port (
            clk: in std_logic;        -- Entrada do sinal de clock
            reset: in std_logic;      -- Entrada do sinal de reset
            entrada: in std_logic_vector(7 downto 0);  -- Entrada dos dados
            saida: out std_logic_vector(7 downto 0)   -- Saída dos dados
        );
    end component;
    
    -- Declaração dos sinais internos do processador
    signal regSaida1: std_logic_vector(7 downto 0);  -- Sinal de saída do primeiro registrador
    signal regSaida2: std_logic_vector(7 downto 0);  -- Sinal de saída do segundo registrador
    
begin
    -- Instanciação dos componentes internos do processador
    registro1: Registrador port map (
        clk => clk,
        reset => reset,
        entrada => entrada,
        saida => regSaida1
    );
    
    registro2: Registrador port map (
        clk => clk,
        reset => reset,
        entrada => regSaida1,
        saida => regSaida2
    );
    
    -- Saída do resultado final
    saida <= regSaida2;

end Behavioral;
```

Neste exemplo, estamos criando um processador simples que consiste em dois registradores sequenciais. O código inicia pela declaração da entidade `ProcessadorSimples`, que possui sinais de entrada para o sinal de clock (`clk`), o sinal de reset (`reset`), e os dados de entrada (`entrada`), bem como um sinal de saída para os dados de saída (`saida`).

Em seguida, temos a arquitetura `Behavioral`, que contém a declaração de dois componentes internos do processador do tipo `Registrador`. Estes registradores possuem os mesmos sinais de entrada e saída da entidade principal.

Dentro da arquitetura, temos as instanciações dos dois registradores, `registro1` e `registro2`, que recebem os sinais de clock, reset e dados de entrada corretamente através do `port map`, e atribuem as saídas aos sinais internos `regSaida1` e `regSaida2`, respectivamente.

Por fim, a saída do resultado final é determinada pela atribuição do sinal `regSaida2` ao sinal de saída `saida`.

Este é apenas um exemplo simplificado para ilustrar o uso do VHDL na criação de componentes de hardware. Normalmente, um processador completo possui mais componentes e funcionalidades, como unidade de controle, memoria, etc.