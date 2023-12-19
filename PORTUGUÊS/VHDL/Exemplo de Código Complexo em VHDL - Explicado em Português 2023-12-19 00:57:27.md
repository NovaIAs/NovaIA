Claro! Aqui está um exemplo de código complexo em VHDL que utiliza muitas funcionalidades diferentes e está explicado em português:

```vhdl
-- Definição da entidade
entity Complexo is
    Port (
        clk : in std_logic;
        reset : in std_logic;
        entrada : in std_logic_vector(7 downto 0);
        saida : out std_logic_vector(7 downto 0)
    );
end Complexo;

-- Arquitetura do componente
architecture Behavioral of Complexo is
    signal contador : integer := 0;
    signal acumulador : unsigned(15 downto 0) := (others => '0');
    signal resultado : unsigned(7 downto 0);

begin

    -- Processo para a lógica interna
    process(clk, reset)
    begin
        if reset = '1' then
            contador <= 0;
            acumulador <= (others => '0');
            resultado <= (others => '0');
        elsif rising_edge(clk) then
            if contador < 5 then
                acumulador <= acumulador + unsigned(entrada);
                contador <= contador + 1;
            elsif contador = 5 then
                resultado <= std_logic_vector(acumulador(15 downto 8));
                contador <= contador + 1;
            else
                resultado <= std_logic_vector(acumulador(7 downto 0));
                contador <= 0;
            end if;
        end if;
    end process;

    -- Atribuição da saída
    saida <= std_logic_vector(resultado);

end Behavioral;
```

Neste exemplo, criamos um componente VHDL chamado "Complexo" que possui uma entrada de clock (`clk`), um sinal de reset (`reset`), uma entrada de dados (`entrada`) e uma saída de dados (`saida`), todos de 8 bits.

Na arquitetura `Behavioral`, definimos dois sinais internos: `contador` e `acumulador`. O sinal `contador` é utilizado para controlar a lógica interna do componente, enquanto `acumulador` é utilizado para acumular os valores de entrada.

Dentro do processo sensível à borda de subida do sinal de clock (`clk`) e ao sinal de reset (`reset`), implementamos a lógica do componente. Quando ocorre um reset (`reset = '1'`), o contador e o acumulador são resetados para 0. Caso contrário, quando ocorre uma borda de subida do sinal de clock, verificamos o valor do contador.

Se o contador for menor que 5, incrementamos o acumulador com o valor de entrada (`acumulador <= acumulador + unsigned(entrada)`) e incrementamos o contador em 1. Quando o contador atinge o valor 5, armazenamos a parte alta do acumulador no sinal `resultado` (`resultado <= std_logic_vector(acumulador(15 downto 8))`). Caso contrário, armazenamos a parte baixa do acumulador no sinal `resultado` (`resultado <= std_logic_vector(acumulador(7 downto 0))`), e o contador é resetado para 0.

Por fim, atribuímos o valor do sinal `resultado` à saída do componente (`saida <= std_logic_vector(resultado)`).

Espero que esse exemplo atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.