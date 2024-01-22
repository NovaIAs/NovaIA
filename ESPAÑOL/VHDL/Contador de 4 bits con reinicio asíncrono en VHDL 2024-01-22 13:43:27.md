```vhdl
-- Código VHDL para un contador de 4 bits con un contador de reinicio asíncrono

-- Definición de la arquitectura
architecture counter_4bit_with_async_reset of counter_4bit is

    -- Definición de las señales
    signal count : std_logic_vector(3 downto 0);

    -- Definición del proceso síncrono
    process(clk, reset)
    begin
        -- Si se resetea, se pone a cero el contador
        if reset = '1' then
            count <= "0000";
        -- Si no se resetea, se incrementa el contador
        else
            count <= count + 1;
        end if;
    end process;

    -- Declaración de la salida
    output count_out : std_logic_vector(3 downto 0) <= count;

end architecture counter_4bit_with_async_reset;

-- Código de prueba
entity counter_4bit_with_async_reset_tb is
end entity counter_4bit_with_async_reset_tb;

architecture behavioral of counter_4bit_with_async_reset_tb is

    -- Componentes
    component counter_4bit_with_async_reset is
        generic(
            WIDTH : positive := 4
        );
        port(
            clk : in std_logic;
            reset : in std_logic;
            count_out : out std_logic_vector(WIDTH-1 downto 0)
        );
    end component;

    -- Señales
    signal clk : std_logic := '0';
    signal reset : std_logic := '0';
    signal count_out : std_logic_vector(3 downto 0);

    -- Instancia del componente
    UUT : counter_4bit_with_async_reset
        generic map(
            WIDTH => 4
        )
        port map(
            clk => clk,
            reset => reset,
            count_out => count_out
        );

    -- Proceso de prueba
    process
    begin
        -- Señal de reloj
        clk <= not clk after 50 ns;

        -- Señal de reinicio
        reset <= '1' after 100 ns;
        reset <= '0' after 200 ns;

        -- Espera hasta el final de la simulación
        wait until now() >= 1000 ns;

        -- Imprime el valor del contador
        report "El valor del contador es: " & std_logic_vector'image(count_out);

        -- Finaliza la simulación
        finish;
    end process;

end architecture behavioral;
```

Este código crea un contador de 4 bits con un contador de reinicio asíncrono. El contador se incrementa cada vez que el reloj se activa y se reinicia cada vez que la señal de reinicio se activa. El código de prueba crea una señal de reloj y una señal de reinicio, y luego instancia el contador y lo conecta a las señales de reloj y reinicio. El código de prueba también imprime el valor del contador cada vez que el reloj se activa.