```vhdl
-- Código VHDL para implementar un controlador de semáforos con protocolo de comunicación RS-232

-- Definición de las constantes y tipos
type Estado_semaforo is (Rojo, Amarillo, Verde);

-- Definición de las señales
signal color_semaforo : Estado_semaforo;
signal enable_semaforo : boolean;
signal rs232_data : std_logic_vector(7 downto 0);

-- Definición de los componentes
component rs232_transceiver is
    port(
        tx_data : in std_logic_vector(7 downto 0);
        tx_enable : in boolean;
        rx_data : out std_logic_vector(7 downto 0);
        rx_ready : out boolean
    );
end component;

component semaforo is
    port(
        color : in Estado_semaforo;
        enable : in boolean;
        clk : in std_logic
    );
end component;

-- Instanciación de los componentes
U_RS232_Transceiver : rs232_transceiver
    port map(
        tx_data => rs232_data,
        tx_enable => enable_semaforo,
        rx_data => rs232_data,
        rx_ready => enable_semaforo
    );

U_Semaforo : semaforo
    port map(
        color => color_semaforo,
        enable => enable_semaforo,
        clk => clk
    );

-- Código VHDL para el proceso principal
process
begin
    -- Esperar hasta que haya datos disponibles en el puerto RS-232
    wait until enable_semaforo = '1';

    -- Leer los datos del puerto RS-232
    color_semaforo <= std_logic_vector(to_unsigned(rs232_data, Estado_semaforo'range));

    -- Activar el semáforo
    enable_semaforo <= '1';

    -- Esperar hasta que el semáforo haya cambiado de color
    wait until color_semaforo /= rs232_data;

    -- Desactivar el semáforo
    enable_semaforo <= '0';
end process;

-- Código VHDL para el proceso de reloj
process
begin
    while true loop
        clk <= '1';
        wait for 100 ns;
        clk <= '0';
        wait for 100 ns;
    end loop;
end process;
```

Este código VHDL implementa un controlador de semáforos con protocolo de comunicación RS-232. El controlador de semáforos está conectado a un puerto RS-232, que se utiliza para comunicarse con un ordenador. El ordenador puede enviar comandos al controlador de semáforos para cambiar el color del semáforo.

El controlador de semáforos está implementado utilizando dos componentes: un transceptor RS-232 y un semáforo. El transceptor RS-232 se encarga de la comunicación con el ordenador, mientras que el semáforo se encarga de cambiar el color del semáforo.

El código VHDL para el proceso principal se encarga de esperar hasta que haya datos disponibles en el puerto RS-232. Una vez que hay datos disponibles, el código lee los datos del puerto RS-232 y cambia el color del semáforo. El código también espera hasta que el semáforo haya cambiado de color antes de desactivarlo.

El código VHDL para el proceso de reloj se encarga de generar la señal de reloj que se utiliza para controlar el semáforo. La señal de reloj se genera utilizando un bucle while infinito. En cada iteración del bucle, la señal de reloj se establece en '1' durante 100 ns y luego se establece en '0' durante 100 ns.