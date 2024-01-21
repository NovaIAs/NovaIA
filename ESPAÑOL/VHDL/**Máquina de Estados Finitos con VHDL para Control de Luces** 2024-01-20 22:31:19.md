```vhdl
-- Módulo principal del proyecto.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity proyecto_principal is
    port (
        clk : in STD_LOGIC;
        rst : in STD_LOGIC;

        -- Entradas del usuario.
        entrada_usuario : in STD_LOGIC_VECTOR (7 downto 0);

        -- Salidas del sistema.
        leds : out STD_LOGIC_VECTOR (7 downto 0)
    );
end entity proyecto_principal;

architecture arquitectura_principal of proyecto_principal is
    -- Definición de señales internas.
    signal contador : STD_LOGIC_VECTOR (3 downto 0) := (others => '0');
    signal estado_actual : STD_LOGIC_VECTOR (2 downto 0) := (others => '0');

begin
    -- Máquina de estados finitos para controlar el comportamiento del sistema.
    process (clk, rst)
    begin
        if (rst = '1') then
            estado_actual <= (others => '0'); -- Reiniciar el estado actual.
        elsif (rising_edge(clk)) then
            case estado_actual is
                when "000" =>
                    -- Estado inicial.
                    if (entrada_usuario(7) = '1') then
                        estado_actual <= "001"; -- Avanzar al siguiente estado.
                    end if;
                when "001" =>
                    -- Estado de espera.
                    contador <= contador + 1;
                    if (contador = "1111") then
                        estado_actual <= "010"; -- Avanzar al siguiente estado.
                    end if;
                when "010" =>
                    -- Estado de salida.
                    leds <= entrada_usuario;
                    estado_actual <= "000"; -- Volver al estado inicial.
            end case;
        end if;
    end process;

    -- Lógica combinatoria para generar las salidas del sistema.
    leds <= "11111111"; -- Inicializar las salidas a '1'.

    case estado_actual is
        when "010" =>
            -- Estado de salida.
            leds <= entrada_usuario;
    end case;
end architecture arquitectura_principal;
```

Explicación del código:

* El módulo principal `proyecto_principal` define las entradas y salidas del sistema.
* La arquitectura `arquitectura_principal` define la implementación del sistema.
* El proceso `maquina_de_estados_finitos` implementa la máquina de estados finitos que controla el comportamiento del sistema.
* La lógica combinatoria `logica_combinatoria` genera las salidas del sistema en función del estado actual.

El sistema funciona de la siguiente manera:

1. En el estado inicial "000", el sistema espera a que el usuario presione el botón de entrada.
2. Cuando el usuario presiona el botón de entrada, el sistema avanza al estado "001".
3. En el estado "001", el sistema cuenta hasta 15 (4 bits).
4. Cuando el contador llega a 15, el sistema avanza al estado "010".
5. En el estado "010", el sistema enciende los LEDs de salida según el valor de la entrada del usuario.
6. Después de un tiempo, el sistema vuelve al estado "000" y espera a que el usuario presione el botón de entrada nuevamente.

El sistema puede ser utilizado para controlar cualquier tipo de dispositivo que pueda ser controlado mediante LEDs. Por ejemplo, el sistema podría ser utilizado para controlar las luces de una habitación, el volumen de un equipo de música, o la temperatura de un termostato.