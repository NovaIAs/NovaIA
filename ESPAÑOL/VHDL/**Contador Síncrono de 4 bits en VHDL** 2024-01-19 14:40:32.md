**Código VHDL en español**

```vhdl
-- Este código VHDL implementa un contador síncrono de 4 bits con reset asíncrono.

-- Declaración de la biblioteca IEEE.

library IEEE;
use IEEE.std_logic_1164.all;

-- Declaración de la entidad del contador.

entity contador is
    port (
        clk : in std_logic; -- Señal de reloj
        reset : in std_logic; -- Señal de reset asíncrono
        count : out std_logic_vector(3 downto 0) -- Salida del contador
    );
end entity contador;

-- Declaración de la arquitectura del contador.

architecture estructural of contador is
    -- Declaración del tipo de datos del contador.

    type estado_contador is (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15);

    -- Declaración del estado actual del contador.

    signal estado_actual : estado_contador;

    -- Declaración del siguiente estado del contador.

    signal estado_siguiente : estado_contador;

    -- Procedimiento para determinar el siguiente estado del contador.

    procedure siguiente_estado(estado_actual : in estado_contador; reset : in std_logic; clk : in std_logic; estado_siguiente : out estado_contador) is
    begin
        case estado_actual is
            when s0 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s1;
                else
                    estado_siguiente <= s0;
                end if;
            when s1 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s2;
                else
                    estado_siguiente <= s1;
                end if;
            when s2 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s3;
                else
                    estado_siguiente <= s2;
                end if;
            when s3 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s4;
                else
                    estado_siguiente <= s3;
                end if;
            when s4 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s5;
                else
                    estado_siguiente <= s4;
                end if;
            when s5 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s6;
                else
                    estado_siguiente <= s5;
                end if;
            when s6 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s7;
                else
                    estado_siguiente <= s6;
                end if;
            when s7 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s8;
                else
                    estado_siguiente <= s7;
                end if;
            when s8 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s9;
                else
                    estado_siguiente <= s8;
                end if;
            when s9 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s10;
                else
                    estado_siguiente <= s9;
                end if;
            when s10 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s11;
                else
                    estado_siguiente <= s10;
                end if;
            when s11 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s12;
                else
                    estado_siguiente <= s11;
                end if;
            when s12 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s13;
                else
                    estado_siguiente <= s12;
                end if;
            when s13 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s14;
                else
                    estado_siguiente <= s13;
                end if;
            when s14 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then
                    estado_siguiente <= s15;
                else
                    estado_siguiente <= s14;
                end if;
            when s15 =>
                if reset then
                    estado_siguiente <= s0;
                elsif clk'event and clk = '1' then