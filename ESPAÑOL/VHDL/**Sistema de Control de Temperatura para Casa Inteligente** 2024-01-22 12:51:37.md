```vhdl
-- Código VHDL para un sistema de control de temperatura de una casa inteligente

-- Biblioteca Estándar
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Entidad del sistema de control de temperatura
entity Sistema_Control_Temperatura is
    Port (
        clk : in STD_LOGIC;
        rst : in STD_LOGIC;
        sensor_temperatura : in STD_LOGIC_VECTOR(7 downto 0);
        ventilador : out STD_LOGIC;
        calefactor : out STD_LOGIC;
        aire_acondicionado : out STD_LOGIC
    );
end Sistema_Control_Temperatura;

-- Arquitectura del sistema de control de temperatura
architecture Comportamiento of Sistema_Control_Temperatura is

    -- Estados del sistema
    type Estado is (Espera, Calefaccion, Ventilacion, Aire_Acondicionado);
    signal estado_actual : Estado := Espera;

    -- Registros para almacenar la temperatura y el estado deseado
    signal temperatura_actual : STD_LOGIC_VECTOR(7 downto 0);
    signal temperatura_deseada : STD_LOGIC_VECTOR(7 downto 0);

    -- Proceso para leer la temperatura del sensor
    process (clk)
    begin
        if (clk'event and clk = '1') then
            temperatura_actual <= sensor_temperatura;
        end if;
    end process;

    -- Proceso para controlar el estado del sistema
    process (clk, rst)
    begin
        if (rst = '1') then
            estado_actual <= Espera;
        elsif (clk'event and clk = '1') then
            case estado_actual is
                when Espera =>
                    if (temperatura_actual > temperatura_deseada) then
                        estado_actual <= Ventilacion;
                    elsif (temperatura_actual < temperatura_deseada) then
                        estado_actual <= Calefaccion;
                    end if;
                when Calefaccion =>
                    if (temperatura_actual >= temperatura_deseada) then
                        estado_actual <= Espera;
                    end if;
                when Ventilacion =>
                    if (temperatura_actual <= temperatura_deseada) then
                        estado_actual <= Espera;
                    end if;
                when Aire_Acondicionado =>
                    if (temperatura_actual <= temperatura_deseada) then
                        estado_actual <= Espera;
                    end if;
            end case;
        end if;
    end process;

    -- Proceso para controlar el ventilador
    process (clk, rst)
    begin
        if (rst = '1') then
            ventilador <= '0';
        elsif (clk'event and clk = '1') then
            case estado_actual is
                when Ventilacion =>
                    ventilador <= '1';
                when Calefaccion, Aire_Acondicionado =>
                    ventilador <= '0';
                when Espera =>
                    ventilador <= '0';
            end case;
        end if;
    end process;

    -- Proceso para controlar el calefactor
    process (clk, rst)
    begin
        if (rst = '1') then
            calefactor <= '0';
        elsif (clk'event and clk = '1') then
            case estado_actual is
                when Calefaccion =>
                    calefactor <= '1';
                when Ventilacion, Aire_Acondicionado, Espera =>
                    calefactor <= '0';
            end case;
        end if;
    end process;

    -- Proceso para controlar el aire acondicionado
    process (clk, rst)
    begin
        if (rst = '1') then
            aire_acondicionado <= '0';
        elsif (clk'event and clk = '1') then
            case estado_actual is
                when Aire_Acondicionado =>
                    aire_acondicionado <= '1';
                when Ventilacion, Calefaccion, Espera =>
                    aire_acondicionado <= '0';
            end case;
        end if;
    end process;

end Comportamiento;
```

**Explicación del código:**

* El código VHDL anterior implementa un sistema de control de temperatura de una casa inteligente.
* El sistema cuenta con un sensor de temperatura, un ventilador, un calefactor y un aire acondicionado.
* El sistema funciona de la siguiente manera:
    * El sensor de temperatura mide la temperatura actual de la casa.
    * El sistema compara la temperatura actual con la temperatura deseada.
    * Si la temperatura actual es superior a la deseada, el sistema enciende el ventilador.
    * Si la temperatura actual es inferior a la deseada, el sistema enciende el calefactor.
    * Si la temperatura actual es igual a la deseada, el sistema apaga el ventilador y el calefactor.
* El sistema también cuenta con un modo de aire acondicionado, que se activa cuando la temperatura actual es muy alta.
* El modo de aire acondicionado enciende el aire acondicionado y apaga el ventilador y el calefactor.