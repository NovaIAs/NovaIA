```vhdl
-- Librerías del lenguaje
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

-- Definición de un nuevo tipo de dato
type estado_actual is (S0, S1, S2, S3);

-- Definición de una entidad
entity maquina_de_estados is
    port(
        clk : in std_logic;
        rst : in std_logic;
        entrada : in std_logic;
        salida : out std_logic
    );
end maquina_de_estados;

-- Definición de la arquitectura de la entidad
architecture comportamiento of maquina_de_estados is
    -- Definición de las señales internas
    signal estado_actual : estado_actual := S0;
    signal estado_siguiente : estado_actual;

begin
    -- Proceso que implementa la máquina de estados
    process(clk, rst)
    begin
        if rst = '1' then
            estado_actual <= S0;
        elsif clk'event and clk = '1' then
            case estado_actual is
                when S0 =>
                    if entrada = '1' then
                        estado_siguiente <= S1;
                    else
                        estado_siguiente <= S0;
                    end if;
                when S1 =>
                    if entrada = '0' then
                        estado_siguiente <= S2;
                    else
                        estado_siguiente <= S1;
                    end if;
                when S2 =>
                    if entrada = '1' then
                        estado_siguiente <= S3;
                    else
                        estado_siguiente <= S2;
                    end if;
                when S3 =>
                    if entrada = '0' then
                        estado_siguiente <= S0;
                    else
                        estado_siguiente <= S3;
                    end if;
            end case;
            estado_actual <= estado_siguiente;
        end if;
    end process;

    -- Proceso que implementa la salida de la máquina de estados
    process(estado_actual)
    begin
        case estado_actual is
            when S0 =>
                salida <= '0';
            when S1 =>
                salida <= '1';
            when S2 =>
                salida <= '0';
            when S3 =>
                salida <= '1';
        end case;
    end process;
end comportamiento;
```

**Explicación del código:**

* La primera parte del código define las bibliotecas que se utilizarán en el código. En este caso, se utilizan las bibliotecas `ieee.std_logic_1164` e `ieee.std_logic_unsigned`.
* La segunda parte del código define un nuevo tipo de dato llamado `estado_actual`. Este tipo de dato puede tomar los valores `S0`, `S1`, `S2` y `S3`.
* La tercera parte del código define una entidad llamada `maquina_de_estados`. Esta entidad tiene cuatro puertos: `clk`, `rst`, `entrada` y `salida`. El puerto `clk` es la señal de reloj, el puerto `rst` es la señal de reset, el puerto `entrada` es la señal de entrada de la máquina de estados y el puerto `salida` es la señal de salida de la máquina de estados.
* La cuarta parte del código define la arquitectura de la entidad `maquina_de_estados`. Esta arquitectura se implementa mediante dos procesos. El primer proceso implementa la máquina de estados y el segundo proceso implementa la salida de la máquina de estados.
* El primer proceso se ejecuta cada vez que el valor de la señal `clk` cambia a '1'. El primer proceso guarda el estado actual de la máquina de estados en la señal `estado_actual`. El segundo proceso guarda el estado siguiente de la máquina de estados en la señal `estado_siguiente`. El estado siguiente de la máquina de estados se calcula en función del estado actual de la máquina de estados y de la señal de entrada.
* El segundo proceso se ejecuta cada vez que el valor de la señal `estado_actual` cambia. El segundo proceso guarda el valor de la señal `estado_actual` en la señal `salida`. El valor de la señal `salida` es la señal de salida de la máquina de estados.