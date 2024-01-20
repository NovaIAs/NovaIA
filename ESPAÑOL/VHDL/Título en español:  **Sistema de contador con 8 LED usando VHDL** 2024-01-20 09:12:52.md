```vhdl
-- Módulo principal del sistema

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity sistema is
  port (
    clk : in STD_LOGIC;
    reset : in STD_LOGIC;
    led : out STD_LOGIC_VECTOR (7 downto 0)
  );
end entity sistema;

architecture comportamental of sistema is
  -- Definición de señales internas

  signal contador : unsigned (7 downto 0);

begin

  -- Proceso principal

  process (clk)
  begin
  
    -- Restablecimiento del sistema

    if (reset = '1') then
      contador <= (others => '0');
    elsif (clk'event and clk = '1') then
      contador <= contador + 1;
    end if;

    -- Actualización de las salidas

    led <= std_logic_vector(contador);
  end process;

end architecture comportamental;
```

Este código VHDL implementa un sistema simple que cuenta de 0 a 255 y muestra el valor del contador en un conjunto de 8 LED. El sistema está controlado por un reloj (clk) y un reinicio (reset).

Cuando el sistema se reinicia, el contador se pone a 0. A continuación, el proceso principal se ejecuta continuamente, incrementando el contador en 1 cada ciclo de reloj. El valor del contador se muestra en los LED mediante la asignación de la señal `contador` a la salida `led`.

Este código es bastante sencillo, pero demuestra algunos conceptos básicos de VHDL, como la definición de señales, la creación de procesos y la asignación de señales a salidas.