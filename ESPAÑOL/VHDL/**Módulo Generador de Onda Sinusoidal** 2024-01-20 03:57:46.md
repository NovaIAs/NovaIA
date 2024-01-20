```vhdl
-- **Módulo de Onda Sinusoidal**

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity OndaSinusoidal es
    generic (
        Frecuencia      : natural;    -- Frecuencia de la onda sinusoidal
        Resolucion      : natural;    -- Resolución de la onda sinusoidal
        Amplitud        : real        -- Amplitud de la onda sinusoidal
    );
    port (
        Clock          : in std_logic;
        Salida         : out std_logic
    );
end entity OndaSinusoidal;

architecture Behavioral of OndaSinusoidal is

    signal Fase : integer range 0 to 359; -- Fase actual de la onda sinusoidal

begin

    -- Actualizar la fase de la onda sinusoidal
    Fase <= Fase + Frecuencia;

    -- Generar la salida de la onda sinusoidal
    Salida <= std_logic'value(sin(Fase * 2 * pi / 360) * Amplitud);

end architecture Behavioral;
```

**Explicación del Código:**

El código VHDL anterior define un módulo llamado `OndaSinusoidal` que genera una onda sinusoidal. El módulo tiene tres parámetros genéricos:

* **Frecuencia:** La frecuencia de la onda sinusoidal en hercios.
* **Resolucion:** La resolución de la onda sinusoidal en bits.
* **Amplitud:** La amplitud de la onda sinusoidal en voltios.

El módulo también tiene dos puertos:

* **Clock:** Un puerto de entrada que proporciona la señal de reloj para el módulo.
* **Salida:** Un puerto de salida que proporciona la señal de salida del módulo.

La arquitectura `Behavioral` del módulo define la implementación del módulo. La arquitectura contiene una señal llamada `Fase` que almacena la fase actual de la onda sinusoidal. La señal `Fase` se actualiza en cada ciclo de reloj utilizando el operador `+`.

La salida del módulo se genera utilizando la función `sin()` de la biblioteca `numeric_std`. La función `sin()` toma un argumento en radianes y devuelve el valor del seno del argumento. El argumento de la función `sin()` es la fase actual de la onda sinusoidal multiplicada por 2π y dividida por 360. Esto convierte la fase de la onda sinusoidal de grados a radianes.

El valor devuelto por la función `sin()` se multiplica por la amplitud de la onda sinusoidal para obtener la amplitud de la salida del módulo. La salida del módulo se asigna al puerto `Salida` utilizando el operador `<=`.

El código VHDL anterior se puede utilizar para generar una onda sinusoidal con cualquier frecuencia, resolución y amplitud. La onda sinusoidal se puede utilizar para una variedad de propósitos, como probar circuitos electrónicos, generar música y controlar sistemas de iluminación.