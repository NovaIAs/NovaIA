```vhdl
-- ARCHIVO: contador_asc_desc.vhd
-- DESCRIPCIÓN: Módulo VHDL que implementa un contador ascendente y descendente con un rango de valores configurable.

-- Definición de la entidad del contador.
entity contador_asc_desc is
    port (
        clk : in std_logic;            -- Señal de reloj.
        reset : in std_logic;         -- Señal de reset.
        dir : in std_logic;            -- Dirección del contador (0: ascendente, 1: descendente).
        load : in std_logic;           -- Señal de carga del contador.
        data : in unsigned(7 downto 0); -- Valor de carga del contador.
        q : out unsigned(7 downto 0)  -- Salida del contador.
    );
end entity;

-- Definición de la arquitectura del contador.
architecture behavioral of contador_asc_desc is

    -- Señales internas.
    signal count : unsigned(7 downto 0); -- Contador interno.
    signal next_count : unsigned(7 downto 0); -- Siguiente valor del contador.

begin

    -- Proceso que implementa el contador.
    process(clk, reset, dir, load, data)
    begin
        -- Si la señal de reset está activa, se reinicia el contador.
        if reset = '1' then
            count <= (others => '0');
        -- Si la señal de carga está activa, se carga el contador con el valor de la señal data.
        elsif load = '1' then
            count <= data;
        -- Si la dirección del contador es ascendente, se incrementa el contador.
        elsif dir = '0' then
            next_count <= count + 1;
        -- Si la dirección del contador es descendente, se decrementa el contador.
        else
            next_count <= count - 1;
        end if;

        -- Se actualiza el contador con el siguiente valor.
        count <= next_count;
    end process;

    -- Asignación de la salida del contador.
    q <= count;

end architecture;
```

**Explicación del código:**

- La **entidad** `contador_asc_desc` define las entradas y salidas del contador:
    - `clk` es la señal de reloj del contador.
    - `reset` es la señal de reset del contador.
    - `dir` es la señal que determina la dirección del contador (0: ascendente, 1: descendente).
    - `load` es la señal que indica que el contador debe cargarse con el valor de la señal `data`.
    - `data` es la señal que contiene el valor de carga del contador.
    - `q` es la señal que contiene el valor actual del contador.


- La **arquitectura** `behavioral` define la implementación del contador:
    - Se declara una señal interna `count` para almacenar el valor actual del contador.
    - Se declara una señal interna `next_count` para almacenar el siguiente valor del contador.
    - Se implementa un proceso que se ejecuta cada vez que cambia el valor de las señales `clk`, `reset`, `dir`, `load` o `data`.
    - En el proceso, se comprueba si la señal de reset está activa. Si es así, se reinicia el contador.
    - Si la señal de carga está activa, se carga el contador con el valor de la señal `data`.
    - Si la dirección del contador es ascendente, se incrementa el contador.
    - Si la dirección del contador es descendente, se decrementa el contador.
    - Se actualiza el valor del contador con el siguiente valor.
    - Se asigna el valor del contador a la salida `q`.