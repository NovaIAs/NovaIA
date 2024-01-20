```vhdl
-- Código VHDL "Procesamiento Complejo de Señales"

-- Librerías
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Definición de constantes
constant CLK_PERIOD : time := 10 ns; -- Período del reloj
constant NUM_SAMPLES : integer := 1024; -- Número de muestras

-- Definición de tipos
type sample_t is signed(15 downto 0); -- Tipo de dato para las muestras

-- Definición de señales
signal clk : std_logic; -- Reloj
signal reset : std_logic; -- Señal de reset
signal signed [15:0] signed_data_in : std_logic_vector(15 downto 0); -- Datos de entrada con signo
signal sample_data_in : sample_t; -- Datos de entrada sin signo
signal signed [15:0] signed_data_out : std_logic_vector(15 downto 0); -- Datos de salida con signo
signal sample_data_out : sample_t; -- Datos de salida sin signo

-- Definición de componentes
component fifo is
    generic (
        DATA_WIDTH : integer; -- Ancho de datos
        DEPTH : integer -- Profundidad
    );
    port (
        clk : in std_logic; -- Reloj
        reset : in std_logic; -- Señal de reset
        din : in std_logic_vector(DATA_WIDTH-1 downto 0); -- Datos de entrada
        dout : out std_logic_vector(DATA_WIDTH-1 downto 0); -- Datos de salida
        full : out std_logic; -- Indicador de FIFO lleno
        empty : out std_logic -- Indicador de FIFO vacío
    );
end component;

component fir_filter is
    generic (
        COEFFICIENTS : array of signed(15 downto 0); -- Coeficientes del filtro
        NUM_TAPS : integer -- Número de taps del filtro
    );
    port (
        clk : in std_logic; -- Reloj
        reset : in std_logic; -- Señal de reset
        din : in std_logic_vector(15 downto 0); -- Datos de entrada
        dout : out std_logic_vector(15 downto 0) -- Datos de salida
    );
end component;

-- Instancias de componentes
FIFO_IN : fifo
    generic map (
        DATA_WIDTH => 16, -- Ancho de datos
        DEPTH => NUM_SAMPLES + 1 -- Profundidad
    )
    port map (
        clk => clk, -- Reloj
        reset => reset, -- Señal de reset
        din => signed_data_in, -- Datos de entrada
        dout => sample_data_in, -- Datos de salida
        full => full_fifo_in, -- Indicador de FIFO lleno
        empty => empty_fifo_in -- Indicador de FIFO vacío
    );

FIR_FILTER : fir_filter
    generic map (
        COEFFICIENTS => {-1, 0, 1}, -- Coeficientes del filtro
        NUM_TAPS => 3 -- Número de taps del filtro
    )
    port map (
        clk => clk, -- Reloj
        reset => reset, -- Señal de reset
        din => sample_data_in, -- Datos de entrada
        dout => sample_data_out -- Datos de salida
    );

FIFO_OUT : fifo
    generic map (
        DATA_WIDTH => 16, -- Ancho de datos
        DEPTH => NUM_SAMPLES + 1 -- Profundidad
    )
    port map (
        clk => clk, -- Reloj
        reset => reset, -- Señal de reset
        din => sample_data_out, -- Datos de entrada
        dout => signed_data_out, -- Datos de salida
        full => full_fifo_out, -- Indicador de FIFO lleno
        empty => empty_fifo_out -- Indicador de FIFO vacío
    );

-- Proceso principal
process
begin
    -- Esperar hasta que el reloj esté activo
    wait until rising_edge(clk);
    
    -- Restablecer el sistema
    reset <= '1';
    wait for CLK_PERIOD;
    reset <= '0';
    
    -- Procesamiento de las muestras
    for i in 0 to NUM_SAMPLES-1 loop
        -- Esperar hasta que haya datos disponibles en el FIFO de entrada
        wait until not empty_fifo_in;
        
        -- Leer los datos de entrada
        signed_data_in <= dout_fifo_in;
        
        -- Aplicar el filtro FIR
        sample_data_out <= dout_fir_filter;
        
        -- Esperar hasta que haya espacio disponible en el FIFO de salida
        wait until not full_fifo_out;
        
        -- Escribir los datos de salida en el FIFO de salida
        signed_data_out <= signed(15 downto 0, sample_data_out);
    end loop;
end process;

-- Fin del código
```

**Explicación del código:**

Este código VHDL implementa un sistema de procesamiento de señales complejo. El sistema consta de tres componentes: un FIFO de entrada, un filtro FIR y un FIFO de salida. El FIFO de entrada almacena los datos de entrada, el filtro FIR procesa los datos de entrada y el FIFO de salida almacena los datos de salida.

El código está dividido en varias secciones:

* **Librerías:** Esta sección incluye las librerías que se utilizarán en el código.
* **Definición de constantes:** Esta sección define las constantes que se utilizarán en el código.
* **Definición de tipos:** Esta sección define los tipos de datos que se utilizarán en el código.
* **Definición de señales:** Esta sección define las señales que se utilizarán en el código.
* **Definición de componentes:** Esta sección define los componentes que se utilizarán en el código.
* **Instancias de componentes:** Esta sección crea instancias de los componentes que se utilizarán en el código.
* **Proceso principal:** Esta sección es el proceso principal del código. Esta sección se ejecutará continuamente hasta que se detenga el código.
* **Fin del código:** Esta sección marca el final del código.

El código se ejecuta de la siguiente manera:

1. El proceso principal espera hasta que el reloj esté activo.
2. El proceso principal restablece el sistema.
3. El proceso principal procesa las muestras.
4. El proceso principal espera hasta que el sistema se detenga.

El sistema procesa las muestras de la siguiente manera:

1. El FIFO de entrada almacena los datos de entrada.
2. El filtro FIR procesa los datos de entrada.
3. El FIFO de salida almacena los datos de salida.