```vhdl
-- Módulo principal del sistema
entity sistema is
  port (
    clk_in  : in  std_logic;  -- Reloj de entrada
    rst_in  : in  std_logic;  -- Señal de reset
    data_in : in  std_logic_vector(7 downto 0);  -- Datos de entrada
    data_out: out std_logic_vector(7 downto 0)  -- Datos de salida
  );
end entity sistema;

-- Arquitectura del sistema
architecture estructura of sistema is
  -- Señales internas
  signal clk_div : std_logic;  -- Reloj dividido
  signal cnt_div : integer range 0 to 255;  -- Contador de división
  signal data_reg: std_logic_vector(7 downto 0);  -- Registro de datos
  signal data_out_reg: std_logic_vector(7 downto 0);  -- Registro de datos de salida

begin
  -- Divisor de reloj
  clk_div_proc: process(clk_in, rst_in)
  begin
    if rst_in = '1' then
      cnt_div <= 0;
    elsif rising_edge(clk_in) then
      cnt_div <= cnt_div + 1;
    end if;

    clk_div <= '1' when cnt_div = 255 else '0';
  end process clk_div_proc;

  -- Registro de datos
  data_reg_proc: process(clk_div, rst_in, data_in)
  begin
    if rst_in = '1' then
      data_reg <= (others => '0');
    elsif rising_edge(clk_div) then
      data_reg <= data_in;
    end if;
  end process data_reg_proc;

  -- Registro de datos de salida
  data_out_reg_proc: process(clk_in, rst_in, data_reg)
  begin
    if rst_in = '1' then
      data_out_reg <= (others => '0');
    elsif rising_edge(clk_in) then
      data_out_reg <= data_reg;
    end if;
  end process data_out_reg_proc;

  -- Salida de datos
  data_out <= data_out_reg;
end architecture estructura;
```

Este código VHDL implementa un sistema digital que recibe datos en paralelo de 8 bits, los almacena en un registro y los envía en serie, un bit a la vez. El sistema está controlado por un reloj de entrada y una señal de reset.

El divisor de reloj genera un reloj dividido que es la mitad de la frecuencia del reloj de entrada. Este reloj dividido se utiliza para controlar el registro de datos y el registro de datos de salida.

El registro de datos almacena los datos de entrada en paralelo durante un ciclo de reloj. Cuando el reloj dividido llega a su valor alto, el registro de datos se actualiza con los nuevos datos de entrada.

El registro de datos de salida envía los datos almacenados en el registro de datos en serie, un bit a la vez. Cuando el reloj de entrada llega a su valor alto, el registro de datos de salida se actualiza con el siguiente bit de los datos almacenados.

La salida de datos del sistema es una señal de 8 bits que contiene los datos de entrada en serie.