```vhdl
-- Declaración del módulo principal
module modulo_principal is
  -- Declaración de las entradas y salidas del módulo
  port (
    clk_i    : in  std_logic; -- Reloj de entrada
    rst_i    : in  std_logic; -- Señal de reset
    data_i   : in  std_logic_vector(7 downto 0); -- Datos de entrada
    result_o : out std_logic_vector(7 downto 0) -- Resultados de la operación
  );

  -- Declaración de las variables internas del módulo
  signal data_reg : std_logic_vector(7 downto 0); -- Registro para almacenar los datos de entrada
  signal result_reg : std_logic_vector(7 downto 0); -- Registro para almacenar los resultados de la operación

  -- Proceso secuencial para almacenar los datos de entrada y calcular los resultados
  process (clk_i, rst_i)
  begin
    if rst_i = '1' then -- Si se activa la señal de reset
      data_reg <= (others => '0'); -- Se limpian los registros
      result_reg <= (others => '0');
    elsif clk_i'event and clk_i = '1' then -- Si se produce una transición positiva en el reloj
      data_reg <= data_i; -- Se almacenan los datos de entrada en el registro
      result_reg <= data_reg + data_i; -- Se calculan los resultados de la operación
    end if;
  end process;

  -- Asignación de los resultados de la operación a la salida del módulo
  result_o <= result_reg;
endmodule;
```

Este código VHDL implementa un módulo que almacena los datos de entrada en un registro y calcula los resultados de una operación aritmética (en este caso, la suma) entre los datos de entrada y los datos almacenados en el registro. El módulo tiene tres entradas: `clk_i`, `rst_i` y `data_i`, y una salida: `result_o`.

El proceso secuencial `process (clk_i, rst_i)` se ejecuta de forma continua y realiza las siguientes operaciones:

1. Si la señal `rst_i` es igual a '1', se limpian los registros `data_reg` y `result_reg` asignándoles un valor de '0' en todas sus posiciones.
2. Si se produce una transición positiva en el reloj (`clk_i'event and clk_i = '1'`), se almacenan los datos de entrada en el registro `data_reg` y se calculan los resultados de la operación aritmética entre los datos de entrada y los datos almacenados en el registro `data_reg`.

Los resultados de la operación aritmética se asignan a la salida del módulo mediante la asignación `result_o <= result_reg`.