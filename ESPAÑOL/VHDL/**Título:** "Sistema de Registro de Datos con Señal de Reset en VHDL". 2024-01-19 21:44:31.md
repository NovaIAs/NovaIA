```vhdl
-- Declaración de la entidad superior
entidad sistema is
  -- Declaración de las interfaces
  puerto(
    clk : in std_logic;     -- Reloj de sistema
    rst : in std_logic;     -- Señal de reset
    data_in : in std_logic_vector(7 downto 0); -- Datos de entrada
    data_out : out std_logic_vector(7 downto 0)  -- Datos de salida
  );
end entidad sistema;

-- Declaración de la arquitectura
arquitectura comportamiento of sistema is
  -- Declaración de las señales internas
  signal reg_data : std_logic_vector(7 downto 0); -- Registro de datos
  signal next_data : std_logic_vector(7 downto 0); -- Datos de salida del registro

  -- Proceso para el registro de datos
  proceso(clk)
  begin
    if (rst = '1') then
      -- Si hay un reset, se cargan los datos iniciales
      reg_data <= (others => '0');
    elsif (clk'event and clk = '1') then
      -- Si hay un flanco de subida en el reloj, se cargan los datos de entrada
      reg_data <= data_in;
    end if;
  end proceso;

  -- Proceso para los datos de salida
  proceso(clk)
  begin
    if (clk'event and clk = '1') then
      -- Si hay un flanco de subida en el reloj, se envían los datos de salida
      data_out <= reg_data;
    end if;
  end proceso;

end arquitectura comportamiento;
```

Explicación:

* La entidad **sistema** declara las interfaces del módulo, incluyendo las señales de entrada, salida y reloj.
* La arquitectura **comportamiento** define el comportamiento del módulo mediante procesos.
* El proceso **para el registro de datos** carga los datos iniciales si hay un reset, o los datos de entrada si hay un flanco de subida en el reloj.
* El proceso **para los datos de salida** envía los datos de salida si hay un flanco de subida en el reloj.