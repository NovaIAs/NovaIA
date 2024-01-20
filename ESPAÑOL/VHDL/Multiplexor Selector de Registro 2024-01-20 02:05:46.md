```vhdl
-- Definición de constantes y tipos personalizados
constant NUM_BITS : integer := 8;
type Registro is array (0 to NUM_BITS-1) of std_logic;
type VectorRegistros is array (0 to NUM_BITS-1) of Registro;

-- Definición de señales
signal clk, rst : std_logic;
signal in_data : Registro;
signal out_data : Registro;
signal reg_select : integer range 0 to NUM_BITS-1;

-- Registro de control
process(clk, rst)
begin
    if rst = '1' then
        reg_select <= 0;
    elsif rising_edge(clk) then
        reg_select <= reg_select + 1;
        if reg_select = NUM_BITS then
            reg_select <= 0;
        end if;
    end if;
end process;

-- Módulo de Registro
component RegistroGenerico
    generic (
        NUM_BITS : in integer
    );
    port (
        clk, rst : in std_logic;
        in_data : in Registro;
        out_data : out Registro
    );
end component;

-- Instancias de módulos de Registro
for i in 0 to NUM_BITS-1 loop
    Registro_instancia : RegistroGenerico
        generic map (
            NUM_BITS => NUM_BITS
        )
        port map (
            clk => clk,
            rst => rst,
            in_data => in_data(i),
            out_data => out_data(i)
        );
end loop;

-- Mux selector de Registro
process (clk, rst)
begin
    if rst = '1' then
        out_data <= (others => '0');
    elsif rising_edge(clk) then
        out_data <= out_data(reg_select);
    end if;
end process;
```

Este código VHDL implementa un mux selector de registro. Se compone de un registro de control, que selecciona el registro que se va a leer o escribir, y de una serie de módulos de registro, que almacenan los datos.

El registro de control es un contador que se incrementa cada vez que se produce una señal de reloj ascendente. Cuando el contador alcanza el valor máximo, se reinicia a cero. Esto asegura que el registro de control recorre cíclicamente todos los registros.

Los módulos de registro son instancias del componente RegistroGenerico, que es un registro genérico que puede almacenar un número arbitrario de bits. El componente RegistroGenerico tiene tres puertos de entrada y un puerto de salida. Los puertos de entrada son la señal de reloj, la señal de reinicio y los datos de entrada. El puerto de salida es los datos de salida.

El mux selector de registro está implementado mediante un proceso que se ejecuta cada vez que se produce una señal de reloj ascendente. Este proceso selecciona el registro que se va a leer o escribir en función del valor del registro de control. Si el valor del registro de control es 0, se selecciona el primer registro. Si el valor del registro de control es 1, se selecciona el segundo registro, y así sucesivamente.

El mux selector de registro es un circuito útil que se puede utilizar en una variedad de aplicaciones. Por ejemplo, se puede utilizar para almacenar los datos de un sensor o para seleccionar los datos que se van a enviar a un dispositivo de salida.