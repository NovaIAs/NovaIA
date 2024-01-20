```vhdl
-- Esta entidad define la interfaz del módulo.
entity modulo is
  port (
    clk : in std_logic;
    reset : in std_logic;
    dato_entrada : in std_logic_vector(7 downto 0);
    dato_salida : out std_logic_vector(7 downto 0)
  );
end modulo;

-- Esta arquitectura implementa la funcionalidad del módulo.
architecture implementacion of modulo is

  -- Definición de señales internas.
  signal dato_interno : std_logic_vector(7 downto 0);
  signal contador : integer range 0 to 255;

begin

  -- Bloque de proceso síncrono.
  process(clk, reset)
  begin
    if reset = '1' then
      contador <= 0;
      dato_interno <= (others => '0');
    elsif rising_edge(clk) then
      contador <= contador + 1;
      dato_interno <= dato_entrada;
    end if;
  end process;

  -- Bloque de proceso asíncrono.
  process(contador)
  begin
    if contador = 255 then
      dato_salida <= dato_interno;
    end if;
  end process;

end implementacion;
```

Este código implementa un módulo VHDL que realiza lo siguiente:

* Recibe un dato de entrada de 8 bits.
* Almacena el dato de entrada en un registro interno.
* Cuenta el número de ciclos de reloj desde que se recibió el dato de entrada.
* Cuando el número de ciclos de reloj alcanza 255, el dato almacenado en el registro interno se envía a la salida.

El módulo se implementa utilizando dos bloques de proceso: un bloque de proceso síncrono y un bloque de proceso asíncrono.

El bloque de proceso síncrono se ejecuta en cada flanco de subida del reloj. Almacena el dato de entrada en el registro interno y cuenta el número de ciclos de reloj desde que se recibió el dato de entrada.

El bloque de proceso asíncrono se ejecuta cuando el número de ciclos de reloj alcanza 255. Envía el dato almacenado en el registro interno a la salida.

El módulo está diseñado para funcionar con una frecuencia de reloj de 1 MHz. Con esta frecuencia de reloj, el dato de salida se enviará a la salida aproximadamente cada 256 ms.