```vhdl
-- Define la entidad del circuito
entidad mi_circuito es
  -- Puertos de entrada
  puerto (
    clk : in std_logic; -- Reloj de sistema
    rst : in std_logic; -- Reset de sistema
    data_in : in std_logic_vector(7 downto 0); -- Datos de entrada
  );
  -- Puertos de salida
  puerto (
    data_out : out std_logic_vector(7 downto 0) -- Datos de salida
  );
end mi_circuito;

-- Define la arquitectura del circuito
arquitectura mi_arquitectura of mi_circuito is
  -- Declaración de señales internas
  signal reg_data : std_logic_vector(7 downto 0); -- Registro de datos

  -- Componente del flip-flop
  componente flip_flop es
    -- Puertos del flip-flop
    puerto (
      clk : in std_logic; -- Reloj de sistema
      rst : in std_logic; -- Reset de sistema
      data_in : in std_logic; -- Datos de entrada
      data_out : out std_logic -- Datos de salida
    );
  end componente;

  -- Instancias de los flip-flops
  flop_0 : flip_flop port map(clk, rst, data_in(0), reg_data(0));
  flop_1 : flip_flop port map(clk, rst, data_in(1), reg_data(1));
  flop_2 : flip_flop port map(clk, rst, data_in(2), reg_data(2));
  flop_3 : flip_flop port map(clk, rst, data_in(3), reg_data(3));
  flop_4 : flip_flop port map(clk, rst, data_in(4), reg_data(4));
  flop_5 : flip_flop port map(clk, rst, data_in(5), reg_data(5));
  flop_6 : flip_flop port map(clk, rst, data_in(6), reg_data(6));
  flop_7 : flip_flop port map(clk, rst, data_in(7), reg_data(7));

  -- Asignación de los datos de salida
  data_out <= reg_data;
begin
end mi_arquitectura;
```

Explicación del código:

1. **Entidad mi_circuito:** Define la interfaz del circuito, incluyendo los puertos de entrada y salida.
2. **Arquitectura mi_arquitectura:** Define la arquitectura del circuito, incluyendo la lógica interna y las instancias de los componentes.
3. **Señales internas:** Se definen las señales internas del circuito, en este caso el registro de datos `reg_data`.
4. **Componente flip_flop:** Se define un componente flip-flop, que es un elemento básico de la lógica digital.
5. **Instancias de los flip-flops:** Se crean ocho instancias del componente flip-flop, una para cada bit de los datos de entrada.
6. **Asignación de los datos de salida:** Se asigna el valor del registro de datos a los datos de salida.

Este código implementa un registro de desplazamiento de 8 bits, que almacena los datos de entrada y los desplaza hacia la derecha al ser cargado.