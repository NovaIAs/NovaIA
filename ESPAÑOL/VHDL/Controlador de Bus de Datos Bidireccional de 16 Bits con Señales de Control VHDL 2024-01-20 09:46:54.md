```vhdl
-- Este código implementa un controlador de bus de datos bidireccional de 16 bits con señales de control.

-- Definición de la entidad.
entity Bus_de_Datos_Bidireccional is
    port (
        CLK : in STD_LOGIC; -- Reloj del sistema.
        RST : in STD_LOGIC; -- Señal de reset.

        DirIn : in STD_LOGIC; -- Dirección de entrada.
        DirOut : in STD_LOGIC; -- Dirección de salida.
        Din : in STD_LOGIC_VECTOR(15 downto 0); -- Datos de entrada.
        Dout : out STD_LOGIC_VECTOR(15 downto 0); -- Datos de salida.

        EN : in STD_LOGIC; -- Habilitación del bus.
        OE : in STD_LOGIC; -- Salida habilitada.
        RW : in STD_LOGIC; -- Lectura/Escritura.
    );
end Bus_de_Datos_Bidireccional;

-- Definición de la arquitectura.
architecture Estructura of Bus_de_Datos_Bidireccional is

    -- Declaración de señales internas.
    signal IntDin : STD_LOGIC_VECTOR(15 downto 0); -- Datos internos de entrada.
    signal IntDout : STD_LOGIC_VECTOR(15 downto 0); -- Datos internos de salida.
    signal IntDir : STD_LOGIC; -- Dirección interna.

    -- Circuito combinacional.
    IntDin <= Din when RW = '1' else IntDout;
    IntDir <= DirIn when EN = '1' else DirOut;
    IntDout <= Dout;

    -- Registro de salida.
    Dout <= IntDout when EN = '1' and OE = '1' else 'Z';

    -- Registro de dirección.
    DirOut <= IntDir;
end Estructura;
```

Este código implementa un controlador de bus de datos bidireccional de 16 bits con señales de control. La entidad define los puertos de entrada y salida del componente, mientras que la arquitectura define la lógica interna del componente.

El circuito combinacional se utiliza para determinar la dirección del bus y los datos de entrada y salida internos. El registro de salida se utiliza para almacenar los datos de salida del componente, mientras que el registro de dirección se utiliza para almacenar la dirección del bus.

El componente se puede utilizar para conectar dos dispositivos en un bus de datos bidireccional. El dispositivo maestro controla la dirección del bus y los datos de entrada, mientras que el dispositivo esclavo controla los datos de salida.