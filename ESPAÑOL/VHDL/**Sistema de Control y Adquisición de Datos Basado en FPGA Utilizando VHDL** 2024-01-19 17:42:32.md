```vhdl
-- VHDL: Lenguaje de Descripción de Hardware
-- Propósito: Diseñar un sistema de control y adquisición de datos basado en FPGA

-- Definición de la entidad principal del sistema
entidad Sistema_Control_Adquisicion_Datos es
    -- Definiciones de los puertos de entrada y salida
    -- Puerto de entrada para el reloj del sistema
    puerto (CLK: in std_logic);
    -- Puertos de entrada para los datos analógicos
    puerto (Analog_In_0: in std_logic_vector(11 downto 0);
           Analog_In_1: in std_logic_vector(11 downto 0));
    -- Puertos de salida para los datos digitales
    puerto (Digital_Out_0: out std_logic_vector(7 downto 0);
           Digital_Out_1: out std_logic_vector(7 downto 0));
end Sistema_Control_Adquisicion_Datos;

-- Arquitectura del sistema
arquitectura Control_Adquisicion_Digital_FPGA de Sistema_Control_Adquisicion_Datos es
    -- Señales internas para el reloj del sistema y los datos analógicos
    señal CLK_Dividido: std_logic;
    señal Analog_In_Filtrado_0: std_logic_vector(11 downto 0);
    señal Analog_In_Filtrado_1: std_logic_vector(11 downto 0);

    -- Componente para el filtro digital
    componente Filtro_Digital es
        -- Definiciones de los puertos de entrada y salida
        puerto (CLK: in std_logic;
               Analog_In: in std_logic_vector(11 downto 0);
               Analog_Out: out std_logic_vector(11 downto 0));
    end componente Filtro_Digital;

    -- Instancias de los componentes del sistema
    filtro_0: Filtro_Digital port map (CLK => CLK_Dividido,
                                       Analog_In => Analog_In_0,
                                       Analog_Out => Analog_In_Filtrado_0);
    filtro_1: Filtro_Digital port map (CLK => CLK_Dividido,
                                       Analog_In => Analog_In_1,
                                       Analog_Out => Analog_In_Filtrado_1);

    -- Bloque para dividir el reloj del sistema
    proceso
        begin
            -- Dividir el reloj del sistema por 2
            CLK_Dividido <= CLK'event and CLK = '1';
        end proceso;

    -- Bloque para procesar los datos analógicos filtrados y generar los datos digitales
    proceso
        begin
            -- Operar en el flanco de subida del reloj dividido
            if rising_edge(CLK_Dividido) then
                -- Generar los datos digitales
                Digital_Out_0 <= std_logic_vector(Analog_In_Filtrado_0(10 downto 3));
                Digital_Out_1 <= std_logic_vector(Analog_In_Filtrado_1(10 downto 3));
            end if;
        end proceso;
end Control_Adquisicion_Digital_FPGA;
```

Explicación del código:

- El código VHDL define una entidad llamada `Sistema_Control_Adquisicion_Datos` que representa el sistema de control y adquisición de datos.
- La arquitectura `Control_Adquisicion_Digital_FPGA` implementa la entidad.
- El sistema incluye puertos de entrada para el reloj del sistema (`CLK`) y los datos analógicos (`Analog_In_0` y `Analog_In_1`), y puertos de salida para los datos digitales (`Digital_Out_0` y `Digital_Out_1`).
- El bloque de división del reloj divide el reloj del sistema por 2 para obtener un reloj con una frecuencia más baja.
- El sistema utiliza dos instancias del componente `Filtro_Digital` para filtrar los datos analógicos antes de procesarlos.
- El bloque de procesamiento de datos opera en el flanco de subida del reloj dividido y genera los datos digitales en función de los datos analógicos filtrados.

En resumen, este código VHDL implementa un sistema de control y adquisición de datos que filtra los datos analógicos, los procesa y genera datos digitales como resultado. El sistema utiliza una arquitectura compleja con componentes y procesamiento de datos sincronizado con el reloj, lo que lo convierte en un ejemplo avanzado de diseño en VHDL.