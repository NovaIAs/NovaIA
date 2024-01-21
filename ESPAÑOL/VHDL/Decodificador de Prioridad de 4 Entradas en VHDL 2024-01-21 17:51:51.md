```vhdl
-- Decodificador de prioridad de 4 entradas en VHDL

-- Definición de la entidad
entity decodificador_prioridad_4_entradas is
    generic (
        ancho_datos: natural := 4; -- Ancho de los datos de entrada
        ancho_salida: natural := 2 -- Ancho de la salida seleccionada
    );
    port (
        datos: in std_logic_vector(ancho_datos-1 downto 0); -- Datos de entrada
        seleccionada: out std_logic_vector(ancho_salida-1 downto 0) -- Salida seleccionada
    );
end entity;

-- Definición de la arquitectura
architecture prioridad_4_entradas of decodificador_prioridad_4_entradas is
    signal vector_seleccion: std_logic_vector(ancho_datos-1 downto 0); -- Vector de selección
    signal vector_habilitacion: std_logic_vector(ancho_datos-1 downto 0); -- Vector de habilitación
    signal valor_seleccionado: std_logic_vector(ancho_salida-1 downto 0); -- Valor seleccionado
begin

        -- Generar el vector de selección
        vector_seleccion(ancho_datos-1) <= '1' when datos(ancho_datos-1) = '1' else '0';
        for i in ancho_datos-2 to 0 loop
            vector_seleccion(i) <= vector_seleccion(i+1) and datos(i);
        end loop;

        -- Generar el vector de habilitación
        vector_habilitacion(ancho_datos-1) <= '1';
        for i in ancho_datos-2 to 0 loop
            vector_habilitacion(i) <= vector_habilitacion(i+1) or datos(i);
        end loop;

        -- Seleccionar el valor de salida
        valor_seleccionado <= datos(std_logic(ancho_datos-1)) when vector_seleccion(ancho_datos-1) = '1' else
                            datos(std_logic(ancho_datos-2)) when vector_seleccion(ancho_datos-2) = '1' else
                            ...
                            datos(std_logic(0)) when vector_seleccion(0) = '1' else
                            "00";

        -- Asignar el valor seleccionado a la salida
        seleccionada <= valor_seleccionado;

    end architecture;
```

Explicación del código:

* El código define una entidad llamada `decodificador_prioridad_4_entradas` que recibe 4 bits de datos de entrada y selecciona el bit de mayor prioridad que esté activo.
* La arquitectura `prioridad_4_entradas` implementa la lógica del decodificador.
* La señal `vector_seleccion` es un vector de 4 bits que indica el bit de entrada de mayor prioridad que está activo.
* La señal `vector_habilitacion` es un vector de 4 bits que indica si el bit de entrada correspondiente está activo o no.
* La señal `valor_seleccionado` es un vector de 2 bits que contiene el valor seleccionado.
* La instrucción `case` selecciona el bit de entrada de mayor prioridad que está activo y asigna el valor correspondiente a la señal `valor_seleccionado`.
* La señal `seleccionada` se asigna al valor seleccionado.