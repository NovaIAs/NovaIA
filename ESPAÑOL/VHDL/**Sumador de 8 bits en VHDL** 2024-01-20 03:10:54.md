```vhdl
-- Módulo superior del diseño
entidad sistema es
    puerto (
        clk : in std_logic;
        rst : in std_logic;
        entrada : in std_logic_vector(7 downto 0);
        salida : out std_logic_vector(7 downto 0)
    );
end entidad sistema;

-- Arquitectura del diseño
arquitectura estructural de sistema es
    -- Componentes del diseño
    componente sumador es
        puerto (
            a : in std_logic_vector(7 downto 0);
            b : in std_logic_vector(7 downto 0);
            carry_in : in std_logic;
            carry_out : out std_logic;
            suma : out std_logic_vector(7 downto 0)
        );
    end componente;

    componente registro is
        puerto (
            clk : in std_logic;
            rst : in std_logic;
            d : in std_logic_vector(7 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
    end componente;

    -- Señales internas del diseño
    señal suma_1, suma_2 : std_logic_vector(7 downto 0);
    señal carry_1, carry_2 : std_logic;
    señal registro_1, registro_2 : std_logic_vector(7 downto 0);

    -- Instancias de los componentes
    sumador_1 : sumador port map (
        a => entrada(7 downto 4),
        b => entrada(3 downto 0),
        carry_in => '0',
        carry_out => carry_1,
        suma => suma_1
    );

    sumador_2 : sumador port map (
        a => suma_1,
        b => registro_1,
        carry_in => carry_1,
        carry_out => carry_2,
        suma => suma_2
    );

    registro_1 : registro port map (
        clk => clk,
        rst => rst,
        d => entrada,
        q => registro_1
    );

    registro_2 : registro port map (
        clk => clk,
        rst => rst,
        d => suma_2,
        q => registro_2
    );

    -- Asignación de la salida del diseño
    salida <= registro_2;
begin
end arquitectura estructural;
```

Explicación del código:

El código VHDL implementa un circuito digital que realiza la suma de dos números de 8 bits. El circuito está compuesto por dos sumadores de 4 bits y dos registros de 8 bits. Los sumadores se utilizan para sumar los dos números de 8 bits, y los registros se utilizan para almacenar los resultados de las sumas.

El circuito funciona de la siguiente manera:

1. Los dos números de 8 bits se introducen en el circuito a través de la entrada.
2. Los dos números de 8 bits se suman utilizando los dos sumadores de 4 bits.
3. El resultado de la primera suma se almacena en el primer registro.
4. El resultado de la segunda suma se almacena en el segundo registro.
5. El resultado de la suma final se puede leer en la salida.

El código VHDL utiliza una serie de componentes para implementar el circuito. Los componentes son bloques de código predefinidos que se pueden utilizar para implementar funciones comunes. En este caso, se utilizan dos componentes: un sumador de 4 bits y un registro de 8 bits.

El código VHDL también utiliza una serie de señales internas para comunicar los datos entre los diferentes componentes. Las señales internas son variables que se definen dentro de la arquitectura del diseño. En este caso, se utilizan cuatro señales internas: suma_1, suma_2, carry_1 y carry_2.

El código VHDL utiliza una serie de instrucciones para implementar el circuito. Las instrucciones se utilizan para controlar el flujo de datos a través del circuito. En este caso, se utilizan las siguientes instrucciones:

* La instrucción `port map` se utiliza para conectar los puertos de los componentes a las señales internas del diseño.
* La instrucción `<=` se utiliza para asignar valores a las señales internas del diseño.
* La instrucción `begin` se utiliza para iniciar un bloque de instrucciones.
* La instrucción `end` se utiliza para terminar un bloque de instrucciones.

El código VHDL es un lenguaje de descripción de hardware (HDL) que se utiliza para diseñar circuitos digitales. Los HDL son lenguajes de programación que se utilizan para describir la estructura y el comportamiento de los circuitos digitales. El código VHDL se utiliza para diseñar una amplia variedad de circuitos digitales, incluyendo microprocesadores, memorias y dispositivos de entrada/salida.