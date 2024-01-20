```vhdl
-- Código VHDL complejo

-- Se define el nombre de la entidad
entidad sistema is

-- Se definen las entradas y salidas de la entidad
port (
  clk : in std_logic;  -- Reloj de entrada
  rst : in std_logic;  -- Reinicio de entrada
  dato_in : in std_logic_vector(7 downto 0);  -- Dato de entrada
  dato_out : out std_logic_vector(7 downto 0)  -- Dato de salida
);

-- Se define la arquitectura del sistema
arquitectura estructural de sistema is

-- Se definen los componentes del sistema
componente sumador is
  port (
    a : in std_logic_vector(7 downto 0);  -- Primer sumando
    b : in std_logic_vector(7 downto 0);  -- Segundo sumando
    cin : in std_logic;  -- Bit de acarreo de entrada
    sum : out std_logic_vector(8 downto 0);  -- Suma
    cout : out std_logic  -- Bit de acarreo de salida
  );
end component;

componente registro is
  port (
    clk : in std_logic;  -- Reloj de entrada
    rst : in std_logic;  -- Reinicio de entrada
    dato_in : in std_logic_vector(7 downto 0);  -- Dato de entrada
    dato_out : out std_logic_vector(7 downto 0)  -- Dato de salida
  );
end component;

-- Se definen las señales internas del sistema
signal suma : std_logic_vector(8 downto 0);  -- Suma intermedia
signal dato_reg : std_logic_vector(7 downto 0);  -- Dato registrado

-- Se instancia el sumador 8 bits
sumador_inst : sumador
port map (
  a => dato_in,
  b => dato_reg,
  cin => '0',
  sum => suma,
  cout => open  -- Se ignora el bit de acarreo de salida
);

-- Se instancia el registro 8 bits
registro_inst : registro
port map (
  clk => clk,
  rst => rst,
  dato_in => suma(7 downto 0),  -- Se toma la suma de los 8 bits más significativos
  dato_out => dato_reg
);

-- Se asigna el dato de salida a la suma de los 8 bits menos significativos
dato_out <= suma(7 downto 0);

end architecture;

-- Fin de la entidad sistema
end sistema;
```

**Explicación del código:**

Este es un código VHDL de un sistema digital complejo que consiste en un sumador de 8 bits y un registro de 8 bits. El sistema suma dos datos de entrada de 8 bits y almacena el resultado en el registro. El resultado de la suma se puede leer en la salida del sistema.

**Componentes:**

* **Sumador:** El componente sumador es un circuito combinacional que suma dos números de 8 bits y genera la suma y el acarreo.
* **Registro:** El componente registro es un circuito secuencial que almacena un dato de 8 bits y lo actualiza en cada flanco de subida del reloj.

**Señales:**

* **suma:** Es una señal de 9 bits que almacena la suma de los dos datos de entrada.
* **dato_reg:** Es una señal de 8 bits que almacena el dato registrado.

**Instanciación de componentes:**

* Se instancia el sumador 8 bits con el nombre sumador_inst.
* Se instancia el registro 8 bits con el nombre registro_inst.

**Conexiones:**

* La salida del sumador se conecta a la entrada del registro.
* La salida del registro se conecta a la salida del sistema.

**Asignaciones:**

* Se asigna el dato de salida del sistema a la suma de los 8 bits menos significativos.

**Explicación del funcionamiento:**

1. El sistema se inicializa con el reinicio activo. Esto borra el registro y pone la suma a cero.
2. Cuando el reloj hace un flanco de subida, el dato de entrada se suma al dato registrado en el sumador.
3. La suma se almacena en el registro y se puede leer en la salida del sistema.
4. El proceso se repite en cada flanco de subida del reloj.