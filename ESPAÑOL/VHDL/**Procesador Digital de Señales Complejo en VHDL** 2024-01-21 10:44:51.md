```vhdl
-- Componentes
COMPONENT SumadorComplejo is
  PORT (
    A_Real, A_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    B_Real, B_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    S_Real, S_Imag: OUT STD_LOGIC_VECTOR(31 downto 0)
  );
END COMPONENT;

COMPONENT MultiplicadorComplejo is
  PORT (
    A_Real, A_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    B_Real, B_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    P_Real, P_Imag: OUT STD_LOGIC_VECTOR(31 downto 0)
  );
END COMPONENT;

COMPONENT MultiplicadorEscalar is
  PORT (
    A: IN STD_LOGIC_VECTOR(31 downto 0);
    B: IN STD_LOGIC_VECTOR(31 downto 0);
    P: OUT STD_LOGIC_VECTOR(31 downto 0)
  );
END COMPONENT;


-- Entidad
ENTITY ProcesadorDSP is
  PORT (
    CLK, RST: IN STD_LOGIC;
    X_Real, X_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    Y_Real, Y_Imag: IN STD_LOGIC_VECTOR(31 downto 0);
    Z_Real, Z_Imag: OUT STD_LOGIC_VECTOR(31 downto 0)
  );
END ENTITY;

-- Arquitectura
ARCHITECTURE Behaviour of ProcesadorDSP is

  -- Señales internas
  signal A_Real, A_Imag: STD_LOGIC_VECTOR(31 downto 0);
  signal B_Real, B_Imag: STD_LOGIC_VECTOR(31 downto 0);
  signal P_Real, P_Imag: STD_LOGIC_VECTOR(31 downto 0);
  signal S_Real, S_Imag: STD_LOGIC_VECTOR(31 downto 0);

BEGIN

  -- Instanciación de los componentes
  SumadorComplejo: SumadorComplejo
    PORT MAP (
      A_Real => A_Real,
      A_Imag => A_Imag,
      B_Real => B_Real,
      B_Imag => B_Imag,
      S_Real => S_Real,
      S_Imag => S_Imag
    );

  MultiplicadorComplejo: MultiplicadorComplejo
    PORT MAP (
      A_Real => X_Real,
      A_Imag => X_Imag,
      B_Real => Y_Real,
      B_Imag => Y_Imag,
      P_Real => P_Real,
      P_Imag => P_Imag
    );

  MultiplicadorEscalar: MultiplicadorEscalar
    PORT MAP (
      A => P_Real,
      B => Z_Real,
      P => Z_Real
    );

  MultiplicadorEscalar: MultiplicadorEscalar
    PORT MAP (
      A => P_Imag,
      B => Z_Imag,
      P => Z_Imag
    );

  -- Registro de las señales internas
  PROCESS (CLK, RST)
  BEGIN
    IF RST THEN
      A_Real <= (OTHERS => '0');
      A_Imag <= (OTHERS => '0');
      B_Real <= (OTHERS => '0');
      B_Imag <= (OTHERS => '0');
      P_Real <= (OTHERS => '0');
      P_Imag <= (OTHERS => '0');
      S_Real <= (OTHERS => '0');
      S_Imag <= (OTHERS => '0');
    ELSIF RISING_EDGE(CLK) THEN
      A_Real <= X_Real;
      A_Imag <= X_Imag;
      B_Real <= Y_Real;
      B_Imag <= Y_Imag;
      P_Real <= S_Real;
      P_Imag <= S_Imag;
    END IF;
  END PROCESS;

END ARCHITECTURE;
```

Explicación:

El código VHDL implementa un procesador digital de señales (DSP) que realiza operaciones aritméticas complejas en dos números complejos de 32 bits. El procesador consiste en componentes que realizan las operaciones de suma, multiplicación y multiplicación escalar de números complejos.

El componente SumadorComplejo suma dos números complejos y genera la suma como salida. El componente MultiplicadorComplejo multiplica dos números complejos y genera el producto como salida. El componente MultiplicadorEscalar multiplica un número complejo por un escalar y genera el producto como salida.

La entidad ProcesadorDSP es la interfaz externa del procesador. Tiene puertos para la entrada de los dos números complejos X e Y, y puertos para la salida del resultado Z.

La arquitectura Behaviour define la implementación interna del procesador. Incluye los componentes SumadorComplejo, MultiplicadorComplejo y MultiplicadorEscalar, y los conecta para realizar las operaciones aritméticas deseadas. También incluye un registro que almacena los valores de las señales internas entre los ciclos de reloj.

El proceso en la arquitectura Behaviour se ejecuta en cada ciclo de reloj y lee los valores de los puertos de entrada. Luego, calcula los valores de las señales internas y los almacena en el registro. Los valores de las señales internas se utilizan para calcular los valores de los puertos de salida.

Este código es complejo porque implementa una serie de operaciones matemáticas en números complejos. La implementación utiliza una combinación de componentes y procesos para lograr la funcionalidad deseada. El código está bien documentado y es fácil de entender para alguien con conocimientos de VHDL.