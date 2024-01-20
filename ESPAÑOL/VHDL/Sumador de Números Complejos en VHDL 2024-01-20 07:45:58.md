```vhdl
-- Declaración de la entidad
entidad Sumador_Complejo es
  -- Puertos de entrada y salida
  puerto (
    A_Real : in std_logic_vector(7 downto 0);
    A_Imag : in std_logic_vector(7 downto 0);
    B_Real : in std_logic_vector(7 downto 0);
    B_Imag : in std_logic_vector(7 downto 0);
    S_Real : out std_logic_vector(8 downto 0);
    S_Imag : out std_logic_vector(8 downto 0)
  );
end entidad;

-- Declaración de la arquitectura
arquitectura SumadorComplejo of Sumador_Complejo is
  -- Constantes y variables internas
  constant CERO : std_logic_vector(1 downto 0) := "00";
  constant UNO : std_logic_vector(1 downto 0) := "01";

  variable CarryR : std_logic_vector(1 downto 0) := CERO;
  variable CarryI : std_logic_vector(1 downto 0) := CERO;

  -- Proceso de suma
  proceso
  begin
    -- Suma de las partes reales
    CarryR := CERO;
    S_Real(7 downto 0) := A_Real(7 downto 0) + B_Real(7 downto 0) + CarryR;
    if S_Real(7 downto 0) > "11111111" then
      CarryR := UNO;
      S_Real(7 downto 0) := S_Real(7 downto 0) - "100000000";
    end if;
    S_Real(8) := CarryR;

    -- Suma de las partes imaginarias
    CarryI := CERO;
    S_Imag(7 downto 0) := A_Imag(7 downto 0) + B_Imag(7 downto 0) + CarryI;
    if S_Imag(7 downto 0) > "11111111" then
      CarryI := UNO;
      S_Imag(7 downto 0) := S_Imag(7 downto 0) - "100000000";
    end if;
    S_Imag(8) := CarryI;
  end proceso;
end arquitectura;
```

Explicación del código:

1. `entidad Sumador_Complejo` es la declaración de la entidad, que define los puertos de entrada y salida del módulo. En este caso, tenemos cuatro puertos de entrada para las partes reales e imaginarias de los dos números complejos, y dos puertos de salida para las partes reales e imaginarias del resultado de la suma.

2. `arquitectura SumadorComplejo of Sumador_Complejo` es la declaración de la arquitectura, que define la implementación del módulo.

3. Las constantes `CERO` y `UNO` se utilizan para representar los valores binarios 0 y 1, respectivamente.

4. Las variables `CarryR` y `CarryI` se utilizan para llevar el acarreo de la suma de las partes reales e imaginarias, respectivamente.

5. El proceso `begin ... end` realiza la suma de los números complejos.

6. En primer lugar, se calcula la suma de las partes reales utilizando el operador `+`. Si la suma es mayor que 255 (el valor máximo que se puede representar con 8 bits), se incrementa el acarreo `CarryR` y se resta 256 de la suma.

7. A continuación, se calcula la suma de las partes imaginarias de forma similar.

8. Finalmente, los valores de las partes reales e imaginarias del resultado de la suma se asignan a los puertos de salida `S_Real` y `S_Imag`.