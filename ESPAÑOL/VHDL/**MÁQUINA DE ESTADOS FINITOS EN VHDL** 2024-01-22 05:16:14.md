**CÓDIGO VHDL**

```vhdl
-- Paquete de definiciones
PACKAGE instrucciones IS

    TYPE estados IS (S0, S1, S2, S3);
    TYPE datos IS ARRAY(0 TO 7) OF STD_LOGIC;

    -- Función de transición de estado
    FUNCTION siguiente_estado(estado_actual: estados; datos_entrada: datos) RETURN estados IS
    BEGIN
        CASE estado_actual IS
            WHEN S0 =>
                IF datos_entrada(0) = '1' THEN
                    RETURN S1;
                ELSE
                    RETURN S0;
                END IF;
            WHEN S1 =>
                IF datos_entrada(1) = '1' THEN
                    RETURN S2;
                ELSE
                    RETURN S0;
                END IF;
            WHEN S2 =>
                IF datos_entrada(2) = '1' THEN
                    RETURN S3;
                ELSE
                    RETURN S0;
                END IF;
            WHEN S3 =>
                IF datos_entrada(3) = '1' THEN
                    RETURN S0;
                ELSE
                    RETURN S3;
                END IF;
        END CASE;
    END siguiente_estado;

    -- Función de salida
    FUNCTION salida(estado_actual: estados; datos_entrada: datos) RETURN datos IS
    BEGIN
        CASE estado_actual IS
            WHEN S0 =>
                RETURN "00000000";
            WHEN S1 =>
                RETURN "00000001";
            WHEN S2 =>
                RETURN "00000010";
            WHEN S3 =>
                RETURN "00000011";
        END CASE;
    END salida;

END instrucciones;

-- Arquitectura del circuito
ARCHITECTURE comportamiento OF maquina_de_estados IS

    -- Señales y variables
    TYPE estados IS INSTRUCTIONS.estados;
    SIGNAL estado_actual, estado_siguiente: estados;
    SIGNAL datos_entrada, salida_final: INSTRUCTIONS.datos;

BEGIN

    -- Lógica secuencial
    estado_siguiente <= instrucciones.siguiente_estado(estado_actual, datos_entrada);
    estado_actual <= estado_siguiente;

    -- Lógica combinacional
    salida_final <= instrucciones.salida(estado_actual, datos_entrada);

END comportamiento;
```

**EXPLICACIÓN**

Este código VHDL describe una máquina de estados finitos (FSM) que procesa una secuencia de ocho bits de entrada y genera una salida de ocho bits. La FSM tiene cuatro estados, S0, S1, S2 y S3. El estado inicial es S0.

La función `siguiente_estado` determina el siguiente estado de la FSM en función del estado actual y los datos de entrada. La función `salida` determina la salida de la FSM en función del estado actual y los datos de entrada.

La arquitectura `comportamiento` del circuito implementa la FSM utilizando bloques de lógica secuencial y lógica combinacional. La lógica secuencial se utiliza para almacenar el estado actual de la FSM y para calcular el siguiente estado. La lógica combinacional se utiliza para calcular la salida de la FSM.

El código VHDL se puede utilizar para sintetizar un circuito integrado (IC) que implementa la FSM. El IC se puede utilizar en una variedad de aplicaciones, como el control de motores, el procesamiento de señales y el diseño de computadoras.