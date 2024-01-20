```vhdl
-- Este es un código VHDL muy complejo que implementa un procesador RISC de 32 bits.
-- El código está dividido en varios módulos, cada uno de los cuales implementa una parte diferente del procesador.

-- El módulo principal es el módulo `procesador`. Este módulo contiene la lógica principal del procesador, incluyendo las unidades de control, aritmética y lógica.
module procesador is
  -- Las entradas del módulo son las siguientes:
  -- * `clk`: La señal de reloj del procesador.
  -- * `reset`: La señal de reset del procesador.
  -- * `instruccion`: La instrucción que se va a ejecutar.
  -- * `datos`: Los datos que se van a procesar.

  -- Las salidas del módulo son las siguientes:
  -- * `resultado`: El resultado de la ejecución de la instrucción.
  -- * `estado`: El estado del procesador.

  -- La lógica del módulo se implementa utilizando una máquina de estados finitos. La máquina de estados finitos se implementa utilizando un proceso `case`.
  process (clk, reset)
  begin
    if reset then
      -- El procesador se inicializa.
      estado <= INICIAL;
    elsif rising_edge(clk) then
      -- El procesador ejecuta la siguiente instrucción.
      case estado is
        when INICIAL =>
          -- El procesador lee la siguiente instrucción.
          instruccion <= memoria[contador_programa];
          contador_programa <= contador_programa + 4;
          estado <= DECODIFICACION;
        when DECODIFICACION =>
          -- El procesador decodifica la instrucción.
          decoded_instruction <= instruccion(31..26);
          estado <= EJECUCION;
        when EJECUCION =>
          -- El procesador ejecuta la instrucción.
          case decoded_instruction is
            when "000000": -- ADD
              resultado <= datos_a + datos_b;
            when "000001": -- SUB
              resultado <= datos_a - datos_b;
            when "000010": -- AND
              resultado <= datos_a & datos_b;
            when "000011": -- OR
              resultado <= datos_a | datos_b;
            when "000100": -- XOR
              resultado <= datos_a xor datos_b;
            when "000101": -- SHL
              resultado <= datos_a << datos_b;
            when "000110": -- SHR
              resultado <= datos_a >> datos_b;
            when "000111": -- SAR
              resultado <= datos_a >>> datos_b;
            when "001000": -- MOV
              datos_a <= datos_b;
            when "001001": -- LDI
              datos_a <= inmediato;
            when "001010": -- STL
              memoria[datos_a] <= datos_b;
            when "001011": -- LDX
              datos_a <= memoria[datos_b];
            when "001100": -- BNZ
              if resultado <> 0 then
                contador_programa <= contador_programa + inmediato;
              end if;
            when "001101": -- BEQ
              if resultado = 0 then
                contador_programa <= contador_programa + inmediato;
              end if;
            when "001110": -- BLT
              if resultado < 0 then
                contador_programa <= contador_programa + inmediato;