```vhdl
-- This code implements a finite state machine (FSM) in VHDL.

-- Define the states of the FSM.
constant IDLE : natural := 0;
constant READY : natural := 1;
constant BUSY : natural := 2;

-- Define the inputs and outputs of the FSM.
signal clock : std_logic;
signal reset : std_logic;
signal input : std_logic;
signal output : std_logic;

-- Define the state register.
signal state : natural := IDLE;

-- Define the next state logic.
process (clock, reset, input)
begin
  if reset = '1' then
    state <= IDLE;
  elsif (clock'event and clock = '1') then
    case state is
      when IDLE =>
        if input = '1' then
          state <= READY;
        end if;
      when READY =>
        output <= '1';
        state <= BUSY;
      when BUSY =>
        if input = '0' then
          state <= IDLE;
        end if;
    end case;
  end if;
end process;

-- Define the output logic.
process (state)
begin
  case state is
    when IDLE =>
      output <= '0';
    when READY =>
      output <= '0';
    when BUSY =>
      output <= '1';
  end case;
end process;
```

Este código implementa una máquina de estados finitos (FSM) en VHDL.

La FSM tiene tres estados: IDLE, READY y BUSY.

La entrada de la FSM es una señal booleana llamada input.

La salida de la FSM es una señal booleana llamada output.

El comportamiento de la FSM está definido por el proceso con la etiqueta "next state logic".

El proceso "next state logic" actualiza el estado de la FSM en función de la entrada y el estado actual.

El comportamiento de la salida de la FSM está definido por el proceso con la etiqueta "output logic".

El proceso "output logic" actualiza la salida de la FSM en función del estado actual.