**Module d'interface de communication série**

```vhdl
-- Déclaration du module
MODULE interf_comm_serie EST
  -- Ports d'entrées/sorties
  PORT (
    clk      : IN  STD_LOGIC;
    rst      : IN  STD_LOGIC;
    tx_data  : IN  STD_LOGIC_VECTOR (7 DOWNTO 0);
    tx_start : IN  STD_LOGIC;
    tx_done  : OUT STD_LOGIC;
    rx_data  : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    rx_start : OUT STD_LOGIC;
    rx_done  : IN  STD_LOGIC
  );
END MODULE interf_comm_serie;

-- Architecture du module
ARCHITECTURE arch OF interf_comm_serie IS

  -- Constantes
  CONSTANT NB_BITS_PAR_CARACTERE : INTEGER := 8;

  -- Types
  TYPE etat_machine_tx IS (S_IDLE, S_START, S_TX_BIT0, S_TX_BIT1, S_TX_BIT2, S_TX_BIT3, S_TX_BIT4, S_TX_BIT5, S_TX_BIT6, S_TX_BIT7, S_TX_STOP);
  TYPE etat_machine_rx IS (S_IDLE, S_START, S_RX_BIT0, S_RX_BIT1, S_RX_BIT2, S_RX_BIT3, S_RX_BIT4, S_RX_BIT5, S_RX_BIT6, S_RX_BIT7, S_RX_STOP);

  -- Variables
  VARIABLE etat_tx : etat_machine_tx := S_IDLE;
  VARIABLE etat_rx : etat_machine_rx := S_IDLE;
  VARIABLE compteur_tx : INTEGER := 0;
  VARIABLE compteur_rx : INTEGER := 0;
  VARIABLE bit_tx : STD_LOGIC_VECTOR (7 DOWNTO 0);
  VARIABLE bit_rx : STD_LOGIC_VECTOR (7 DOWNTO 0);

  -- Processus de transmission
  PROCESS (clk, rst)
  BEGIN
    IF rst THEN
      etat_tx <= S_IDLE;
      compteur_tx <= 0;
    ELSIF clk'EVENT AND clk = '1' THEN
      CASE etat_tx IS
        WHEN S_IDLE =>
          IF tx_start = '1' THEN
            etat_tx <= S_START;
          END IF;
        WHEN S_START =>
          compteur_tx <= 1;
          bit_tx := tx_data;
          etat_tx <= S_TX_BIT0;
        WHEN S_TX_BIT0 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT1;
          END IF;
        WHEN S_TX_BIT1 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT2;
          END IF;
        WHEN S_TX_BIT2 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT3;
          END IF;
        WHEN S_TX_BIT3 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT4;
          END IF;
        WHEN S_TX_BIT4 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT5;
          END IF;
        WHEN S_TX_BIT5 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT6;
          END IF;
        WHEN S_TX_BIT6 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            bit_tx := bit_tx(6 DOWNTO 0) & tx_data(7);
            etat_tx <= S_TX_BIT7;
          END IF;
        WHEN S_TX_BIT7 =>
          compteur_tx <= compteur_tx + 1;
          IF compteur_tx = NB_BITS_PAR_CARACTERE THEN
            tx_done <= '1';
            etat_tx <= S_TX_STOP;
          ELSE
            etat_tx <= S_IDLE;
          END IF;
        WHEN S_TX_STOP =>
          IF tx_start = '0' THEN
            etat_tx <= S_IDLE;
          END IF;
        WHEN OTHERS =>
          etat_tx <= S_IDLE;
      END CASE;
    END IF;
  END PROCESS;

  -- Processus de réception
  PROCESS (clk, rst)
  BEGIN
    IF rst THEN
      etat_rx <= S_IDLE;
      compteur_rx <= 0;
    ELSIF clk'EVENT AND clk = '1' THEN
      CASE etat_rx IS
        WHEN S_IDLE =>
          IF rx_start = '1' THEN
            etat_rx <= S_START;
          END IF;
        WHEN S_START =>
          compteur_rx <= 1;
          etat_rx <= S_RX_BIT0;
        WHEN S_RX_BIT0 =>
          compteur_rx <= compteur_rx + 1;
          IF compteur_rx = NB_BITS_PAR_CARACTERE THEN
            rx_data <= bit_rx;
            etat_rx <= S_RX_STOP;
          ELSE
            bit_rx(0) <= rx_done;
            etat_rx <= S_RX_BIT1;
          END IF;
        WHEN S_RX_BIT1 =>
          compteur_rx <= compteur_rx + 1;
          IF compteur_rx = NB_BITS_PAR_CARACTERE THEN
            rx_data <= bit_rx;
            etat_rx <= S_RX_STOP;
          ELSE
            bit_rx(1) <= rx_done;
            etat_rx <= S_RX_BIT2;
          END IF;
        WHEN S_RX_BIT2 =>
          compteur_rx <= compteur_rx + 1;
          IF compteur_rx = NB_BITS_PAR_CARACTERE THEN
            rx_data <= bit_rx;
            etat_rx <= S_RX_STOP;
          ELSE
            bit_rx(2) <= rx_done;
            etat_rx <= S_RX_BIT3;
          END IF;
        WHEN S_RX_BIT3 =>
          compteur_rx <= compteur_rx + 1;
          IF compteur_rx = NB_BITS_PAR_CARACTERE THEN
            rx_data <= bit_rx;
            etat_rx <= S_RX_STOP;
          ELSE
            bit_rx(3) <= rx_done;
            etat_rx <= S_RX_BIT4;
          END IF;
        WHEN S_RX_BIT4 =>
          compteur_rx <= compteur_rx + 1;
          IF compteur_rx = NB_BITS_PAR_CARACTERE THEN
            rx_data