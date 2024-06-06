```vhdl
-- Déclaration de l'architecture de l'entité "complexe"
architecture complexe of complexe is
    -- Déclaration des signaux et variables internes
    signal clk_50mhz : std_logic;
    signal reset_n : std_logic;
    signal data_in : std_logic_vector(7 downto 0);
    signal data_out : std_logic_vector(7 downto 0);

    -- Déclaration des composants
    component compteur_16bits is
        port (
            clk : in std_logic;
            reset_n : in std_logic;
            enable : in std_logic;
            data_in : in std_logic_vector(15 downto 0);
            data_out : out std_logic_vector(15 downto 0)
        );
    end component;

    component registre is
        generic (
            nb_bits : integer
        );
        port (
            clk : in std_logic;
            reset_n : in std_logic;
            enable : in std_logic;
            data_in : in std_logic_vector(nb_bits - 1 downto 0);
            data_out : out std_logic_vector(nb_bits - 1 downto 0)
        );
    end component;

    -- Instanciation des composants
    compteur_16bits : compteur_16bits
        port map(
            clk => clk_50mhz,
            reset_n => reset_n,
            enable => '1',
            data_in => std_logic_vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
            data_out => data_out
        );

    registre_8bits : registre
        generic map(nb_bits => 8)
        port map(
            clk => clk_50mhz,
            reset_n => reset_n,
            enable => data_out(1),
            data_in => data_in,
            data_out => data_out
        );

begin

    -- Processus de génération du signal d'horloge à 50 MHz
    process
    begin
        clk_50mhz <= not clk_50mhz after 10 ns;
    end process;

    -- Processus de réinitialisation
    process (reset_n)
    begin
        if reset_n = '0' then
            data_in <= (others => '0');
            data_out <= (others => '0');
        end if;
    end process;

    -- Processus de lecture des données d'entrée
    process (data_in)
    begin
        if data_out(1) = '1' then
            data_in <= data_in;
        end if;
    end process;

end architecture complexe;
```

**Explication du code**

Ce code VHDL implémente un circuit complexe qui implémente les fonctions suivantes :

* Génération d'un signal d'horloge à 50 MHz
* Réinitialisation du circuit lorsque le signal `reset_n` est bas
* Stockage des données d'entrée dans un registre 8 bits lorsque le compteur 16 bits atteint 1
* Lecture des données d'entrée du registre 8 bits lorsque le compteur 16 bits atteint 2

Le circuit est divisé en trois composants :

* Un compteur 16 bits qui incrémente sa valeur chaque cycle d'horloge
* Un registre 8 bits qui stocke les données d'entrée
* Un processus qui gère la lecture et l'écriture des données d'entrée

Le code commence par la déclaration de l'architecture de l'entité `complexe`. Cette architecture définit les signaux et les variables internes du circuit, ainsi que les composants qui seront instanciés.

Ensuite, le code déclare les composants qui seront utilisés dans le circuit. Dans ce cas, deux composants sont déclarés : un compteur 16 bits et un registre 8 bits.

Après la déclaration des composants, le code instancie les composants et les connecte aux signaux et variables internes. Dans ce cas, le compteur 16 bits est instancié et connecté au signal d'horloge, au signal de réinitialisation, au signal d'activation et aux signaux d'entrée et de sortie des données. Le registre 8 bits est instancié et connecté au signal d'horloge, au signal de réinitialisation, au signal d'activation et aux signaux d'entrée et de sortie des données.

Enfin, le code implémente les processus qui gèrent le fonctionnement du circuit. Dans ce cas, trois processus sont implémentés : un processus qui génère le signal d'horloge à 50 MHz, un processus qui gère la réinitialisation du circuit et un processus qui gère la lecture et l'écriture des données d'entrée.