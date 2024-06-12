**Code VHDL Complexe**

```vhdl
-- Bibliothèque pour les entités et les architectures.
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

-- Définition de l'entité principale.
entity Systeme_Complexe is
    port (
        clk : in std_logic;
        rst : in std_logic;

        -- Entrées et sorties pour les différents modules.
        entree1 : in std_logic_vector (31 downto 0);
        entree2 : in std_logic_vector (63 downto 0);
        entree3 : in std_logic_vector (127 downto 0);

        sortie1 : out std_logic_vector (31 downto 0);
        sortie2 : out std_logic_vector (63 downto 0);
        sortie3 : out std_logic_vector (127 downto 0)
    );
end Systeme_Complexe;

-- Architecture du système complexe.
architecture Implementionation of Systeme_Complexe is

    -- Composants pour les différents modules.
    component Module_1 is
        port (
            clk : in std_logic;
            rst : in std_logic;

            entree : in std_logic_vector (31 downto 0);
            sortie : out std_logic_vector (31 downto 0)
        );
    end component;

    component Module_2 is
        port (
            clk : in std_logic;
            rst : in std_logic;

            entree : in std_logic_vector (63 downto 0);
            sortie : out std_logic_vector (63 downto 0)
        );
    end component;

    component Module_3 is
        port (
            clk : in std_logic;
            rst : in std_logic;

            entree : in std_logic_vector (127 downto 0);
            sortie : out std_logic_vector (127 downto 0)
        );
    end component;

    -- Signaux internes pour la communication entre les modules.
    signal signal_interne1 : std_logic_vector (31 downto 0);
    signal signal_interne2 : std_logic_vector (63 downto 0);
    signal signal_interne3 : std_logic_vector (127 downto 0);

begin

    -- Instanciation des modules.
    module_1_instance : Module_1
        port map (
            clk => clk,
            rst => rst,

            entree => entree1,
            sortie => signal_interne1
        );

    module_2_instance : Module_2
        port map (
            clk => clk,
            rst => rst,

            entree => signal_interne1,
            sortie => signal_interne2
        );

    module_3_instance : Module_3
        port map (
            clk => clk,
            rst => rst,

            entree => signal_interne2,
            sortie => signal_interne3
        );

    -- Affectation des sorties du système.
    sortie1 <= signal_interne1;
    sortie2 <= signal_interne2;
    sortie3 <= signal_interne3;

end Implementionation;
```

**Explication du Code**

Ce code VHDL complexe implémente un système composé de trois modules interconnectés : Module_1, Module_2 et Module_3. Voici une explication générale de chaque partie du code :

**Entité Principale :** Système_Complexe

* Définit les ports d'entrée/sortie du système, y compris les entrées (entree1, entree2, entree3) et les sorties (sortie1, sortie2, sortie3).

**Architecture de Mise en Œuvre :** Implementionation

* Instancie les trois modules (Module_1, Module_2, Module_3) à l'aide de signaux internes pour la communication entre eux.
* Connecte les entrées et les sorties des modules comme indiqué dans le schéma de conception.
* Assigne les signaux internes aux ports de sortie du système pour les rendre accessibles en dehors du module.

**Composants :** Module_1, Module_2, Module_3

* (Non inclus dans ce code fourni car ils doivent être définis séparément)
* Ils représentent les modules individuels qui effectuent des opérations spécifiques sur leurs entrées et produisent des sorties.

Ce code complexe démontre des concepts avancés en VHDL, tels que l'instanciation de composants, la communication entre les modules et l'affectation des ports de sortie. Il est destiné à illustrer la complexité et la capacité du langage VHDL pour la conception de systèmes matériels complexes.