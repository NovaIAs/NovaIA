**Module de commande d'ascenseur en VHDL**

**Objectif:** Ce module implémente la logique de commande d'un ascenseur à plusieurs étages, gérant les appels d'étage, la sélection d'étage et le mouvement de la cabine.

**Description du code:**

```vhdl
-- Fichier de définition du module
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ControleurAscenseur is
    -- Entrées
    Port (clk : in std_logic;
          reset : in std_logic;
          appelsHaut : in std_logic_vector(NombreEtages - 1 downto 0);
          appelsBas : in std_logic_vector(NombreEtages - 1 downto 0);
          cabineEtage : in unsigned(NombreBitsEtages - 1 downto 0);
          positionCabine : in unsigned(NombreBitsPositionCabine - 1 downto 0);
          directionCabine : in std_logic);

    -- Sorties
    Port (commandeMoteur : out std_logic;
          selectionEtage : out unsigned(NombreBitsEtages - 1 downto 0);
          etageSelectionne : out std_logic_vector(NombreEtages - 1 downto 0);
          alarme : out std_logic);
end ControleurAscenseur;

-- Fichier d'architecture
architecture ArchitectureImpl of ControleurAscenseur is

    -- Constantes
    constant NombreEtages : integer := 10;
    constant NombreBitsEtages : integer := 4;
    constant NombreBitsPositionCabine : integer := 5;

    -- Variables d'état
    signal etageCible : unsigned(NombreBitsEtages - 1 downto 0);
    signal directionCible : std_logic;

    begin

        -- Processus de gestion des appels d'étage
        process(clk, reset)
        begin
            if reset = '1' then
                etageCible <= (others => '0');
                directionCible <= '0';
            else
                -- Détermine l'étage cible le plus proche
                etageCible <= MaxCalledFloor(appelsHaut, appelsBas, positionCabine, directionCabine);
                directionCible <= DirectionToEtage(etageCible, positionCabine, directionCabine);
            end if;
        end process;

        -- Processus de sélection d'étage
        process(clk, reset)
        begin
            if reset = '1' then
                selectionEtage <= (others => '0');
            else
                selectionEtage <= std_logic_vector(etageCible);
            end if;
        end process;

        -- Processus de commande moteur
        process(clk, reset)
        begin
            if reset = '1' then
                commandeMoteur <= '0';
            else
                case directionCible is
                    when '0' =>
                        commandeMoteur <= '1' when positionCabine < etageCible;
                    when '1' =>
                        commandeMoteur <= '1' when positionCabine > etageCible;
                    when others =>
                        commandeMoteur <= '0';
                end case;
            end if;
        end process;

        -- Processus d'alarme
        process(positionCabine, etageSelectionne)
        begin
            alarme <= positionCabine /= etageSelectionne;
        end process;

    end ArchitectureImpl;
```

**Explication du code:**

* Le module `ControleurAscenseur` est une entité qui attend plusieurs entrées et produit plusieurs sorties.
* L'architecture `ArchitectureImpl` implémente la logique du module.
* Le processus `GestionDesAppels` détermine l'étage cible et la direction en fonction des appels d'étage et de la position actuelle de la cabine.
* Le processus `SelectionDEtage` sélectionne l'étage cible pour l'affichage.
* Le processus `CommandeMoteur` commande le moteur de l'ascenseur en fonction de l'étage cible et de la direction.
* Le processus `Alarme` signale une alarme lorsque la cabine n'est pas à l'étage sélectionné.
* Les constantes, variables d'état et fonctions auxiliaires sont utilisées pour implémenter la logique.
* Le code utilise des types personnalisés (`unsigned`, `std_logic_vector`) pour représenter les valeurs multi-bits.
* Le code est divisé en plusieurs processus pour améliorer la clarté et la maintenabilité.