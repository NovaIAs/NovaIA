**Machine d'état finie pour un système de contrôle d'ascenseur**

```vhdl
-- Bibliothèque standard VHDL
library IEEE;
use IEEE.std_logic_1164.all;

-- Définition des constantes
constant NB_ETAGES : integer := 5;
constant ETAGE_MIN : integer := 1;
constant ETAGE_MAX : integer := NB_ETAGES;

-- Définition des types
type Etat is (Repos, Montant, Descendant);
type Requete is (AppelMontant, AppelDescendant, DemandeEtage);

-- Signal d'entrée et de sortie
signal Etat_actuel : Etat;
signal Etage_actuel : integer range ETAGE_MIN to ETAGE_MAX;
signal Requete_arrivee : Requete;
signal Appel_etage(ETAGE_MIN to ETAGE_MAX) : std_logic;
signal Demande_etage : integer range ETAGE_MIN to ETAGE_MAX;

-- Processus de la machine d'état
process (Etage_actuel, Requete_arrivee, Appel_etage, Demande_etage)
begin

    -- Initialisation de l'état actuel
    Etat_actuel <= Repos;

    -- Transition d'état en fonction des entrées
    case Etat_actuel is
        when Repos =>
            if Requete_arrivee = AppelMontant then
                Etat_actuel <= Montant;
            elsif Requete_arrivee = AppelDescendant then
                Etat_actuel <= Descendant;
            elsif Requete_arrivee = DemandeEtage then
                Etat_actuel <= Montant;
                Demande_etage <= Requete_arrivee;
            end if;

        when Montant =>
            if Etage_actuel = ETAGE_MAX or Appel_etage(Etage_actuel) = '0' then
                Etat_actuel <= Repos;
            else
                Etage_actuel <= Etage_actuel + 1;
            end if;

        when Descendant =>
            if Etage_actuel = ETAGE_MIN or Appel_etage(Etage_actuel) = '0' then
                Etat_actuel <= Repos;
            else
                Etage_actuel <= Etage_actuel - 1;
            end if;
    end case;

end process;

-- Fin de l'architecture
end architecture;
```

**Explication du code**

Ce code VHDL implémente une machine d'état fini pour un système de contrôle d'ascenseur. La machine d'état contrôle l'ascenseur en fonction des requêtes d'appel d'étage, des demandes d'étage et de l'étage actuel de l'ascenseur.

Le code se compose de plusieurs parties :

* **Constantes** : Définit les constantes utilisées dans le code, telles que le nombre d'étages, la plage d'étages et les différents états de la machine d'état.
* **Types** : Définit des types personnalisés pour les états de la machine d'état et les types de requêtes.
* **Signaux** : Définit les signaux d'entrée et de sortie de la machine d'état, tels que l'état actuel, l'étage actuel, les requêtes d'arrivée, les appels d'étage et les demandes d'étage.
* **Processus de la machine d'état** : Implémente la logique de la machine d'état, qui met à jour l'état actuel et l'étage actuel en fonction des entrées.

La machine d'état fonctionne comme suit :

* Initialement, la machine d'état est à l'état **Repos**.
* Lorsqu'une **requête d'arrivée** est reçue (AppelMontant, AppelDescendant ou DemandeEtage), la machine d'état passe à l'état **Montant**, **Descendant** ou **Montant**, respectivement.
* Si **l'ascenseur est à l'état Montant**, il monte jusqu'à ce qu'il atteigne l'étage le plus élevé (ETAGE_MAX) ou jusqu'à ce qu'il n'y ait plus d'appels d'étage.
* Si **l'ascenseur est à l'état Descendant**, il descend jusqu'à ce qu'il atteigne l'étage le plus bas (ETAGE_MIN) ou jusqu'à ce qu'il n'y ait plus d'appels d'étage.
* Une fois que **l'ascenseur arrive à l'étage demandé**, il passe à l'état **Repos**.