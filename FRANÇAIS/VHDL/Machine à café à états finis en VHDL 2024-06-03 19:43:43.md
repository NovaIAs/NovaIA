**Circuit à états finis (CEF) implémentant une machine à café**

```vhdl
-- Architecture de la machine à café
architecture MACHINE_À_CAFÉ of CEF is

-- États de la machine
constant ETAT_ATTENTE : integer := 0;
constant ETAT_BROYAGE : integer := 1;
constant ETAT_INFUSION : integer := 2;
constant ETAT_FIN : integer := 3;

-- Entrées
signal ENTREE_BOUTON_DE_DEMARRAGE : std_logic := '0';
signal ENTREE_NIVEAU_D_EAU : std_logic := '0';
signal ENTREE_NIVEAU_DE_CAFÉ : std_logic := '0';

-- Sorties
signal SORTIE_BROYEUR : std_logic := '0';
signal SORTIE_INFUSEUR : std_logic := '0';
signal SORTIE_GOBELET_PLEIN : std_logic := '0';

-- Registres d'état
signal ETAT_ACTUEL : integer := ETAT_ATTENTE;
signal ETAT_SUIVANT : integer := ETAT_ATTENTE;

begin

-- Transition d'état
process(ENTREE_BOUTON_DE_DEMARRAGE, ENTREE_NIVEAU_D_EAU, ENTREE_NIVEAU_DE_CAFÉ, ETAT_ACTUEL)
begin
    case ETAT_ACTUEL is
        when ETAT_ATTENTE =>
            if ENTREE_BOUTON_DE_DEMARRAGE = '1' and ENTREE_NIVEAU_D_EAU = '1' and ENTREE_NIVEAU_DE_CAFÉ = '1' then
                ETAT_SUIVANT <= ETAT_BROYAGE;
            end if;
        when ETAT_BROYAGE =>
            if ENTREE_NIVEAU_D_EAU = '1' and ENTREE_NIVEAU_DE_CAFÉ = '1' then
                ETAT_SUIVANT <= ETAT_INFUSION;
            else
                ETAT_SUIVANT <= ETAT_ATTENTE;
            end if;
        when ETAT_INFUSION =>
            if ENTREE_NIVEAU_D_EAU = '1' and ENTREE_NIVEAU_DE_CAFÉ = '1' then
                ETAT_SUIVANT <= ETAT_FIN;
            else
                ETAT_SUIVANT <= ETAT_ATTENTE;
            end if;
        when ETAT_FIN =>
            ETAT_SUIVANT <= ETAT_ATTENTE;
    end case;
end process;

-- Sorties
SORTIE_BROYEUR <= (ETAT_ACTUEL = ETAT_BROYAGE);
SORTIE_INFUSEUR <= (ETAT_ACTUEL = ETAT_INFUSION);
SORTIE_GOBELET_PLEIN <= (ETAT_ACTUEL = ETAT_FIN);

-- Mise à jour de l'état
ETAT_ACTUEL <= ETAT_SUIVANT;

end MACHINE_À_CAFÉ;
```

**Explications du code**

Ce code met en œuvre une machine à café à l'aide d'un CEF en VHDL. La machine a les états suivants :

* **ETAT_ATTENTE** : la machine attend qu'on appuie sur le bouton de démarrage et que les niveaux d'eau et de café soient suffisants.
* **ETAT_BROYAGE** : la machine broie le café.
* **ETAT_INFUSION** : la machine infuse le café.
* **ETAT_FIN** : la machine a terminé le processus et un gobelet de café est prêt.

Les entrées du CEF sont :

* **ENTREE_BOUTON_DE_DEMARRAGE** : indique si le bouton de démarrage a été appuyé.
* **ENTREE_NIVEAU_D_EAU** : indique si le niveau d'eau est suffisant.
* **ENTREE_NIVEAU_DE_CAFÉ** : indique si le niveau de café est suffisant.

Les sorties du CEF sont :

* **SORTIE_BROYEUR** : active le broyeur.
* **SORTIE_INFUSEUR** : active l'infuseur.
* **SORTIE_GOBELET_PLEIN** : indique qu'un gobelet de café est prêt.

Le CEF utilise un processus pour effectuer la transition d'état. Le processus évalue l'état actuel et les entrées et détermine l'état suivant.

Les sorties sont mises à jour en fonction de l'état actuel.

L'état actuel est mis à jour à chaque cycle d'horloge avec l'état suivant.