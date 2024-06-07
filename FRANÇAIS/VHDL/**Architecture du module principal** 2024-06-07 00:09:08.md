**Module principal : Architecture**

```vhdl
-- Nom du module : processeur_principal
-- Description : Noyau d'un processeur 16 bits

entity processeur_principal is
    generic (
        -- Fréquence d'horloge en MHz
        horloge_MHz : positive;
        -- Nombre de registres de base
        nb_registres : positive;
        -- Nombre de bits par registre
        taille_registre : positive
    );
    port (
        -- Entrée : Horloge interne
        horloge : in std_logic;
        -- Entrée : Signal de réinitialisation
        reset : in std_logic;
        -- Entrée : Adresse de mémoire à lire
        adresse_memoire : in std_logic_vector(15 downto 0);
        -- Entrée : Données à écrire en mémoire
        donnees_memoire : in std_logic_vector(15 downto 0);
        -- Entrée : Signal d'écriture de mémoire
        ecriture_memoire : in std_logic;
        -- Sortie : Données lues de la mémoire
        donnees_memoire_lues : out std_logic_vector(15 downto 0);
        -- Entrée : Instruction à exécuter
        instruction : in std_logic_vector(15 downto 0);
        -- Entrée : Registre d'entrée pour les données
        registre_entree : in std_logic_vector(taille_registre - 1 downto 0);
        -- Sortie : Registre de sortie pour les données
        registre_sortie : out std_logic_vector(taille_registre - 1 downto 0)
    );
end processeur_principal;

architecture comportement of processeur_principal is
    -- Registres du processeur
    signal registres : std_logic_vector(nb_registres * taille_registre - 1 downto 0);
    -- Mémoire de programme (ROM)
    constant memoire_programme : std_logic_vector(15 downto 0) array (0 to 255) := (
        "0000000000000000", -- Opcode : NOP
        "0000000000000001", -- Opcode : ADD
        "0000000000000010", -- Opcode : SUB
        "0000000000000011", -- Opcode : AND
        "0000000000000100", -- Opcode : OR
        "0000000000000101", -- Opcode : XOR
        "0000000000000110", -- Opcode : NOT
        "0000000000000111", -- Opcode : MOV
        "0000000000001000", -- Opcode : LOAD
        "0000000000001001", -- Opcode : STORE
        "0000000000001010", -- Opcode : JUMP
        "0000000000001011", -- Opcode : JZ
        "0000000000001100", -- Opcode : JN
        "0000000000001101", -- Opcode : HALT
        "0000000000001110", -- Opcode : PUSH
        "0000000000001111"  -- Opcode : POP
    );
    -- Mémoire de données (RAM)
    signal memoire_donnees : std_logic_vector(15 downto 0) array (0 to 255);
    -- Pipeline d'instruction en 5 étapes
    signal pipeline : std_logic_vector(4 downto 0) array (4 downto 0) := ("00000", "00000", "00000", "00000", "00000");
    -- Registre d'instruction (IR)
    signal ir : std_logic_vector(15 downto 0);
    -- Compteur de programme (PC)
    signal pc : std_logic_vector(8 downto 0);
    -- Registre de flags
    signal flags : std_logic_vector(2 downto 0);
    -- Signal de transfert de données
    signal transfert_donnees : std_logic;

    begin
        -- Horloge
        process (horloge, reset) is
            begin
                if reset = '1' then
                    -- Réinitialisation à l'état initial
                    registres <= (others => '0');
                    memoire_donnees <= (others => '0');
                    pipeline <= ("00000", "00000", "00000", "00000", "00000");
                    ir <= "0000000000000000";
                    pc <= "000000000";
                    flags <= "000";
                else
                    -- Transfert des données
                    if transfert_donnees = '1' then
                        registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3))) <= registre_entree;
                    end if;
                    -- Pipeline d'instruction
                    pipeline(4) <= pipeline(3);
                    pipeline(3) <= pipeline(2);
                    pipeline(2) <= pipeline(1);
                    pipeline(1) <= pipeline(0);
                    pipeline(0) <= ir;
                    -- Compteur de programme
                    pc <= pc + 1;
                end if;
            end process;

        -- Décodage de l'instruction
        if pipeline(4) = "00000" then
            ir <= memoire_programme(std_logic_vector(pc));
        elsif pipeline(4) = "00001" then
            -- Opération arithmétique ou logique
            case ir(7 downto 5) is
                when "000" => -- ADD
                    registre_sortie <= registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3)));
                when "001" => -- SUB
                    registre_sortie <= registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3))) - registres(taille_registre * int(ir(11 downto 10)) + taille_registre - 1 downto taille_registre * int(ir(11 downto 10)));
                when "010" => -- AND
                    registre_sortie <= registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3))) and registres(taille_registre * int(ir(11 downto 10)) + taille_registre - 1 downto taille_registre * int(ir(11 downto 10)));
                when "011" => -- OR
                    registre_sortie <= registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3))) or registres(taille_registre * int(ir(11 downto 10)) + taille_registre - 1 downto taille_registre * int(ir(11 downto 10)));
                when "100" => -- XOR
                    registre_sortie <= registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3))) xor registres(taille_registre * int(ir(11 downto 10)) + taille_registre - 1 downto taille_registre * int(ir(11 downto 10)));
                when "101" => -- NOT
                    registre_sortie <= not registres(taille_registre * int(ir(4 downto 3)) + taille_registre - 1 downto taille_registre * int(ir(4 downto 3)));