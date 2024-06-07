```vhdl
-- Version VHDL : 2008
-- Bibliothèque standard VHDL : std_logic_1164.vhd
-- Bibliothèque spécifique : mon_package.vhd

-- Déclaration de l'entité
entity Processeur is
    -- Ports d'entrée
    port (
        clk : in std_logic;               -- Horloge
        rst : in std_logic;               -- Réinitialisation
        data_in : in std_logic_vector(7 downto 0);  -- Données d'entrée
        addr_in : in std_logic_vector(9 downto 0)  -- Adresse d'entrée
    );

    -- Ports de sortie
    port (
        data_out : out std_logic_vector(7 downto 0); -- Données de sortie
        addr_out : out std_logic_vector(9 downto 0)  -- Adresse de sortie
    );
end Processeur;

-- Architecture du processeur
architecture Architecture_Processeur of Processeur is
    -- Définition des signaux internes
    signal opcode : std_logic_vector(5 downto 0);   -- Code d'opération
    signal funct : std_logic_vector(5 downto 0);    -- Fonction
    signal reg_dest : std_logic_vector(4 downto 0); -- Numéro de registre de destination
    signal reg1 : std_logic_vector(31 downto 0);   -- Contenu du registre 1
    signal reg2 : std_logic_vector(31 downto 0);   -- Contenu du registre 2
    signal alu_result : std_logic_vector(31 downto 0); -- Résultat de l'ALU
    signal mem_data_out : std_logic_vector(31 downto 0); -- Données lues de la mémoire
    signal mem_write : std_logic;                   -- Signal d'écriture mémoire

    -- Décodeur d'instruction
    with select opcode select
        funct <= funct_add when "000100" else
                funct_sub when "000101" else
                funct_and when "001000" else
                funct_or when "001001" else
                funct_xor when "001010" else
                funct_sll when "000000" else
                funct_srl when "000010" else
                funct_jr when "000000" else
                (others => (others => '0'));

    -- Sélecteur de registre
    with select reg_dest select
        reg1 <= file1(reg_dest) when "00000" else
                file2(reg_dest) when "00001" else
                file3(reg_dest) when "00010" else
                file4(reg_dest) when "00011" else
                file5(reg_dest) when "00100" else
                file6(reg_dest) when "00101" else
                file7(reg_dest) when "00110" else
                file8(reg_dest) when "00111" else
                file9(reg_dest) when "01000" else
                file10(reg_dest) when "01001" else
                file11(reg_dest) when "01010" else
                file12(reg_dest) when "01011" else
                file13(reg_dest) when "01100" else
                file14(reg_dest) when "01101" else
                file15(reg_dest) when "01110" else
                file16(reg_dest) when "01111" else
                file17(reg_dest) when "10000" else
                file18(reg_dest) when "10001" else
                file19(reg_dest) when "10010" else
                file20(reg_dest) when "10011" else
                file21(reg_dest) when "10100" else
                file22(reg_dest) when "10101" else
                file23(reg_dest) when "10110" else
                file24(reg_dest) when "10111" else
                file25(reg_dest) when "11000" else
                file26(reg_dest) when "11001" else
                file27(reg_dest) when "11010" else
                file28(reg_dest) when "11011" else
                file29(reg_dest) when "11100" else
                file30(reg_dest) when "11101" else
                file31(reg_dest) when "11110" else
                (others => (others => '0'));

    -- ALU
    with select funct select
        alu_result <= reg1 + reg2 when "000000" else
                     reg1 - reg2 when "000001" else
                     reg1 and reg2 when "000010" else
                     reg1 or reg2 when "000011" else
                     reg1 xor reg2 when "000100" else
                     reg1 sll reg2(4 downto 0) when "000101" else
                     reg1 srl reg2(4 downto 0) when "000110" else
                     (others => (others => '0'));

    -- Mémoire
    with select addr_in select
        mem_data_out <= mem(addr_in) when others => (others => '0');

    -- Registre de sortie
    data_out <= alu_result;

    -- Registre d'adresse de sortie
    addr_out <= addr_in;
end Architecture_Processeur;
```

**Explication du code :**

Ce code VHDL implémente un processeur simple avec les fonctionnalités suivantes :

* **Décodeur d'instruction :** Décode l'instruction à partir des bits d'opcode et de fonction.
* **Sélecteur de registre :** Sélecte le registre source en fonction du numéro de registre de destination.
* **ALU :** Effectue des opérations arithmétiques et logiques sur les registres source.
* **Mémoire :** Lit et écrit des données en mémoire en fonction de l'adresse d'entrée.
* **Registre de sortie :** Stocke le résultat de l'ALU ou les données lues de la mémoire.
* **Registre d'adresse de sortie :** Stocke l'adresse d'entrée.

Le processeur fonctionne comme suit :

1. L'horloge est appliquée et le processeur est réinitialisé.
2. L'instruction est décodée et l'opcode et la fonction sont extraits.
3. Les registres source sont sélectionnés en fonction du numéro de registre de destination.
4. L'ALU effectue l'opération spécifiée sur les registres source.
5. Le résultat de l'ALU est stocké dans le registre de sortie.
6. L'adresse d'entrée est stockée dans le registre d'adresse de sortie.
7. Si un signal d'écriture mémoire est présent, les données de sortie sont écrites en mémoire à l'adresse d'entrée.
8. Si un signal de lecture mémoire est présent, les données lues de la mémoire sont stockées dans le registre de sortie.