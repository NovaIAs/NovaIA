**Module de contrôle d'un processeur RISC-V 32 bits**

```verilog
// Module de contrôle du processeur RISC-V 32 bits
module Controleur(
    input   clk,
    input   rst,

    // Entrées instruction
    input   [31:0] instruction,

    // Sorties vers l'unité de traitement
    output  [4:0]  rd_adresse_registre_destination,
    output  [4:0]  rs1_adresse_registre_source_1,
    output  [4:0]  rs2_adresse_registre_source_2,
    output  [3:0]  opcode_fonction,
    output          branchement,
    output          ecriture_registre,
    output          ecriture_memoire_donnees,
    output          lecture_memoire_donnees,
    output          ecriture_memoire_instructions,
    output          lecture_memoire_instructions,

    // Sorties vers l'unité de génération d'adresses
    output [31:0] adresse_destination_branchement,
    output [31:0] adresse_saut_inconditionnel,
    output [31:0] adresse_source_chargement
);

// États du processeur
parameter etat_lecture_instruction = 0;
parameter etat_lecture_donnees = 1;
parameter etat_lecture_donnees_branchement = 2;
parameter etat_traitement_instruction = 3;
parameter etat_ecriture_registre = 4;
parameter etat_ecriture_memoire = 5;

// Registres d'état
reg [3:0] etat_actuel;
reg [3:0] etat_suivant;

// Signaux combinatoires
assign rd_adresse_registre_destination = instruction[11:7];
assign rs1_adresse_registre_source_1 = instruction[19:15];
assign rs2_adresse_registre_source_2 = instruction[24:20];
assign opcode_fonction = instruction[6:0];
assign branchement = (opcode_fonction == 4'b11000) || (opcode_fonction == 4'b11011);
assign ecriture_registre = (opcode_fonction[3:0] == 4'b0110);
assign ecriture_memoire_donnees = (opcode_fonction == 4'b0100);
assign lecture_memoire_donnees = (opcode_fonction == 4'b0000);
assign ecriture_memoire_instructions = (opcode_fonction == 4'b11001);
assign lecture_memoire_instructions = (opcode_fonction == 4'b11000);

// Calcul de l'adresse de destination du branchement
assign adresse_destination_branchement = {instruction[31], instruction[7], instruction[30:25], instruction[11:8], 1'b0};

// Calcul de l'adresse de saut inconditionnel
assign adresse_saut_inconditionnel = {instruction[31:12], 12'b0};

// Calcul de l'adresse de chargement de données
assign adresse_source_chargement = rs1_adresse_registre_source_1;

// Machine d'états
always @(posedge clk, posedge rst) begin
    if (rst) begin
        etat_actuel <= etat_lecture_instruction;
    end else begin
        etat_actuel <= etat_suivant;
    end
end

always @(*) begin
    case (etat_actuel)
        etat_lecture_instruction: begin
            if (lecture_memoire_instructions) begin
                etat_suivant = etat_lecture_donnees;
            else if (lecture_memoire_donnees) begin
                etat_suivant = etat_lecture_donnees_branchement;
            else begin
                etat_suivant = etat_traitement_instruction;
            end
        end
        etat_lecture_donnees: begin
            if (branchement) begin
                etat_suivant = etat_lecture_donnees_branchement;
            else begin
                etat_suivant = etat_traitement_instruction;
            end
        end
        etat_lecture_donnees_branchement: begin
            etat_suivant = etat_traitement_instruction;
        end
        etat_traitement_instruction: begin
            if (ecriture_registre) begin
                etat_suivant = etat_ecriture_registre;
            else if (ecriture_memoire_donnees) begin
                etat_suivant = etat_ecriture_memoire;
            end else begin
                etat_suivant = etat_lecture_instruction;
            end
        end
        etat_ecriture_registre: begin
            etat_suivant = etat_lecture_instruction;
        end
        etat_ecriture_memoire: begin
            etat_suivant = etat_lecture_instruction;
        end
        default: begin
            etat_suivant = etat_lecture_instruction;
        end
    endcase
end

endmodule
```

**Explication du code**

Ce code Verilog implémente le module de contrôle d'un processeur RISC-V 32 bits. Le module de contrôle est responsable de décoder les instructions et de générer les signaux de contrôle appropriés pour les autres composants du processeur.

**Structure du code**

Le code est structuré en plusieurs sections :

* La section des paramètres définit les états du processeur et les constantes utilisées dans le code.
* La section des entrées et des sorties définit les ports d'entrée et de sortie du module.
* La section des signaux combinatoires définit les signaux combinatoires qui sont calculés à partir des entrées du module.
* La section de la machine d'états définit la machine d'états qui contrôle le comportement du module.

**Fonctionnement du module de contrôle**

Le module de contrôle fonctionne comme suit :

1. **Lecture de l'instruction** : Dans l'état de lecture de l'instruction, le module de contrôle lit l'instruction suivante de la mémoire.
2. **Lecture des données** : Si l'instruction nécessite la lecture de données de la mémoire, le module de contrôle passe à l'état de lecture des données.
3. **Calcul de l'adresse de branchement** : Si l'instruction est une instruction de branchement, le module de contrôle calcule l'adresse de destination du branchement.
4. **Traitement de l'instruction** : Dans l'état de traitement de l'instruction, le module de contrôle décode l'instruction et génère les signaux de contrôle appropriés pour les autres composants du processeur.
5. **Écriture du registre** : Si l'instruction nécessite l'écriture d'un registre, le module de contrôle passe à l'état d'écriture du registre.
6. **Écriture de la mémoire** : Si l'instruction nécessite l'écriture de données dans la mémoire, le module de contrôle passe à l'état d'écriture de la mémoire.

Le module de contrôle répète ces étapes pour chaque instruction exécutée par le processeur.