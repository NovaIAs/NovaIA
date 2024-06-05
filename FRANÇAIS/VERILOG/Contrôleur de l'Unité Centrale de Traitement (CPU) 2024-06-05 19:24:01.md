```verilog
// Module de contrôle de l'unité de traitement centrale (CPU)

module ControleCPU(
    input clock,        // Horloge d'entrée
    input reset,        // Signal de réinitialisation
    
    // Entrées depuis l'unité de commande
    input [15:0] instruction,   // Code de l'instruction exécutée
    input [15:0] registreIR,    // Registre d'instruction (IR) contenant l'instruction actuelle
    input [15:0] registrePC,    // Registre de compteur de programme (PC) contenant l'adresse de l'instruction suivante
    input [15:0] registreA,     // Registre A contenant le premier opérande
    input [15:0] registreB,     // Registre B contenant le deuxième opérande
    
    // Sorties vers l'unité d'exécution
    output reg [2:0] opcode,     // Code d'opération à exécuter
    output reg [1:0] fonction,   // Fonction à exécuter (pour les instructions de type R)
    output reg [2:0] destination, // Registre de destination pour le résultat
    output reg [2:0] sourceA,    // Registre source pour le premier opérande
    output reg [2:0] sourceB,    // Registre source pour le deuxième opérande
    output reg [15:0] constante, // Constante immédiate pour les instructions de type I
    output reg [15:0] adresseBranche,  // Adresse de branche pour les instructions de branchement
    
    // Sorties vers l'unité de mémoire
    output reg [15:0] adresseMemoire, // Adresse de mémoire pour les accès mémoire
    output reg [15:0] donneeMemoire,  // Données à écrire dans la mémoire
    output reg memEcriture,    // Signal d'écriture en mémoire
    
    // Sorties de contrôle
    output reg pcEcriture,    // Signal d'écriture dans le registre PC
    output reg regEcriture,   // Signal d'écriture dans les registres
    output reg finInstruction, // Signal indiquant la fin de l'exécution de l'instruction
    output reg [15:0] nextPC     // Prochain registre PC à charger
);

// Signaux internes
reg [2:0] opcodeInterne;
reg [1:0] fonctionInterne;
reg [2:0] registreDestinationInterne;
reg [2:0] registreSourceAInterne;
reg [2:0] registreSourceBInterne;
reg [15:0] constanteInterne;
reg [15:0] adresseBrancheInterne;
reg [15:0] adresseMemoireInterne;
reg [15:0] donneeMemoireInterne;
reg memEcritureInterne;
reg pcEcritureInterne;
reg regEcritureInterne;
reg finInstructionInterne;
reg [15:0] nextPCInterne;

// Décodeur d'instruction
always @(instruction, registreIR, registrePC) begin
    case (instruction)
        16'h0000: begin // Instruction ADD
            opcodeInterne = 3'b000;
            fonctionInterne = 2'b10;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = 16'b0;
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = 16'b0;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h1000: begin // Instruction SUB
            opcodeInterne = 3'b000;
            fonctionInterne = 2'b10;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = 16'b0;
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = 16'b0;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h2000: begin // Instruction AND
            opcodeInterne = 3'b000;
            fonctionInterne = 2'b00;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = 16'b0;
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = 16'b0;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h3000: begin // Instruction OR
            opcodeInterne = 3'b000;
            fonctionInterne = 2'b01;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = 16'b0;
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = 16'b0;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h4000: begin // Instruction XOR
            opcodeInterne = 3'b000;
            fonctionInterne = 2'b11;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = 16'b0;
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = 16'b0;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h5000: begin // Instruction LW
            opcodeInterne = 3'b001;
            fonctionInterne = 2'b00;
            registreDestinationInterne = registreIR[11:9];
            registreSourceAInterne = registreIR[8:6];
            registreSourceBInterne = registreIR[5:3];
            constanteInterne = registreIR[15:6];
            adresseBrancheInterne = 16'b0;
            adresseMemoireInterne = registreA + constanteInterne;
            donneeMemoireInterne = 16'b0;
            memEcritureInterne = 0;
            pcEcritureInterne = 0;
            regEcritureInterne = 1;
            finInstructionInterne = 1;
            nextPCInterne = registrePC + 16'd1;
        end
        16'h6000: begin // Instruction SW
            opcodeInterne = 3'b010;
            fonctionInterne = 2'b00;
            registreDestinationInterne = registreIR[11:9];
            regist