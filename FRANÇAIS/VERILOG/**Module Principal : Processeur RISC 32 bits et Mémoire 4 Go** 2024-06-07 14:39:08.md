**Module Principal : Processeur RISC 32 bits**

```verilog
// Module principal : Processeur RISC 32 bits
module processeur_risc32 (
    input wire clk,
    input wire reset,
    output wire [31:0] data_out,
    output wire [31:0] addr_out
);

// Registres
reg [31:0] registres [31:0];

// Compteurs de programme
reg [31:0] compteur_programme;

// Instruction
reg [31:0] instruction;

// Decodeur d'instructions
always @(posedge clk) begin
    if (reset) begin
        compteur_programme <= 0;
    end else begin
        compteur_programme <= compteur_programme + 4;
    end

    instruction <= memoire[compteur_programme];
end

// Exécution d'instructions
always @(posedge clk) begin
    if (reset) begin
        registres[0] <= 0;
    end else begin
        case (instruction[31:26])
            6'b000000: // ADD
                registres[instruction[25:21]] <= registres[instruction[20:16]] + registres[instruction[15:11]];
            6'b000001: // SUB
                registres[instruction[25:21]] <= registres[instruction[20:16]] - registres[instruction[15:11]];
            6'b000010: // AND
                registres[instruction[25:21]] <= registres[instruction[20:16]] & registres[instruction[15:11]];
            6'b000011: // OR
                registres[instruction[25:21]] <= registres[instruction[20:16]] | registres[instruction[15:11]];
            6'b000100: // XOR
                registres[instruction[25:21]] <= registres[instruction[20:16]] ^ registres[instruction[15:11]];
            6'b000101: // Shift à gauche
                registres[instruction[25:21]] <= registres[instruction[20:16]] << instruction[10:6];
            6'b000110: // Shift à droite
                registres[instruction[25:21]] <= registres[instruction[20:16]] >> instruction[10:6];
            6'b000111: // LW
                registres[instruction[25:21]] <= memoire[registres[instruction[20:16]] + instruction[15:0]];
            6'b001000: // SW
                memoire[registres[instruction[25:21]] + instruction[15:0]] <= registres[instruction[20:16]];
            6'b001001: // BEQ
                if (registres[instruction[25:21]] == 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001010: // BNE
                if (registres[instruction[25:21]] != 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001011: // BLT
                if (registres[instruction[25:21]] < 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001100: // BGT
                if (registres[instruction[25:21]] > 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001101: // BLE
                if (registres[instruction[25:21]] <= 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001110: // BGE
                if (registres[instruction[25:21]] >= 0) begin
                    compteur_programme <= compteur_programme + 4 + (instruction[15:0] << 2);
                end
            6'b001111: // JAL
                registres[31] <= compteur_programme + 4;
                compteur_programme <= compteur_programme + 4 + (instruction[25:0] << 2);
            6'b100000: // JR
                compteur_programme <= registres[instruction[25:21]];
            6'b100001: // JALR
                registres[31] <= compteur_programme + 4;
                compteur_programme <= registres[instruction[25:21]];
        endcase
    end
end

// Sorties
assign data_out = registres[instruction[25:21]];
assign addr_out = registres[instruction[20:16]];

endmodule
```

**Module Mémoire : Mémoire de 4 Go**

```verilog
// Module Mémoire : Mémoire de 4 Go
module memoire (
    input wire clk,
    input wire reset,
    input wire [31:0] addr,
    input wire [31:0] data_in,
    input wire write_en,
    output wire [31:0] data_out
);

// RAM de 4 Go
reg [31:0] memoire [0:262143];

// Écriture dans la mémoire
always @(posedge clk) begin
    if (write_en) begin
        memoire[addr[31:2]] <= data_in;
    end
end

// Lecture de la mémoire
assign data_out = memoire[addr[31:2]];

endmodule
```

**Exemple d'Utilisation : Programme de Calcul de Moyenne**

```verilog
// Programme de Calcul de Moyenne
module calcul_moyenne (
    input wire clk,
    input wire reset,
    input wire [31:0] data_in,
    output wire [31:0] data_out
);

// Registre de moyenne
reg [31:0] moyenne;

// Compteur d'échantillons
reg [31:0] compteur_echantillons;

// Calcul de la moyenne
always @(posedge clk) begin
    if (reset) begin
        moyenne <= 0;
        compteur_echantillons <= 0;
    end else begin
        moyenne <= (moyenne * compteur_echantillons + data_in) / (compteur_echantillons + 1);
        compteur_echantillons <= compteur_echantillons + 1;
    end
end

// Sortie
assign data_out = moyenne;

endmodule
```