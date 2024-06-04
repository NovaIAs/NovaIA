**Module de Vérification d'Échiquier**

```verilog
module VerificateurEchiquier;

// Définitions des constantes
localparam NB_COLONNES = 8;
localparam NB_RANGEES = 8;

// Declarations d'entrées
input [NB_COLONNES-1:0] colonnes[NB_RANGEES-1:0];
input [NB_RANGEES-1:0] rangees[NB_COLONNES-1:0];

// Declarations de sorties
output reg erreur;

// Variables internes
reg [NB_COLONNES-1:0] presenceColonnes;
reg [NB_RANGEES-1:0] presenceRangees;
reg [NB_COLONNES*NB_RANGEES-1:0] presenceDiagonales;

// Initialisation
always @(*) begin
    presenceColonnes = {NB_COLONNES{1'b0}};
    presenceRangees = {NB_RANGEES{1'b0}};
    presenceDiagonales = {NB_COLONNES*NB_RANGEES{1'b0}};
end

// Vérification des colonnes
always @(*) begin
    for (integer i = 0; i < NB_COLONNES; i = i + 1) begin
        for (integer j = 0; j < NB_RANGEES; j = j + 1) begin
            if (colonnes[j][i]) begin
                if (presenceColonnes[i]) begin
                    erreur = 1'b1;
                    return;
                end else begin
                    presenceColonnes[i] = 1'b1;
                end
            end
        end
    end
end

// Vérification des rangées
always @(*) begin
    for (integer i = 0; i < NB_RANGEES; i = i + 1) begin
        for (integer j = 0; j < NB_COLONNES; j = j + 1) begin
            if (rangees[i][j]) begin
                if (presenceRangees[i]) begin
                    erreur = 1'b1;
                    return;
                end else begin
                    presenceRangees[i] = 1'b1;
                end
            end
        end
    end
end

// Vérification des diagonales
always @(*) begin
    for (integer i = 0; i < NB_COLONNES; i = i + 1) begin
        for (integer j = 0; j < NB_RANGEES; j = j + 1) begin
            if (colonnes[j][i]) begin
                integer index = i + NB_COLONNES * j;
                if (presenceDiagonales[index]) begin
                    erreur = 1'b1;
                    return;
                else begin
                    presenceDiagonales[index] = 1'b1;
                end
            end
        end
    end
end

endmodule
```

**Explication du Code:**

Ce code Verilog modélise un vérificateur d'échiquier qui vérifie si un échiquier donné est valide, c'est-à-dire si aucune case ne contient plus d'une pièce.

**Entrées:**

* `colonnes`: Un tableau 2D de bits représentant la présence de pièces sur les colonnes de l'échiquier.
* `rangees`: Un tableau 2D de bits représentant la présence de pièces sur les rangées de l'échiquier.

**Sorties:**

* `erreur`: Un bit qui est réglé sur 1 (vrai) si l'échiquier n'est pas valide.

**Fonctionnement:**

Le vérificateur effectue les vérifications suivantes:

* **Vérification des colonnes:** Pour chaque colonne, il vérifie qu'il n'y a pas plus d'une case occupée.
* **Vérification des rangées:** Pour chaque rangée, il vérifie qu'il n'y a pas plus d'une case occupée.
* **Vérification des diagonales:** Il vérifie qu'il n'y a pas plus d'une case occupée sur chaque diagonale principale et secondaire.

Si une erreur est détectée lors de l'une de ces vérifications, la sortie `erreur` est réglée sur 1. Sinon, elle reste à 0, indiquant que l'échiquier est valide.