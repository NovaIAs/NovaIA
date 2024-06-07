```verilog
// Module de top-niveau d'un système de traitement de signaux numériques (DSP)
module dsp_top(
    // Entrées
    input clk,
    input rst,
    input [15:0] data_in,

    // Sorties
    output [15:0] data_out
);

// Registres de pipeline
reg [15:0] pipeline_reg1;
reg [15:0] pipeline_reg2;

// Filtre FIR à réponse impulsionnelle finie
wire [15:0] fir_out;
fir filter(.clk(clk), .rst(rst), .data_in(data_in), .data_out(fir_out));

// Décimateur
wire [15:0] decimator_out;
decimator decim(.clk(clk), .rst(rst), .data_in(fir_out), .data_out(decimator_out));

// Moyenneur glissant
wire [15:0] average_out;
average aver(.clk(clk), .rst(rst), .data_in(decimator_out), .data_out(average_out));

// Assignation de la sortie
assign data_out = average_out;

// Implémentation du filtre FIR
module fir(
    input clk,
    input rst,
    input [15:0] data_in,
    output [15:0] data_out
);

// Coefficients du filtre
parameter [15:0] COEFFS[0:7] = {
    16'h0001, 16'h0002, 16'h0004, 16'h0008,
    16'h0010, 16'h0020, 16'h0040, 16'h0080
};

// Registres de décalage
reg [15:0] shift_reg[0:7];

// Calcul de la sortie
always @(posedge clk) begin
    if (rst) begin
        for (int i = 0; i < 8; i++)
            shift_reg[i] <= 0;
    end else begin
        for (int i = 0; i < 7; i++)
            shift_reg[i] <= shift_reg[i+1];
        shift_reg[7] <= data_in;
    end

    data_out <= COEFFS[0] * shift_reg[0] +
                COEFFS[1] * shift_reg[1] +
                COEFFS[2] * shift_reg[2] +
                COEFFS[3] * shift_reg[3] +
                COEFFS[4] * shift_reg[4] +
                COEFFS[5] * shift_reg[5] +
                COEFFS[6] * shift_reg[6] +
                COEFFS[7] * shift_reg[7];
end

// Implémentation du décimateur
module decimator(
    input clk,
    input rst,
    input [15:0] data_in,
    output [15:0] data_out
);

// Compteur de décimation
reg [7:0] decim_cnt;

// Registre de sortie
reg [15:0] decim_reg;

// Calcul de la sortie
always @(posedge clk) begin
    if (rst) begin
        decim_cnt <= 0;
        decim_reg <= 0;
    end else begin
        if (decim_cnt == 8'd255) begin
            decim_cnt <= 0;
            decim_reg <= data_in;
        end else
            decim_cnt <= decim_cnt + 1;
    end

    data_out <= decim_reg;
end

// Implémentation du moyenneur glissant
module average(
    input clk,
    input rst,
    input [15:0] data_in,
    output [15:0] data_out
);

// Registre sommateur
reg [31:0] sum_reg;

// Compteur d'échantillons
reg [15:0] sample_cnt;

// Calcul de la sortie
always @(posedge clk) begin
    if (rst) begin
        sum_reg <= 0;
        sample_cnt <= 0;
    end else begin
        sum_reg <= sum_reg + data_in;
        sample_cnt <= sample_cnt + 1;
    end

    data_out <= sum_reg / sample_cnt;
end
```

**Explication du code :**

Ce code met en œuvre un système DSP complexe qui effectue les opérations suivantes :

* Filtrage FIR : Un filtre à réponse impulsionnelle finie est utilisé pour supprimer le bruit et améliorer le rapport signal/bruit.
* Décimation : Le taux d'échantillonnage est réduit pour réduire la quantité de données à traiter.
* Moyennage glissant : Une moyenne glissante est calculée pour lisser les données et réduire la variance.

Le module de top-niveau, `dsp_top`, instancie des modules pour chacun de ces composants et connecte leurs entrées et sorties.

Le filtre FIR est implémenté à l'aide d'un filtre à coefficients fixes avec 8 coefficients. Le filtre utilise une architecture de décalage pour calculer la sortie filtrée.

Le décimateur réduit le taux d'échantillonnage par un facteur de 256 à l'aide d'un compteur.

Le moyenneur glissant utilise un sommateur pour cumuler les échantillons et un compteur pour suivre le nombre d'échantillons. La sortie est calculée en divisant la somme par le nombre d'échantillons.

Ce code est complexe et il est peu probable qu'il soit répété exactement tel quel, car il est spécifique à une application particulière. Cependant, il illustre les concepts de conception DSP et peut servir de modèle pour des systèmes DSP plus complexes.