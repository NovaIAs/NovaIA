**Module haut niveau : Système de traitement d'image**

```verilog
module SystèmeTraitementImage(
    input clk,
    input rst,

    // Entrées
    input [7:0] pixel_in,
    input [2:0] canal,

    // Sorties
    output [7:0] pixel_out
);

    // Déclaration des signaux internes
    reg [7:0] pixel_intermédiaire;

    // Instanciation des modules subordonnés
    FiltreMédiane filtre_médian(
        .clk(clk),
        .rst(rst),
        .pixel_in(pixel_in),
        .pixel_intermédiaire(pixel_intermédiaire)
    );

    RéglageContraste réglage_contraste(
        .clk(clk),
        .rst(rst),
        .pixel_intermédiaire(pixel_intermédiaire),
        .canal(canal),
        .pixel_out(pixel_out)
    );

endmodule
```

**Module subordonné 1 : Filtre médiane**

```verilog
module FiltreMédiane(
    input clk,
    input rst,
    input [7:0] pixel_in,

    output [7:0] pixel_intermédiaire
);

    // Déclaration des signaux internes
    reg [7:0] registre_pixels[2:0];
    reg [1:0] index_registre;

    // Processus de filtrage
    always @(posedge clk) begin
        if (rst) begin
            // Réinitialisation
            index_registre <= 0;
            registre_pixels[0] <= 0;
            registre_pixels[1] <= 0;
            registre_pixels[2] <= 0;
        end else begin
            // Enregistrement du nouveau pixel
            registre_pixels[index_registre] <= pixel_in;

            // Incrémentation de l'index du registre
            index_registre <= index_registre + 1;

            // Calcul de la médiane
            pixel_intermédiaire <= (registre_pixels[0] + registre_pixels[1] + registre_pixels[2]) / 3;
        end
    end

endmodule
```

**Module subordonné 2 : Réglage du contraste**

```verilog
module RéglageContraste(
    input clk,
    input rst,
    input [7:0] pixel_intermédiaire,
    input [2:0] canal,

    output [7:0] pixel_out
);

    // Déclaration des signaux internes
    reg [7:0] table_contraste[31:0];

    // Processus de calcul du contraste
    always @(posedge clk) begin
        if (rst) begin
            // Réinitialisation de la table de contraste
            for (i = 0; i < 32; i++) begin
                table_contraste[i] <= i * 255 / 31;
            end
        end else begin
            // Calcul du pixel de sortie
            pixel_out <= table_contraste[pixel_intermédiaire] >> canal;
        end
    end

endmodule
```