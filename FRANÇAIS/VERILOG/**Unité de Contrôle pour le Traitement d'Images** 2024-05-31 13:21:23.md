**Unité de Contrôle de Traitement d'Image**

```verilog
// Déclaration des constantes
`define WIDTH 1024 // Largeur de l'image
`define HEIGHT 512 // Hauteur de l'image
`define PIXEL_DEPTH 8 // Profondeur des pixels

// Définition de l'énumération des états
`define IDLE 0
`define READ 1
`define PROCESS 2
`define WRITE 3

// Définition du module
module ImageProcessingUnit (
    input clk, // Horloge
    input rst, // Réinitialisation
    input [7:0] pixel_in, // Pixel d'entrée
    output reg [7:0] pixel_out, // Pixel de sortie
    output reg done // Indicateur de fin de traitement
);

// Registres d'état
reg [`STATE_WIDTH-1:0] state; // État actuel
reg [`STATE_WIDTH-1:0] next_state; // État suivant

// Registres de données
reg [`WIDTH-1:0] image_buffer [`HEIGHT-1:0]; // Tampon d'image
reg [7:0] filter_kernel [3:0][3:0]; // Noyau de filtrage
reg [7:0] processed_pixel; // Pixel traité

// Logique combinatoire
// Calcul de l'état suivant en fonction de l'état actuel et des entrées
always @(*) begin
    case (state)
        `IDLE: begin
            if (rst) begin
                next_state = `IDLE;
            end else begin
                next_state = `READ;
            end
        end
        `READ: begin
            next_state = `PROCESS;
        end
        `PROCESS: begin
            next_state = `WRITE;
        end
        `WRITE: begin
            next_state = `IDLE;
        end
        default: begin
            next_state = `IDLE;
        end
    endcase
end

// Calcul du pixel traité
always @(*) begin
    processed_pixel = 0;
    for (int i = 0; i < 4; i++) begin
        for (int j = 0; j < 4; j++) begin
            processed_pixel += image_buffer[i][j] * filter_kernel[i][j];
        end
    end
end

// Logique séquentielle
// Mise à jour des registres d'état et de données
always @(posedge clk) begin
    if (rst) begin
        state <= `IDLE;
        done <= 0;
    end else begin
        state <= next_state;
    end

    case (state)
        `READ: begin
            // Chargement de l'image dans le tampon
            for (int i = 0; i < `HEIGHT; i++) begin
                for (int j = 0; j < `WIDTH; j++) begin
                    image_buffer[i][j] <= pixel_in;
                end
            end
        end
        `PROCESS: begin
            // Application du filtrage sur l'image tamponnée
            for (int i = 0; i < `HEIGHT; i++) begin
                for (int j = 0; j < `WIDTH; j++) begin
                    image_buffer[i][j] <= processed_pixel;
                end
            end
        end
        `WRITE: begin
            // Écriture de l'image traitée vers la sortie
            pixel_out <= image_buffer[0][0];
            done <= 1;
        end
        default: begin
            // Do nothing
        end
    endcase
end

endmodule
```

**Explications**

Ce code implémente une unité de contrôle pour le traitement d'image. Elle effectue les étapes suivantes :

1. **Lecture** de l'image d'entrée dans un tampon.
2. **Traitement** de l'image en appliquant un filtrage convolutif.
3. **Écriture** de l'image traitée vers la sortie.

L'unité de contrôle est implémentée en utilisant une machine à états finis (FSM). La FSM a quatre états : `IDLE`, `READ`, `PROCESS` et `WRITE`. L'état actuel de la FSM est stocké dans le registre `state`. Le registre `next_state` contient l'état suivant de la FSM, qui est déterminé par l'état actuel et les entrées.

Les registres de données sont utilisés pour stocker le tampon d'image, le noyau de filtrage et le pixel traité.

Le circuit fonctionne comme suit :

1. Lorsque le signal `clk` est activé, la FSM passe à l'état `IDLE`. Dans cet état, l'unité de contrôle attend que le signal `rst` soit activé.
2. Lorsque le signal `rst` est activé, la FSM passe à l'état `READ`. Dans cet état, l'unité de contrôle lit les pixels d'entrée et les stocke dans le tampon d'image.
3. Une fois que l'image est entièrement lue, l'unité de contrôle passe à l'état `PROCESS`. Dans cet état, l'unité de contrôle applique le filtrage sur les pixels de l'image stockée dans le tampon.
4. Une fois que l'image est entièrement traitée, l'unité de contrôle passe à l'état `WRITE`. Dans cet état, l'unité de contrôle écrit les pixels traités vers la sortie.
5. Une fois que l'image est entièrement écrite, l'unité de contrôle passe à l'état `IDLE` et définit le signal `done` à 1 pour indiquer que le traitement est terminé.