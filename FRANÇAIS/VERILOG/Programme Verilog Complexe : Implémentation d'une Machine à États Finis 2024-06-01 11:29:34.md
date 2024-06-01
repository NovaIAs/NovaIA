**Code Verilog Complexe**

**Entrée :**

```verilog
// Unité de contrôle pour une machine à états finis complexe
input wire [7:0] state;
input wire clk;
input wire [31:0] data_in;

// Sorties
output reg [31:0] data_out;
output reg [7:0] next_state;
```

**Processus séquentiels :**

```verilog
// Registre de données de sortie
always @(posedge clk) begin
    case (state)
        8'b00000001: data_out <= data_in;
        8'b00000010: data_out <= data_out + 1;
        8'b00000011: data_out <= data_out << 1;
        8'b00000100: data_out <= data_out >> 1;
        default: data_out <= 32'b0;
    endcase
end

// Registre d'état suivant
always @(posedge clk) begin
    case (state)
        8'b00000001: next_state <= 8'b00000010;
        8'b00000010: next_state <= data_in[0] ? 8'b00000100 : 8'b00000011;
        8'b00000011: next_state <= 8'b00000101;
        8'b00000100: next_state <= 8'b00000101;
        8'b00000101: next_state <= 8'b00000110;
        8'b00000110: next_state <= 8'b00000111;
        8'b00000111: next_state <= 8'b00001000;
        8'b00001000: next_state <= 8'b00001001;
        default: next_state <= 8'b00000001;
    endcase
end
```

**Explication :**

* **Entrée :**
    * `state` : État actuel de la machine à états finis.
    * `clk` : Signal d'horloge.
    * `data_in` : Données d'entrée.
* **Sorties :**
    * `data_out` : Données de sortie.
    * `next_state` : État suivant de la machine à états finis.

* **Processus séquentiels :**
    * **`data_out` :**
        * Modifie les données de sortie en fonction de l'état actuel.
    * **`next_state` :**
        * Détermine l'état suivant de la machine à états finis en fonction de l'état actuel et des données d'entrée.

La machine à états finis effectue les opérations suivantes :

* **État 1 :** Initialisation
* **État 2 :** Ajoute 1 à `data_out`
* **État 3 :** Décale `data_out` à gauche
* **État 4 :** Décale `data_out` à droite
* **États 5-7 :** États intermédiaires
* **État 8 :** Retour à l'état initial