**Module principal : Système d'arbitrage de bus**

```verilog
module bus_arbitre(
    input clk,
    input reset,
    input [7:0] dems, // demandes de bus
    input [7:0] prio, // priorités des demandes
    output reg [2:0] grant, // attribution de bus
    output reg collision // collision sur le bus
);

    // Déclaration des registres
    reg [7:0] arb_reg; // registre d'arbitrage
    reg [7:0] arb_next; // registre d'arbitrage suivant l'état
    reg [2:0] grant_next; // attribution de bus suivante l'état

    // Circuit d'arbitrage
    always @(posedge clk) begin
        if (reset) begin
            arb_reg <= 8'b0;
            grant <= 3'b0;
            collision <= 1'b0;
        end else begin
            // Calculer le registre d'arbitrage suivant
            arb_next = dems & ~arb_reg;

            // Vérifier s'il y a une collision
            collision <= |arb_next;

            // Calculer l'attribution de bus suivante
            grant_next = collision ? 3'b0 : {2'b0, $argmax(arb_next)};

            // Mettre à jour le registre d'arbitrage et l'attribution de bus
            arb_reg <= arb_next;
            grant <= grant_next;
        end
    end

endmodule
```

**Module secondaire : Générateur de demande de bus**

```verilog
module bus_demandeur(
    input clk,
    input reset,
    input [2:0] id, // identifiant du demandeur
    input rdy, // signal de disponibilité du demandeur
    output reg dem, // demande de bus
    output reg prio // priorité de la demande
);

    // Déclaration des registres
    reg dem_reg; // registre de demande
    reg prio_reg; // registre de priorité

    // Circuit de génération de demande de bus
    always @(posedge clk) begin
        if (reset) begin
            dem_reg <= 1'b0;
            prio_reg <= 1'b0;
        end else begin
            // Calculer la demande et la priorité en fonction de l'état du demandeur
            if (rdy) begin
                dem_reg <= 1'b1;
                prio_reg <= $signed({2'b0, id});
            end else begin
                dem_reg <= 1'b0;
                prio_reg <= 1'b0;
            end
        end
    end

    // Sortir la demande et la priorité
    assign dem = dem_reg;
    assign prio = prio_reg;

endmodule
```

**Module de test**

```verilog
module bus_test;

    // Déclaration des instances
    bus_arbitre arb(
        .clk(clk),
        .reset(reset),
        .dems(dems),
        .prio(prio),
        .grant(grant),
        .collision(collision)
    );

    bus_demandeur[] dems(
        .clk(clk),
        .reset(reset),
        .id({3'b0, i}),
        .rdy(rdy[i]),
        .dem(dems[i]),
        .prio(prio[i])
    );

    // Déclaration des signaux
    reg clk;
    reg reset;
    reg [7:0] rdy;
    wire [7:0] dems;
    wire [7:0] prio;
    wire [2:0] grant;
    wire collision;

    // Initialisation
    initial begin
        clk = 0;
        reset = 1;
        #10;
        reset = 0;
    end

    // Génération d'horloge
    always #5 clk = !clk;

    // Génération des signaux aléatoires
    always @(posedge clk) begin
        for (i = 0; i < 8; i++) begin
            rdy[i] <= $random;
        end
    end

    // Affichage des signaux
    always @(posedge clk) begin
        $display("dems: %b, prio: %b, grant: %b, collision: %b", dems, prio, grant, collision);
    end

endmodule
```

**Explication du code**

Ce code implémente un système d'arbitrage de bus complexe en Verilog. Le module principal, `bus_arbitre`, est un arbitre qui attribue le bus à un demandeur en fonction de la demande et de la priorité. Le module secondaire, `bus_demandeur`, est un générateur de demande de bus qui génère une demande de bus en fonction de la disponibilité du demandeur. Le module de test, `bus_test`, instancie plusieurs demandeurs et l'arbitre, et génère des signaux aléatoires pour tester le système.

Le registre `arb_reg` dans le module `bus_arbitre` stocke les demandes de bus actuelles. Le circuit d'arbitrage calcule ensuite le registre d'arbitrage suivant, `arb_next`, en mettant à jour les bits correspondant aux demandes qui viennent de commencer. Le circuit vérifie également s'il y a une collision en vérifiant si le registre `arb_next` contient plus d'un bit à 1.

Le `grant_next` est calculé en prenant le plus grand bit à 1 dans le registre `arb_next`. Si une collision se produit, `grant_next` est mis à 0. Enfin, le registre d'arbitrage et l'attribution de bus sont mis à jour avec leurs valeurs respectives suivantes.

Le module `bus_demandeur` génère une demande de bus et une priorité en fonction de l'état du demandeur. Si le demandeur est prêt, il génère une demande de bus et une priorité élevée. Lorsque le demandeur n'est pas prêt, il ne génère pas de demande de bus et une faible priorité.

Le module de test instancie plusieurs demandeurs et l'arbitre, et génère des signaux aléatoires pour tester le système. Le signal `rdy` indique si le demandeur est prêt à accéder au bus. Le signal `dems` contient les demandes de bus de tous les demandeurs. Le signal `prio` contient les priorités des demandes de bus. Le signal `grant` contient l'attribution de bus de l'arbitre. Le signal `collision` indique s'il y a une collision sur le bus.