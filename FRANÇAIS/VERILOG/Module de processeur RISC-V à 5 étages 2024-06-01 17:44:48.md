**Module pour implémenter un processeur RISC-V à 5 étages**

```verilog
module processeur_RISC_V_5etages (
    input clock,
    input resetn,

    // Mémoire instructions
    output [31:0] addr_i,
    input [31:0] data_i,

    // Mémoire données
    output [31:0] addr_d,
    output [31:0] data_d,
    input [31:0] data_d_in,

    // E/S
    output [31:0] uart_txd,
    input [31:0] uart_rxd,

    // Trace
    output [31:0] trace_reg_1
);

    // Registres du processeur
    reg [31:0] registres [0:31];

    // Registres d'état
    reg [31:0] pc;
    reg [31:0] sp;
    reg [31:0] a0;
    reg [31:0] a1;
    reg [31:0] a2;
    reg [31:0] a3;
    reg [31:0] a4;
    reg [31:0] a5;
    reg [31:0] a6;
    reg [31:0] a7;
    reg [31:0] s0;
    reg [31:0] s1;
    reg [31:0] s2;
    reg [31:0] s3;
    reg [31:0] s4;
    reg [31:0] s5;
    reg [31:0] s6;
    reg [31:0] s7;
    reg [31:0] s8;
    reg [31:0] s9;
    reg [31:0] s10;
    reg [31:0] s11;
    reg [31:0] s12;
    reg [31:0] s13;
    reg [31:0] s14;
    reg [31:0] s15;
    reg [31:0] s16;
    reg [31:0] s17;
    reg [31:0] s18;
    reg [31:0] s19;
    reg [31:0] s20;
    reg [31:0] s21;
    reg [31:0] s22;
    reg [31:0] s23;
    reg [31:0] s24;
    reg [31:0] s25;
    reg [31:0] s26;
    reg [31:0] s27;
    reg [31:0] s28;
    reg [31:0] s29;
    reg [31:0] s30;
    reg [31:0] s31;

    // Pipeline d'instructions
    wire [31:0] pc_ex;
    wire [31:0] pc_mem;
    wire [31:0] pc_wb;

    // Pipeline de données
    wire [31:0] alu_data;
    wire [31:0] data_mem;
    wire [31:0] data_wb;

    // Pipeline de contrôle
    wire [2:0] decode;
    wire [2:0] execute;
    wire [2:0] memory;
    wire [2:0] writeback;

    // Unités de contrôle
    pipeline_control controle (
        .resetn(resetn),
        .decode(decode),
        .execute(execute),
        .memory(memory),
        .writeback(writeback)
    );

    // Unité de récupération d'instruction
    instruction_fetch instruction_recup (
        .clock(clock),
        .resetn(resetn),
        .pc_ex(pc_ex),
        .pc(pc),
        .addr_i(addr_i),
        .data_i(data_i)
    );

    // Unité de décodage d'instruction
    instruction_decode instruction_decode (
        .resetn(resetn),
        .decode(decode),
        .data_i(data_i),
        .reg1_addr(reg1_addr),
        .reg2_addr(reg2_addr),
        .reg_wb_addr(reg_wb_addr),
        .alu_op(alu_op),
        .mem_op(mem_op),
        .imm(imm)
    );

    // Unité d'exécution
    execution execution (
        .resetn(resetn),
        .execute(execute),
        .reg1(registres[reg1_addr]),
        .reg2(registres[reg2_addr]),
        .imm(imm),
        .alu_op(alu_op),
        .alu_data(alu_data)
    );

    // Unité d'accès mémoire
    memory_access acces_memoire (
        .resetn(resetn),
        .memory(memory),
        .alu_data(alu_data),
        .data_mem(data_mem),
        .mem_op(mem_op),
        .addr_d(addr_d),
        .data_d(data_d),
        .data_d_in(data_d_in)
    );

    // Unité d'écriture retour
    writeback writeback (
        .resetn(resetn),
        .writeback(writeback),
        .data_wb(data_wb),
        .reg_wb_addr(reg_wb_addr),
        .registres(registres)
    );

    // Trace
    assign trace_reg_1 = registres[0];

endmodule
```

**Principales sous-modules et leur description :**

* **Pipeline_control :** Gère les états du pipeline d'instructions et de données.
* **Instruction_fetch :** Récupère les instructions de la mémoire d'instructions et les place dans le pipeline.
* **Instruction_decode :** Décode l'instruction récupérée et génère les signaux de contrôle pour le reste du pipeline.
* **Execution :** Exécute l'instruction décodée et génère le résultat.
* **Memory_access :** Accède à la mémoire de données et fournit les données nécessaires à l'exécution.
* **Writeback :** Ecrit le résultat de l'exécution dans les registres.

**Le pipeline d'instructions est à 5 étages :**

* Récupération d'instruction
* Décodage d'instruction
* Exécution
* Accès mémoire
* Écriture retour

**Le pipeline de données est à 3 étages :**

* Exécution
* Accès mémoire
* Écriture retour