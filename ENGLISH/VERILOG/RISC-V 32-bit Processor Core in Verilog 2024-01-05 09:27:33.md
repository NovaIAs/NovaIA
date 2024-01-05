```verilog
// Verilog code for a 32-bit RISC-V processor core

// Define the instruction set architecture (ISA)
module riscv_core(
    input clk,
    input reset,
    // Instruction memory interface
    output [31:0] imem_addr,
    input [31:0] imem_rdata,
    // Data memory interface
    output [31:0] dmem_addr,
    output [31:0] dmem_wdata,
    input [31:0] dmem_rdata,
    // Register file interface
    output [4:0] rs1,
    output [4:0] rs2,
    output [4:0] rd,
    input [31:0] rdata1,
    input [31:0] rdata2,
    // Control signals
    output reg_write,
    output mem_read,
    output mem_write,
    output branch,
    output jal,
    output jalr,
    // Exception handling
    output exception
);

// Define the register file
reg [31:0] registers[32];

// Define the program counter (PC)
reg [31:0] pc;

// Define the instruction register (IR)
reg [31:0] ir;

// Define the control signals
assign reg_write = (ir[6:0] == 7'b0110011); // R-type instructions
assign mem_read = (ir[6:0] == 7'b0000011); // Load instructions
assign mem_write = (ir[6:0] == 7'b0100011); // Store instructions
assign branch = (ir[6:0] == 7'b1100011); // Branch instructions
assign jal = (ir[6:0] == 7'b1101111); // Jump and link instructions
assign jalr = (ir[6:0] == 7'b1100111); // Jump and link register instructions
assign exception = 0; // No exceptions are handled in this simple core

// Define the datapath
always @(*) begin
    // Read the instruction from memory
    imem_addr = pc;

    // Decode the instruction
    case (ir[6:0])
        7'b0110011: // R-type instructions
            rs1 = ir[19:15];
            rs2 = ir[24:20];
            rd = ir[11:7];
        7'b0000011: // Load instructions
            rs1 = ir[19:15];
            rd = ir[11:7];
        7'b0100011: // Store instructions
            rs1 = ir[19:15];
            rs2 = ir[24:20];
        7'b1100011: // Branch instructions
            rs1 = ir[19:15];
            rs2 = ir[24:20];
        7'b1101111: // Jump and link instructions
            rd = ir[11:7];
        7'b1100111: // Jump and link register instructions
            rs1 = ir[19:15];
        default:
            rs1 = 0;
            rs2 = 0;
            rd = 0;
    endcase

    // Read the register file
    rdata1 = registers[rs1];
    rdata2 = registers[rs2];

    // Perform the operation
    case (ir[6:0])
        7'b0110011: // R-type instructions
            case (ir[31:25])
                7'b0000000: // ADD
                    registers[rd] = rdata1 + rdata2;
                7'b0100000: // SUB
                    registers[rd] = rdata1 - rdata2;
                7'b0000001: // SLL
                    registers[rd