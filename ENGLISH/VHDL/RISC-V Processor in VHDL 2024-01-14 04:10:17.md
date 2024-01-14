```vhdl
-- Top-level module of a RISC-V processor
module riscv_top (
  input  clk,
  input  reset,
  output [31:0] pc,
  output [31:0] instr,
  output [31:0] alu_result,
  output [31:0] reg_data,
  output [4:0]  rs1_addr,
  output [4:0]  rs2_addr,
  output [4:0]  rd_addr,
  output [31:0] imm_gen_out,
  output        mem_write,
  output [31:0] mem_addr,
  output [31:0] mem_wdata,
  input  [31:0] mem_rdata
);

  -- Register File
  regfile regfile (
    .clk(clk),
    .reset(reset),
    .rs1_addr(rs1_addr),
    .rs2_addr(rs2_addr),
    .rd_addr(rd_addr),
    .rd_data(reg_data),
    .we(rd_addr != 0 && we),
    .wdata(alu_result)
  );

  -- Instruction Memory
  rom instr_mem (
    .addr(pc[31:2]),
    .dout(instr)
  );

  -- Immediate Generator
  imm_gen imm_gen (
    .instr(instr),
    .imm(imm_gen_out)
  );

  -- ALU
  alu alu (
    .a(rs1_data),
    .b(rs2_data),
    .op(alu_op),
    .result(alu_result)
  );

  -- Data Memory
  data_mem data_mem (
    .clk(clk),
    .addr(mem_addr),
    .wdata(mem_wdata),
    .we(mem_write),
    .rdata(mem_rdata)
  );

  -- Program Counter
  program_counter pc (
    .clk(clk),
    .reset(reset),
    .next_pc(pc_next),
    .pc(pc)
  );

  -- Control Unit
  control_unit control_unit (
    .instr(instr),
    .alu_op(alu_op),
    .we(we),
    .mem_write(mem_write)
  );

  -- Hazard Detection Unit
  hazard_detection_unit hazard_detection_unit (
    .rs1_addr(rs1_addr),
    .rs2_addr(rs2_addr),
    .rd_addr(rd_addr),
    .pc_next(pc_next)
  );

  -- Multiplexers
  assign rs1_data = (hazard_detection_unit.data_hazard) ? reg_data : mem_rdata;
  assign rs2_data = (hazard_detection_unit.data_hazard) ? reg_data : mem_rdata;
  assign pc_next = (hazard_detection_unit.ctrl_hazard) ? pc + 4 : imm_gen_out;

  -- Memory Address Register
  register #(32) mar (
    .clk(clk),
    .reset(reset),
    .d(imm_gen_out),
    .q(mem_addr)
  );

  -- Memory Data Register
  register #(32) mdr (
    .clk(clk),
    .reset(reset),
    .d(reg_data),
    .q(mem_wdata)
  );

endmodule

-- Register File
module regfile (
  input  clk,
  input  reset,
  input  [4:0]  rs1_addr,
  input  [4:0]  rs2_addr,
  input  [4:0]  rd_addr,
  output [31:0] rd_data,
  input         we,
  input  [31:0] wdata
);

  reg [31:0] regs[0:31];

  assign rd_data = (rs1_addr == rd_addr) ? regs[rs1_addr] :
                   (rs2_addr == rd_addr) ? regs[rs2_addr] : 0;

  always @(posedge clk) begin
    if (reset) begin
      regs[0] <= 0;
      for (integer i = 1; i < 32; i = i + 1) begin
        regs[i] <= 32'hdeadbeef;
      end
    end else if (we && rd_addr != 0) begin
      regs[rd_addr] <= wdata;
    end
  end

endmodule

-- Instruction Memory
module rom (
  input  [31:0] addr,
  output [31:0] dout
);

  reg [31:0] mem[0:1023];

  assign dout = mem[addr];

  initial begin
    $readmemh("instructions.hex", mem);
  end

endmodule

-- Immediate Generator
module imm_gen (
  input  [31:0] instr,
  output [31:0] imm
);

  wire [6:0] opcode = instr[6:0];
  wire [2:0] funct3 = instr[14:12];
  wire [6:0] fun