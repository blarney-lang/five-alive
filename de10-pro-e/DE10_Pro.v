module DE10_Pro(
  input CLK_100_B3I,
  input CLK_50_B2C,
  input CLK_50_B2L,
  input CLK_50_B3C,
  input CLK_50_B3I,
  input CLK_50_B3L,

  input CPU_RESET_n,
  input [1:0] BUTTON,
  input [1:0] SW,
  output [3:0] LED,

  inout SI5340A0_I2C_SCL,
  inout SI5340A0_I2C_SDA,
  input SI5340A0_INTR,
  output SI5340A0_OE_n,
  output SI5340A0_RST_n,

  inout SI5340A1_I2C_SCL,
  inout SI5340A1_I2C_SDA,
  input SI5340A1_INTR,
  output SI5340A1_OE_n,
  output SI5340A1_RST_n,

  output FLASH_CLK,
  output [27:1] FLASH_A,
  inout [15:0] FLASH_D,
  output FLASH_CE_n,
  output FLASH_WE_n,
  output FLASH_OE_n,
  output FLASH_ADV_n,
  output FLASH_RESET_n,
  input FLASH_RDY_BSY_n,

  input EXP_EN,

  inout UFL_CLKIN_p,
  inout UFL_CLKIN_n
);

wire reset_n;

// Reset release
wire ninit_done;
reset_release reset_release (
        .ninit_done(ninit_done)
        );

assign reset_n = !ninit_done && CPU_RESET_n;

DE10_Pro_QSYS DE10_Pro_QSYS_inst (
        .clk_clk(CLK_50_B3I),
        .reset_reset(~reset_n),
        .iopll_0_locked_export()
    );

assign SI5340A0_RST_n = 1'b1;
assign SI5340A1_RST_n = 1'b1;

assign SI5340A0_OE_n = 1'b0;
assign SI5340A1_OE_n = 1'b0;

endmodule
