//============================================================================
///  PC8801SR
//
//  Copyright (C) 2017,2020 Alexey Melnikov
//  Copyright (C) 2020 Puu
//
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

`default_nettype none

module guest_top
(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef I2S_AUDIO_HDMI
	output        HDMI_MCLK,
	output        HDMI_BCK,
	output        HDMI_LRCK,
	output        HDMI_SDATA,
`endif
`ifdef SPDIF_AUDIO
	output        SPDIF,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

);

`ifndef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_AUDIO_IN
localparam bit USE_AUDIO_IN = 1;
wire TAPE_SOUND=AUDIO_IN;
`else
localparam bit USE_AUDIO_IN = 0;
wire TAPE_SOUND=UART_RX;
`endif


// remove this if the 2nd chip is actually used
`ifdef DUAL_SDRAM
assign SDRAM2_A = 13'hZZZZ;
assign SDRAM2_BA = 0;
assign SDRAM2_DQML = 0;
assign SDRAM2_DQMH = 0;
assign SDRAM2_CKE = 0;
assign SDRAM2_CLK = 0;
assign SDRAM2_nCS = 1;
assign SDRAM2_DQ = 16'hZZZZ;
assign SDRAM2_nCAS = 1;
assign SDRAM2_nRAS = 1;
assign SDRAM2_nWE = 1;
`endif


//////////////////////////////////////////////////////////////////
assign LED  = ~ioctl_download;
 

// Status Bit Map:
//              Upper                          Lower
// 0         1         2         3          4         5         6
// 01234567890123456789012345678901 23456789012345678901234567890123
// 0123456789ABCDEFGHIJKLMNOPQRSTUV 0123456789ABCDEFGHIJKLMNOPQRSTUV
// X XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XX      XXX

`include "build_id.v" 
parameter CONF_STR = {
	"PC8801;;",
	"-;",
	"F0,ROM,Load rom;",
	"OHJ,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	"-;",
	"O78,Mode,N88V2,N88V1H,N88V1L,N;",
	"O9,Speed,4MHz,8MHz;",
	"-;",
	"S0,D88,FDD0;",
	"S1,D88,FDD1;",
	"TF,SYNC FD0;",
	"TG,SYNC FD1;",
	"-;",
	"OA,Basic mode,Basic,Terminal;",
	"OB,Cols,80,40;",
	"OC,Lines,25,20;",
	"OD,Disk boot,Enable,Disable;",
	"-;",
	"OK,Input,Joypad,Mouse;",
	"OL,Sound Board 2,Expansion,Onboard;",
	"-;",
	"T6,Reset;",
};

/////////////////  CLOCKS  ////////////////////////

wire clk_ram, clk_sys, clk_emu;
wire pll_locked;

pll pll
(
	.inclk0(CLOCK_50),
	.c0(clk_ram), // 75mhz
	.c1(clk_sys), // 20mhz
	.c2(clk_emu), // 60mhz
	.locked(pll_locked)
);


altddio_out
#(
	.extend_oe_disable("OFF"),
	.intended_device_family("Cyclone 10 LP"),
	.invert_output("OFF"),
	.lpm_hint("UNUSED"),
	.lpm_type("altddio_out"),
	.oe_reg("UNREGISTERED"),
	.power_up_high("OFF"),
	.width(1)
)


sdramclk_ddr
(
	.datain_h(1'b0),
	.datain_l(1'b1),
	.outclock(clk_ram),
	.dataout(SDRAM_CLK),
	.aclr(1'b0),
	.aset(1'b0),
	.oe(1'b1),
	.outclocken(1'b1),
	.sclr(1'b0),
	.sset(1'b0)
);



/////////////////  HPS  ///////////////////////////

wire [63:0] status;
wire  [1:0] buttons;

wire [15:0] joystick_0, joystick_1;

wire  [5:0] joyA = ~{joystick_0[5:4],joystick_0[0] | joystick_0[6],joystick_0[1] | joystick_0[6],joystick_0[2] | joystick_0[7],joystick_0[3] | joystick_0[7]};
wire  [5:0] joyB = ~{joystick_1[5:4],joystick_1[0] | joystick_1[6],joystick_1[1] | joystick_1[6],joystick_1[2] | joystick_1[7],joystick_1[3] | joystick_1[7]};

wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;

wire        ps2_kbd_clk_out;
wire        ps2_kbd_data_out;
wire        ps2_kbd_clk_in;
wire        ps2_kbd_data_in;
wire        ps2_mouse_clk_out;
wire        ps2_mouse_data_out;
wire        ps2_mouse_clk_in;
wire        ps2_mouse_data_in;

wire  [31:0] sd_lba;
wire   [3:0] sd_rd;
wire   [3:0] sd_wr;

wire  [3:0] sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire  [3:0] img_mounted;
wire  [3:0] img_readonly;
wire [63:0] img_size;

//wire [65:0] ps2_key;
wire [63:0] sysrtc;
wire forced_scandoubler;
wire  [7:0] uart1_mode;
wire [31:0] uart1_speed;


wire scandoubler_disable;
wire ypbpr;
wire no_csync;
wire  [1:0] switches;
wire        key_pressed;
wire [7:0]  key_code;
wire        key_strobe;
wire        key_extended;

wire  [8:0] mouse_x;
wire  [8:0] mouse_y;
wire  [7:0] mouse_flags;
wire        mouse_strobe;
wire [24:0] ps2_mouse = { mouse_strobe_level, mouse_y[7:0], mouse_x[7:0], mouse_flags };
reg         mouse_strobe_level;

wire        sd_ack_conf;
wire        sd_conf;

always @(posedge clk_sys) if (mouse_strobe) mouse_strobe_level <= ~mouse_strobe_level;

`ifdef USE_HDMI
wire        i2c_start;
wire        i2c_read;
wire  [6:0] i2c_addr;
wire  [7:0] i2c_subaddr;
wire  [7:0] i2c_dout;
wire  [7:0] i2c_din;
wire        i2c_ack;
wire        i2c_end;
`endif





user_io #(.STRLEN($size(CONF_STR)>>3), .SD_IMAGES(4), .PS2DIV(600),.PS2BIDIR(1), .FEATURES(32'h0 | (BIG_OSD << 13) | (HDMI << 14))) user_io
( 
    .clk_sys(clk_sys),
    .clk_sd(clk_sys),
    .SPI_SS_IO(CONF_DATA0),
    .SPI_CLK(SPI_SCK),
    .SPI_MOSI(SPI_DI),
    .SPI_MISO(SPI_DO),

    .conf_str(CONF_STR),
	 .rtc(sysrtc),

    .status(status),
    .scandoubler_disable(scandoubler_disable),
    .ypbpr(ypbpr),
    .no_csync(no_csync),
    .buttons(buttons),
    .switches(switches),
    .joystick_0(joystick_0),
    .joystick_1(joystick_1),
    .key_strobe(key_strobe),
    .key_code(key_code),
    .key_pressed(key_pressed),
    .key_extended(key_extended),

    .mouse_x(mouse_x),
    .mouse_y(mouse_y),
    .mouse_flags(mouse_flags),
    .mouse_strobe(mouse_strobe),
	 
	.ps2_kbd_clk(ps2_kbd_clk_out),
	.ps2_kbd_data(ps2_kbd_data_out),
	.ps2_kbd_clk_i(ps2_kbd_clk_in),
	.ps2_kbd_data_i(ps2_kbd_data_in),
	.ps2_mouse_clk(ps2_mouse_clk_out),
	.ps2_mouse_data(ps2_mouse_data_out),
	.ps2_mouse_clk_i(ps2_mouse_clk_in),
	.ps2_mouse_data_i(ps2_mouse_data_in),

`ifdef USE_HDMI
    .i2c_start      (i2c_start      ),
    .i2c_read       (i2c_read       ),
    .i2c_addr       (i2c_addr       ),
    .i2c_subaddr    (i2c_subaddr    ),
    .i2c_dout       (i2c_dout       ),
    .i2c_din        (i2c_din        ),
    .i2c_ack        (i2c_ack        ),
    .i2c_end        (i2c_end        ),
`endif

    .sd_lba         (sd_lba),
    .sd_rd          (sd_rd),
    .sd_wr          (sd_wr),
    .sd_ack_x       (sd_ack),
	 .sd_ack_conf    (sd_ack_conf   ),
    .sd_dout        (sd_buff_dout),
    .sd_dout_strobe (sd_buff_wr),
    .sd_din         (sd_buff_din),
    .sd_buff_addr   (sd_buff_addr),
    .sd_conf        (sd_conf),
    .sd_sdhc        (1'b1),
    .img_mounted(img_mounted),
    .img_size(img_size)
);

data_io data_io (
    // SPI interface
    .SPI_SCK        ( SPI_SCK ),
    .SPI_SS2        ( SPI_SS2 ),
    .SPI_DI         ( SPI_DI  ),
    // ram interface
    .clk_sys        ( clk_sys ),
    .clkref_n       ( 1  ),
    .ioctl_download ( ioctl_download ),
    .ioctl_index    ( ioctl_index ),
    .ioctl_wr       ( ioctl_wr ),
    .ioctl_addr     ( ioctl_addr ),
    .ioctl_dout     ( ioctl_dout )
);

////////////////  RESET  /////////////////////////
//
reg reset_n = 0;
always @(posedge clk_sys) begin
	reg old_download;
	
	old_download <= ioctl_download;
	if(~old_download & ioctl_download) reset_n <= 1;
end

wire reset = buttons[1] || status[6] || ~pll_locked;
///////////////////////////////////////////////////

wire [1:0] basicmode=~status[8:7];
wire clkmode=status[9];
wire cBT		=~status[10];
wire	c40C	=status[11];
wire	c20L	=status[12];
wire	cDisk	=status[13];
wire	MTSAVE	=1;
wire [1:0]FDsync=status[16:15];
wire	cInDev	=status[20];
wire	cSB2	=status[21];


wire disk_led;

wire [7:0] red, green, blue;
wire HSync, VSync, HBlank, VBlank, vid_de, ce_pix;

PC88MiSTer PC88_top
(
	.clk21m(clk_sys),
	.rclk(clk_ram),
	.emuclk(clk_emu),
	.plllocked(pll_locked),
	
	.sysrtc(sysrtc),

	.LOADER_ADR(ioctl_addr[18:0]),
	.LOADER_WDAT(ioctl_dout),
	.LOADER_OE(ioctl_download & ~ldr_done),
	.LOADER_WR(ldr_wr),
	.LOADER_ACK(ldr_ack),
	.LOADER_DONE(ldr_done),

	.pMemCke(SDRAM_CKE),
	.pMemCs_n(SDRAM_nCS),
	.pMemRas_n(SDRAM_nRAS),
	.pMemCas_n(SDRAM_nCAS),
	.pMemWe_n(SDRAM_nWE),
	.pMemUdq(SDRAM_DQMH),
	.pMemLdq(SDRAM_DQML),
	.pMemBa1(SDRAM_BA[1]),
	.pMemBa0(SDRAM_BA[0]),
	.pMemAdr(SDRAM_A),
	.pMemDat(SDRAM_DQ),

	.pPs2Clkin(ps2_kbd_clk_out),
	.pPs2Clkout(ps2_kbd_clk_in),
	.pPs2Datin(ps2_kbd_data_out),
	.pPs2Datout(ps2_kbd_data_in),

	.ps2_mouse(ps2_mouse),

	.pJoyA(joyA),
	.pJoyB(joyB),

	.mist_mounted(img_mounted),
	.mist_readonly(img_readonly),
	.mist_imgsize(img_size),

	.mist_lba(sd_lba),
	.mist_rd(sd_rd),
	.mist_wr(sd_wr),
	.mist_ack({sd_ack[3:2], |sd_ack[1:0], |sd_ack[1:0]}),

	.mist_buffaddr(sd_buff_addr),
	.mist_buffdout(sd_buff_dout),
	.mist_buffdin(sd_buff_din),
	.mist_buffwr(sd_buff_wr),

	.pFd_sync(FDsync),

	.pLed(disk_led),
	.pDip({clkmode,2'b0,cDisk,c20L,c40C,MTSAVE,cBT,basicmode}),
	.pCoreConfig({cSB2,cInDev}),
	.pPsw(2'b11),

	.pVideoR(red),
	.pVideoG(green),
	.pVideoB(blue),
	.pVideoHS(HSync),
	.pVideoVS(VSync),
	.pVideoEN(vid_de),
	.pVideoClk(ce_pix),

	.pSndL(out_l),
	.pSndR(out_r),

	.rstn(reset_n & ~reset)
);

wire ldr_ack;
reg ldr_wr = 0;
reg ldr_done = 0;
always @(posedge clk_sys) begin
	reg old_ack, old_download;

	old_download <= ioctl_download;
	old_ack <= ldr_ack;

	if(~old_ack & ldr_ack & ldr_wr) ldr_wr <= 0;
	if(ioctl_wr & ~ldr_done) ldr_wr <= 1;

	if(old_download & ~ioctl_download) ldr_done <= 1;
end


//////////////////   SD LED   ///////////////////
// reg sd_act;

//always @(posedge clk_sys) begin
//	reg old_mosi, old_miso;
//	integer timeout = 0;
//
//	old_mosi <= SD_MOSI;
//	old_miso <= SD_MISO;
//
//	// sd_act <= 0;
//	if(timeout < 1000000) begin
//		timeout <= timeout + 1;
//		// sd_act <= 1;
//	end
//
//	if((old_mosi ^ SD_MOSI) || (old_miso ^ SD_MISO)) timeout <= 0;
//end

wire[15:0] out_l,out_r;

`ifdef I2S_AUDIO

wire [31:0] clk_rate =  32'd20_000_000;

i2s i2s (
        .reset(reset),
        .clk(clk_sys),
        .clk_rate(clk_rate),

        .sclk(I2S_BCK),
        .lrclk(I2S_LRCK),
        .sdata(I2S_DATA),

        .left_chan (out_l),
        .right_chan(out_r)
);
`ifdef I2S_AUDIO_HDMI
assign HDMI_MCLK = 0;
always @(posedge clk_sys) begin
	HDMI_BCK <= I2S_BCK;
	HDMI_LRCK <= I2S_LRCK;
	HDMI_SDATA <= I2S_DATA;
end
`endif
`endif

`ifdef SPDIF_AUDIO
spdif spdif (
	.clk_i(clk_sys),
	.rst_i(1'b0),
	.clk_rate_i(clk_rate),
	.spdif_o(SPDIF),
	.sample_i({out_l,out_r})
);
`endif

dac #(
   .c_bits	(16))
audiodac_l(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(out_l),
   .dac_o	(AUDIO_L)
  );

dac #(
   .c_bits	(16))
audiodac_r(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(out_r),
   .dac_o	(AUDIO_R)
  );

  
////////////////////////////  VIDEO  ////////////////////////////////////
  
mist_video #(
   .COLOR_DEPTH(8),
	.OUT_COLOR_DEPTH(VGA_BITS),
//	.USE_BLANKS(1),
//	.VIDEO_CLEANER(1),
	.BIG_OSD(BIG_OSD)) 
mist_video (	
   .*,
	.clk_sys      (clk_ram    ),
	.SPI_SCK      (SPI_SCK    ),
	.SPI_SS3      (SPI_SS3    ),
	.SPI_DI       (SPI_DI     ),
	.R            (red),
	.G            (green),
	.B            (blue),
	.HSync        (HSync),
	.VSync        (VSync),
//	.HBlank       (HBlank),
//	.VBlank       (VBlank),
	.VGA_R        (VGA_R      ),
	.VGA_G        (VGA_G      ),
	.VGA_B        (VGA_B      ),
	.VGA_VS       (VGA_VS     ),
	.VGA_HS       (VGA_HS     ),
	.VGA_HB(),
	.VGA_VB(),
	.VGA_DE(),
	.ce_divider   (3'd1	      ),
	.scandoubler_disable (1),
	.rotate(2'b00),
   .blend(1'b0),
	.no_csync(1),
	.ypbpr(ypbpr),
	.scanlines    (status[19:17])
	);

`ifdef USE_HDMI
i2c_master #(20_000_000) i2c_master (
	.CLK         (clk_sys),
	.I2C_START   (i2c_start),
	.I2C_READ    (i2c_read),
	.I2C_ADDR    (i2c_addr),
	.I2C_SUBADDR (i2c_subaddr),
	.I2C_WDATA   (i2c_dout),
	.I2C_RDATA   (i2c_din),
	.I2C_END     (i2c_end),
	.I2C_ACK     (i2c_ack),

	//I2C bus
	.I2C_SCL     (HDMI_SCL),
	.I2C_SDA     (HDMI_SDA)
);

mist_video #(.COLOR_DEPTH(6), .SD_HCNT_WIDTH(11), .OUT_COLOR_DEPTH(8), .USE_BLANKS(1), .BIG_OSD(BIG_OSD), .VIDEO_CLEANER(1)) hdmi_video (
	.clk_sys     ( clk_sys   ),

	// OSD SPI interface
	.SPI_SCK     ( SPI_SCK    ),
	.SPI_SS3     ( SPI_SS3    ),
	.SPI_DI      ( SPI_DI     ),
	.scanlines   (  ),
	.ce_divider  ( 3'd0       ),
	.scandoubler_disable (1'b1),
	.no_csync    ( 1'b1       ),
	.ypbpr       ( 1'b0       ),
	.rotate      ( 2'b00      ),
	.blend       ( 1'b0       ),
	.R           (red),
	.G           (green),
	.B           (blue),
//	.HBlank      ( HBlank      ),
//	.VBlank      ( VBlank      ),
	.HSync       ( HSync       ),
	.VSync       ( VSync       ),
	.VGA_R       ( HDMI_R      ),
	.VGA_G       ( HDMI_G      ),
	.VGA_B       ( HDMI_B      ),
	.VGA_VS      (HDMI_VS      ),
	.VGA_HS      (HDMI_HS      ),
	.VGA_DE      ( HDMI_DE     )
);
assign HDMI_PCLK = clk_sys;

`endif

endmodule
