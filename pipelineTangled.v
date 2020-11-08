// Pipelined Tangled Implementation

// basic sizes of things
`define WORD	[15:0]
`define	STATE	[3:0] // some state numbers are OPs
`define REGSIZE	[7:0]
`define MEMSIZE	[65535:0]

// field placements and values
`define Op0	[15:12] // opcode field
`define	Reg0	[11:8]  // register number field
`define DestReg [7:4]
`define SourceReg [3:0]
`define DATA	[15:0]
`define REGS	[15:0]
`define SECOND4 `Reg0
`define BOTTOM8 [7:0]
`define TOP8    [15:8]
`define THIRD4  [7:4]
`define FOURTH4 [3:0]

// // basic sizes of things
// `define DATA	[15:0]
// `define WORD	[15:0]
// `define MEMSIZE	[65535:0]
// `define WORD	[15:0]
// `define CC	[15:14]
// `define OP	[14:9]
// `define IORR	[8]
// `define RD	[7:4]
// `define RN	[3:0]
// `define REGS    [15:0]

// CC values
// `define AL	0
// `define S	1
// `define NE	2
// `define EQ	3

// // opcode values, also state numbers
// `define OPPRE		5'h00
// `define OPADD		5'h08
// `define OPAND		5'h09
// `define OPBIC		5'h0a
// `define OPEOR		5'h0b
// `define OPMUL		5'h0c
// `define OPORR		5'h0d
// `define OPSHA		5'h0e
// `define OPSLT		5'h0f
// `define OPSUB		5'h10
// `define OPADDF		5'h11
// `define OPFTOI		5'h12
// `define OPITOF		5'h13
// `define OPMULF		5'h14
// `define OPRECF		5'h15
// `define OPSUBF		5'h16
// `define OPMOV		5'h17
// `define OPNEG		5'h18
// `define OPLDR		5'h19
// `define OPSTR		5'h1a
// `define OPSYS		5'h1f

// op codes
`define SYSCALL 16'b0

`define OPsys      4'h0

`define OPoneReg   4'h1
`define OPjumpr    4'h0
`define OPneg      4'h1
`define OPnegf     4'h2
`define OPnot      4'h3

`define OPbrf      4'h2
`define OPbrt      4'h3
`define OPlex      4'h4
`define OPlhi      4'h5

`define OPnorms    4'h6
`define OPadd      4'h0
`define OPmul      4'h1
`define OPslt      4'h2
`define OPand      4'h3
`define OPor	   4'h4
`define OPxor      4'h5
`define OPshift    4'h6

`define OPfloats   4'h7
`define OPaddf     4'h0
`define OPmulf     4'h1
`define OPsltf     4'h2
`define OPrecip    4'h4
`define OPfloat    4'h8
`define OPint      4'h9

`define OPmem      4'h7
`define OPcopy     4'ha
`define OPload     4'hb
`define OPstore    4'hc

`define Decode     4'h8

`define OPsingleQ  4'h9
`define OPnotQ     4'h0
`define OPoneQ     4'h1
`define OPzeroQ    4'h2

`define OPhadQ     4'ha

`define OPtwoQ     4'hb
`define OPcnotQ    4'h0
`define OPswapQ    4'h1

`define OPthreeQ   4'hc
`define OPccnotQ   4'h0
`define OPcswapQ   4'h1
`define OPandQ     4'h3
`define OPorQ      4'h4
`define OPxorQ     4'h5

`define Start      4'hd

`define OPmeasQ    4'he

`define OPnextQ    4'hf

// make NOP (after fetch) an unconditional PRE 0
`define NOP             16'b0
`define NaN             16'hffff
// Floating point Verilog modules for CPE480
  // Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
  // Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

  // Field definitions
  `define	WORD	[15:0]	// generic machine word size
  `define	INT	signed [15:0]	// integer size
  `define FLOAT	[15:0]	// half-precision float size
  `define FSIGN	[15]	// sign bit
  `define FEXP	[14:7]	// exponent
  `define FFRAC	[6:0]	// fractional part (leading 1 implied)
  `define NOTSIGN [14:0]  //portion of the float that isnt the sign

  // Constants
  `define	FZERO	16'b0	  // float 0
  `define F32767  16'h46ff  // closest approx to 32767, actually 32640
  `define F32768  16'hc700  // -32768

  // Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
  module lead0s(d, s);
  output wire [4:0] d;
  input wire `WORD s;
  wire [4:0] t;
  wire [7:0] s8;
  wire [3:0] s4;
  wire [1:0] s2;
  assign t[4] = 0;
  assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
  assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
  assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
  assign t[0] = !s2[1];
  assign d = (s ? t : 16);
  endmodule

  // Float set-less-than, 16-bit (1-bit result) torf=a<b
  module fslt(torf, a, b);
  output wire torf;
  input wire `FLOAT a, b;
  assign torf = (a `FSIGN && !(b `FSIGN)) ||
          (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
          (!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0]));
  endmodule

  // Floating-point addition, 16-bit r=a+b
  module fadd(r, a, b);
  output wire `FLOAT r;
  input wire `FLOAT a, b;
  wire `FLOAT s;
  wire [8:0] sexp, sman, sfrac;
  wire [7:0] texp, taman, tbman;
  wire [4:0] slead;
  wire ssign, aegt, amgt, eqsgn;
  assign r = ((a != a) ? NaN : ((b != b) ? NaN : ((a == 0) ? b : ((b == 0) ? a : s))));
  assign aegt = (a `FEXP > b `FEXP);
  assign texp = (aegt ? (a `FEXP) : (b `FEXP));
  assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
  assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
  assign eqsgn = (a `FSIGN == b `FSIGN);
  assign amgt = (taman > tbman);
  assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
  lead0s m0(slead, {sman, 7'b0});
  assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
  assign sfrac = sman << slead;
  assign sexp = (texp + 1) - slead;
  assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
  endmodule

  // Floating-point multiply, 16-bit r=a*b
  module fmul(r, a, b);
  output wire `FLOAT r;
  input wire `FLOAT a, b;
  wire [15:0] m; // double the bits in a fraction, we need high bits
  wire [7:0] e;
  wire s;
  assign s = (a `FSIGN ^ b `FSIGN);
  assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
  assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
  assign r = ( (a != a) ? NaN : ((b != b) ? NaN : (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}))));
  endmodule

  // Floating-point reciprocal, 16-bit r=1.0/a
  // Note: requires initialized inverse fraction lookup table
  module frecip(r, a);
  output wire `FLOAT r;
  input wire `FLOAT a;
  reg [6:0] look[127:0];
  initial $readmemh("reciprocalLookup.mem", look);
  assign r `FSIGN = a `FSIGN;
  assign r `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
  assign r `FFRAC = look[a `FFRAC];
  assign r = (a != a) ? NaN : r;
  endmodule

  // Floating-point shift, 16 bit
  // Shift +left,-right by integer
  module fshift(r, f, i);
  output wire `FLOAT r;
  input wire `FLOAT f;
  input wire `INT i;
  assign r `FFRAC = f `FFRAC;
  assign r `FSIGN = f `FSIGN;
  assign r `FEXP = (f ? (f `FEXP + i) : 0);
  endmodule

  // Integer to float conversion, 16 bit
  module i2f(f, i);
  output wire `FLOAT f;
  input wire `INT i;
  wire [4:0] lead;
  wire `WORD pos;
  assign pos = (i[15] ? (-i) : i);
  lead0s m0(lead, pos);
  assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
  assign f `FSIGN = i[15];
  assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
  endmodule

  // Float to integer conversion, 16 bit
  // Note: out-of-range values go to -32768 or 32767
  module f2i(i, f);
  output wire `INT i;
  input wire `FLOAT f;
  wire `FLOAT ui;
  wire tiny, big;
  fslt m0(tiny, f, `F32768);
  fslt m1(big, `F32767, f);
  assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
  assign i = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));
  endmodule

  // Floating-point negative, 16-bit r=-a
  // Note: requires initialized inverse fraction lookup table
  module negf(r, a);
  output wire `FLOAT r;
  input wire `FLOAT a;
  assign r = {!(a `FSIGN), a `NOTSIGN};
  assign r = (a != a) ? NaN : r;
  endmodule

// Field definitions
`define	WORD	[15:0]	// generic machine word size

module processor(halt, reset, clk);
  output reg halt;
  input reset, clk;

  reg `DATA r `REGS;	// register file
  reg `DATA data `MEMSIZE;	// data memory
  reg `WORD text `MEMSIZE;	// instruction memory
  reg `WORD pc;		// program counter
  reg `WORD tpc, pc0, pc1;
  reg `WORD ir;		// instruction register
  reg `WORD ir0, ir1;
  reg `DATA im0, rd1, rn1, res;
  reg `WORD target;	// jump target
  reg jump;		// are we jumping?
  reg zreg;		// z flag
  wire pendz;		// z update pending?
  wire pendpc;		// pc update pending?
  reg wait1;		// need to stall in stage 1?
  reg [11:0] prefix;	// 12-bit prefix value
  reg havepre;		// is prefix valid?

  always @(reset) begin
    halt = 0;
    pc = 0;
    ir0 = `NOP;
    ir1 = `NOP;
    jump = 0;
    havepre = 0;

  // use the following with dollars to initialize
  //readmemh0(r); // register file
  //readmemh1(data); // data memory
  //readmemh2(text); // instruction memory
  end

  function setsrd;
  input `WORD inst;
  setsrd = ((inst `OP >= `OPADD) && (inst `OP < `OPSTR));
  endfunction

  function setspc;
  input `WORD inst;
  setspc = ((inst `RD == 15) && setsrd(inst));
  endfunction

  // function setsz;
  // input `WORD inst;
  // setsz = ((inst `CC == `S) && setsrd(inst));
  // endfunction

  // function iscond;
  // input `WORD inst;
  // iscond = ((inst `CC == `NE) || (inst `CC == `EQ));
  // endfunction

  function usesim;
  input `WORD inst;
  usesim = ((inst `IORR) && (inst `OP <= `OPSTR));
  endfunction

  function usesrd;
  input `WORD inst;
  usesrd = ((inst `OP == `OPADD) ||
            (inst `OP == `OPADDF) ||
            (inst `OP == `OPAND) ||
            (inst `OP == `OPBIC) ||
            (inst `OP == `OPEOR) ||
            (inst `OP == `OPMUL) ||
            (inst `OP == `OPMULF) ||
            (inst `OP == `OPORR) ||
            (inst `OP == `OPSHA) ||
            (inst `OP == `OPSTR) ||
            (inst `OP == `OPSLT) ||
            (inst `OP == `OPSUB) ||
            (inst `OP == `OPSUBF));
  endfunction

  function usesrn;
  input `WORD inst;
  usesrn = ((!(inst `IORR)) && (inst `OP <= `OPSTR));
  endfunction

  // pending z update?
  assign pendz = (setsz(ir0) || setsz(ir1));

  // pending PC update?
  assign pendpc = (setspc(ir0) || setspc(ir1));

  // stage 0: instruction fetch and immediate extend
  always @(posedge clk) begin
    tpc = (jump ? target : pc);

    if (wait1) begin
      // blocked by stage 1, so should not have a jump, but...
      pc <= tpc;
    end else begin
      // not blocked by stage 1
      ir = text[tpc];

      if (pendpc || (iscond(ir) && pendz)) begin
        // waiting... pc doesn't change
        ir0 <= `NOP;
        pc <= tpc;
      end else begin
        if (ir[13:12] == 0) begin
          // PRE operation
          havepre <= 1;
          prefix <= ir[11:0];
          ir0 <= `NOP;
        end else begin
          if (usesim(ir)) begin
            // extend immediate
            im0 <= {(havepre ? prefix : {12{ir[3]}}), ir `RN};
            havepre <= 0;
          end
          ir0 <= ir;
        end
        pc <= tpc + 1;
      end

      pc0 <= tpc;
    end
  end

  // stage 1: register read
  always @(posedge clk) begin
    if ((ir0 != `NOP) &&
        setsrd(ir1) &&
        ((usesrd(ir0) && (ir0 `RD == ir1 `RD)) ||
        (usesrn(ir0) && (ir0 `RN == ir1 `RD)))) begin
      // stall waiting for register value
      wait1 = 1;
      ir1 <= `NOP;
    end else begin
      // all good, get operands (even if not needed)
      wait1 = 0;
      rd1 <= ((ir0 `RD == 15) ? pc0 : r[ir0 `RD]);
      rn1 <= (usesim(ir0) ? im0 :
              ((ir0 `RN == 15) ? pc0 : r[ir0 `RN]));
      ir1 <= ir0;
    end
  end

  // stage 2: ALU, data memory access, store in register
  always @(posedge clk) begin
    if ((ir1 == `NOP) ||
        ((ir1 `CC == `EQ) && (zreg == 0)) ||
        ((ir1 `CC == `NE) && (zreg == 1))) begin
      // condition says nothing happens
      jump <= 0;
    end else begin
      // let the instruction execute
      case (ir1 `OP)
        // `OPPRE:  begin end // do nothing
        // `OPADD:  res = rd1 + rn1;
        // `OPAND:  res = rd1 & rn1;
        // `OPBIC:  res = rd1 & ~rn1;
        // `OPEOR:  res = rd1 ^ rn1;
        // `OPMUL:  res = rd1 * rn1;
        // `OPORR:  res = rd1 | rn1;
        // `OPSHA:  res = ((rn1 > 0) ? (rd1 << rn1) : (rd1 >> -rn1));
        // `OPSLT:  res = (rd1 < rn1);
        // `OPSUB:  res = rd1 - rn1;
        // `OPMOV:  res = rn1;
        // `OPNEG:  res = -rn1;
        // `OPLDR:  res = data[rn1];
        // `OPSTR:  begin res = rd1; data[rn1] <= res; end
        default: halt <= 1; // make it stop
      endcase

      // // update z flag if we should
      // if (setsz(ir1)) zreg <= (res == 0);

      // // put result in rd if we should
      // if (setsrd(ir1)) begin
      //   if (ir1 `RD == 15) begin
      //     jump <= 1;
      //     target <= res;
      //   end else begin
      //     r[ir1 `RD] <= res;
      //     jump <= 0;
      //   end
      // end else jump <= 0;
    end
  end
endmodule

module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted, reset, clk);
initial begin
  $dumpfile("pinkydump.txt");
  $dumpvars(0, PE);
  #10 reset = 1;
  #10 reset = 0;
  while (!halted) begin
    #10 clk = 1;
    #10 clk = 0;
  end
  $finish;
end
endmodule