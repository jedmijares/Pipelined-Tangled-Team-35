// basic sizes of things
`define DATA [15:0]
`define ADDR [15:0]
`define SIZE [65535:0]
`define INST [15:0]
`define CC [15:14]
`define OP [14:9]
`define IORR [8]
`define RD [7:4]
`define RN [3:0]
`define REGS [15:0]

// Multi-cycle Tangled Implementation

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

// op codes
`define SYSCALL 16'b0

`define OPsys      4'h0

`define OP8bits [15:8]
`define OP4bits [15:12]

`define OPoneReg   4'h1
`define OPjumpr    8'h10
`define OPneg      8'h11 //
`define OPnegf     8'h12 //
`define OPnot      8'h13 //

`define OPbrf      4'h2
`define OPbrt      4'h3
`define OPlex      4'h4
`define OPlhi      4'h5

`define OPnorms    4'h6
`define OPadd      8'h60 //
`define OPmul      8'h61 //
`define OPslt      8'h62 //
`define OPand      8'h63 //
`define OPor	     8'h64 //
`define OPxor      8'h65 //
`define OPshift    8'h66 //

`define OPfloats   4'h7
`define OPaddf     8'h70 //
`define OPmulf     8'h71 //
`define OPsltf     8'h72 //
`define OPrecip    8'h74 //
`define OPfloat    8'h78// 
`define OPint      8'h79//

`define OPmem      4'h7
`define OPcopy     8'h7a
`define OPload     8'h7b
`define OPstore    8'h7c //

`define DecodeOrNOP     4'h8
`define Decode     8'h80
`define NOP        16'h8100

`define OPsingleQ  4'h9
`define OPnotQ     8'h90
`define OPoneQ     8'h91
`define OPzeroQ    8'h92

`define OPhadQ     4'ha

`define OPtwoQ     4'hb
`define OPcnotQ    8'hb0
`define OPswapQ    8'hb1

`define OPthreeQ   4'hc
`define OPccnotQ   8'hc0
`define OPcswapQ   8'hc1
`define OPandQ     8'hc3
`define OPorQ      8'hc4
`define OPxorQ     8'hc5

`define Start      4'hd

`define OPmeasQ    4'he

`define OPnextQ    4'hf

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
  assign r = ((a == 0) ? b : ((b == 0) ? a : s));
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
  assign r = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));
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
  endmodule

// Field definitions
`define	WORD	[15:0]	// generic machine word size

module processor(halt, reset, clk);
  output reg halt;
  input reset, clk;
  reg `DATA r `REGS;	// register file [15:0]

  reg `WORD text `MEMSIZE; // instruction memory
  reg `WORD data `MEMSIZE; // data memory
  reg `WORD pc = 0;
  reg `WORD tpc, pc0, pc1;
  reg `WORD ir;
  reg `WORD ir0, ir1;
  // reg `WORD nextInstruction;
  reg `DATA im0, rd1, rn1, res;
  reg `WORD target;	// jump target
  reg jump; // are we jumping?
  wire pendpc; // pc update pending?
  reg wait1; // need to stall in stage 1?
  reg `WORD sltCheck;
  reg `STATE s, s2;

  wire `FLOAT addfRes, multfRes, sltfRes, recipRes, floatRes, intRes, negfRes;
  fadd myFAdd(addfRes, r[ir `THIRD4], r[ir `FOURTH4]);
  fmul myFMul(multfRes,r[ir `THIRD4], r[ir `FOURTH4]);
  fslt myFSLT(sltfRes, r[ir `THIRD4], r[ir `FOURTH4]);
  frecip myFRec(recipRes, r[ir `THIRD4]);
  f2i myFloToInt(intRes ,r[ir `THIRD4]);
  i2f myIntToFlo(floatRes, r[ir `THIRD4]); 
  negf myNegF(negfRes, r[ir `THIRD4]);

  always @(posedge reset) begin
    halt <= 0;
    pc <= 0;
    s <= `Start;
    ir0 = `NOP;
    ir1 = `NOP;
    $readmemh("testAssembly.text", text);
    $readmemh("testAssembly.data", data);
    jump = 0;
    r[15] = 16'hffff; // initialize stack pointer
  end

  function setsrd;
    input `WORD inst;
    setsrd = ((
      inst `OP8bits == `OPneg ||
      inst `OP8bits == `OPnegf ||
      inst `OP8bits == `OPnot ||
      inst `OP8bits == `OPadd ||
      inst `OP8bits == `OPmul ||
      inst `OP8bits == `OPslt ||
      inst `OP8bits == `OPand ||
      inst `OP8bits == `OPor ||
      inst `OP8bits == `OPxor ||
      inst `OP8bits == `OPshift ||
      inst `OP8bits == `OPaddf ||
      inst `OP8bits == `OPmulf ||
      inst `OP8bits == `OPsltf ||
      inst `OP8bits == `OPrecip ||
      inst `OP8bits == `OPfloat ||
      inst `OP8bits == `OPint ||
      inst `OP8bits == `OPcopy ||
      inst `OP8bits == `OPload ||
      inst `OP4bits == `OPlex ||
      inst `OP4bits == `OPlhi
    ));
  endfunction

  function setspc;
    input `INST inst;
    setspc = (
      inst `OP8bits == `OPjumpr ||
      inst `OP4bits == `OPbrf ||
      inst `OP4bits == `OPbrt
    );
  endfunction

  // function setsz;
  // input `INST inst;
  // setsz = ((inst `CC == `S) && setsrd(inst));
  // endfunction

  // function iscond;
  // input `INST inst;
  // iscond = ((inst `CC == `NE) || (inst `CC == `EQ));
  // endfunction

  // function usesim;
  // input `INST inst;
  // usesim = ((inst `IORR) && (inst `OP <= `OPSTR));
  // endfunction

  function usesrd;
    input `WORD inst;
    usesrd = (
      inst `OP8bits == `OPneg ||
      inst `OP8bits == `OPnegf ||
      inst `OP8bits == `OPnot ||
      inst `OP8bits == `OPadd ||
      inst `OP8bits == `OPmul ||
      inst `OP8bits == `OPslt ||
      inst `OP8bits == `OPand ||
      inst `OP8bits == `OPor ||
      inst `OP8bits == `OPxor ||
      inst `OP8bits == `OPshift ||
      inst `OP8bits == `OPaddf ||
      inst `OP8bits == `OPmulf ||
      inst `OP8bits == `OPsltf ||
      inst `OP8bits == `OPrecip ||
      inst `OP8bits == `OPfloat ||
      inst `OP8bits == `OPint ||
      inst `OP8bits == `OPstore
    );
  endfunction

  function usesrn;
    input `WORD inst;
    usesrn = (
      inst `OP8bits == `OPadd ||
      inst `OP8bits == `OPmul ||
      inst `OP8bits == `OPslt ||
      inst `OP8bits == `OPand ||
      inst `OP8bits == `OPor ||
      inst `OP8bits == `OPxor ||
      inst `OP8bits == `OPshift ||
      inst `OP8bits == `OPaddf ||
      inst `OP8bits == `OPmulf ||
      inst `OP8bits == `OPsltf ||
      inst `OP8bits == `OPrecip ||
      inst `OP8bits == `OPfloat ||
      inst `OP8bits == `OPint ||
      inst `OP8bits == `OPstore
    );
  endfunction

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

      if (pendpc) begin
        // waiting... pc doesn't change
        ir0 <= `NOP;
        pc <= tpc;
      end else begin
        // if (ir[13:12] == 0) begin
        //   // // PRE operation
        //   // havepre <= 1;
        //   // prefix <= ir[11:0];
        //   ir0 <= `NOP;
        // end else begin
          // if (usesim(ir)) begin
          //   // extend immediate
          //   // im0 <= {{12{ir[3]}}, ir `RN};
          //   // havepre <= 0;
          
          ir0 <= ir;
          end
          pc <= tpc + 1;
        // end
        pc0 <= tpc;
      end
  end

  // stage 1: register read
  always @(posedge clk) begin
    if ((ir0 != `NOP) && setsrd(ir1) && ((usesrd(ir0)) || (usesrn(ir0)))) begin
        // ((usesrd(ir0) && (ir0 `RD == ir1 `RD)) ||
        // (usesrn(ir0) && (ir0 `RN == ir1 `RD)))) begin
      // stall waiting for register value
      wait1 = 1;
      ir1 <= `NOP;
    end else begin
      // all good, get operands (even if not needed)
      wait1 = 0;
      // rd1 <= ((ir0 `RD == 15) ? pc0 : r[ir0 `RD]);
      rd1 <= r[ir0 `RD];
      // rn1 <= (usesim(ir0) ? im0 :
      //         ((ir0 `RN == 15) ? pc0 : r[ir0 `RN]));
      rn1 <= r[ir0 `RN];
      ir1 <= ir0;
    end
  end

  // stage 2: ALU, data memory access, store in register
  always @(posedge clk) begin
    if ((ir1 == `NOP)) begin
      // condition says nothing happens
      jump <= 0;
    end else begin
      // let the instruction execute
      case (ir1 `OP4bits)
        `OPsys: 
            begin
              halt <= 1;
              // s <= `Start;
            end
            
          // Start Qat
          `OPsingleQ:
            begin
              case (ir1 [11:8])
                `OPnotQ: begin halt <= 1; end
                `OPoneQ: begin halt <= 1; end
                `OPzeroQ: begin halt <= 1; end
              endcase
            end

          `OPhadQ: begin halt <= 1; end

          `OPtwoQ:
            begin
              pc <= pc + 1;
              case (ir1 [11:8])
                `OPcnotQ: begin halt <= 1; end
                `OPswapQ: begin halt <= 1; end
              endcase
            end

          `OPthreeQ:
            begin
              pc <= pc + 1;
              case (ir1 [11:8])
                `OPccnotQ: begin halt <= 1; end
                `OPcswapQ: begin halt <= 1; end
                `OPandQ: begin halt <= 1; end
                `OPorQ: begin halt <= 1; end
                `OPxorQ: begin halt <= 1; end
              endcase
            end

          `OPmeasQ: begin halt <= 1; end

          `OPnextQ: begin halt <= 1; end
          // End Qat

          `OPoneReg: 
            begin
              case (ir1 [15:8])
                `OPjumpr: begin target <= r[ir1 `DestReg]; s <= `Start; jump <= 1; end
                `OPneg:   begin r[ir1 `DestReg] <= -r[ir1 `DestReg]; s <= `Start; end
                `OPnegf:  begin r[ir1 `DestReg] <= negfRes; s <= `Start; end 
                `OPnot:   begin r[ir1 `DestReg] <= ~r[ir1 `DestReg]; s <= `Start; end
              endcase
            end

          `OPbrf: begin tpc <= tpc + ((|r[ir1 `SECOND4]) ? (16'b0) : ((ir1[7]) ? {{8{1'b1}}, (ir1 `BOTTOM8)} - 1 : ir1 `BOTTOM8 - 1)); s <= `Start; end // subtract 1 to offset incrementing the pc in Decode
          `OPbrt: begin tpc <= tpc + ((~|r[ir1 `SECOND4]) ? (16'b0) : ((ir1[7]) ? {{8{1'b1}}, (ir1 `BOTTOM8)} - 1 : ir1 `BOTTOM8 - 1)); s <= `Start; end // subtract 1 to offset incrementing the pc in Decode

          `OPlex: 
          begin 
            r[ir1 `SECOND4] <= {{8{ir1[7]}}, ir1 `BOTTOM8}; s <= `Start; 
          end

          `OPlhi: begin r[ir `SECOND4] `TOP8 <= ir `BOTTOM8; s <= `Start; end 

          `OPnorms: 
            begin
              case (ir1 [15:8])
                `OPadd: begin r[ir1 `DestReg] <= (r[ir1 `DestReg] + r[ir1 `SourceReg]); s <= `Start; end
                `OPmul: begin r[ir1 `DestReg] <= r[ir1 `DestReg] * r[ir1 `SourceReg]; s <= `Start; end
                `OPslt: 
                begin 
                  // r[ir1 `DestReg] <= (r[ir1 `DestReg] < r[ir1 `SourceReg] ? 16'b1 : 16'b0); s <= `Start; 
                  sltCheck = r[ir1 `DestReg] - r[ir1 `SourceReg];
                  r[ir1 `DestReg] <= ((sltCheck[15]) ? 16'b1 : 16'b0); s <= `Start; 
                end
                `OPand: begin r[ir1 `DestReg] <= r[ir1 `DestReg] & r[ir1 `SourceReg]; s <= `Start; end
                `OPor:  begin r[ir1 `DestReg] <= r[ir1 `DestReg] | r[ir1 `SourceReg]; s <= `Start; end
                `OPxor: begin r[ir1 `DestReg] <= r[ir1 `DestReg] ^ r[ir1 `SourceReg]; s <= `Start; end
                `OPshift: begin r[ir1 `DestReg] <= ((r[ir1 `SourceReg][15] == 0) ? (r[ir1 `DestReg] << r[ir1 `SourceReg]) : (r[ir1 `DestReg] >> -r[ir1 `SourceReg])); s <= `Start;
                end
              endcase
            end
      
          `OPfloats:
            begin
              case (ir1 [15:8])
                `OPaddf:  begin r[ir1 `DestReg] <= addfRes; s <= `Start; end
                `OPmulf:  begin r[ir1 `DestReg] <= multfRes; s <= `Start; end
                `OPsltf:  begin r[ir1 `DestReg] <= sltfRes; s <= `Start; end
                `OPrecip: begin r[ir1 `DestReg] <= recipRes; s <= `Start; end
                `OPfloat: begin r[ir1 `DestReg] <= floatRes; s <= `Start; end
                `OPint:   begin r[ir1 `DestReg] <= intRes; s <= `Start; end
                `OPcopy: begin r[ir1 `DestReg] <= r[ir1 `SourceReg]; s <= `Start; end
                `OPstore: begin data[r[ir1 `SourceReg]] <= r[ir1 `DestReg]; s <= `Start; end
                `OPload: begin r[ir1 `DestReg] <= data[r[ir1 `SourceReg]]; s <= `Start; end
              endcase
            end


        default: halt <= 1; // make it stop
      endcase

      // update z flag if we should
      // if (setsz(ir1)) zreg <= (res == 0);
      if (setspc(ir1)) begin
        jump <= 1;
      end else begin
        jump <= 0;
      end
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
// endcase

  // always @(posedge clk) begin
  //   case (s)
  //     `Start: 
  //       begin 
  //         ir <= text[pc]; 
  //         // nextInstruction <= text[pc + 1];
  //         s <= `Decode;
  //       end 
      
  //     `Decode: 
  //       begin
  //         pc <= pc + 1;
  //         s <= ir `Op0;
  //         ir1 [11:8] <= ir `Reg0;
  //       end

  //     `OPsys: 
  //       begin
  //         halt <= 1;
  //         s <= `Start;
  //       end
        
  //     // Start Qat
  //     `OPsingleQ:
  //       begin
  //         case (ir1 [11:8])
  //           `OPnotQ: begin s <= `OPsys; end
  //           `OPoneQ: begin s <= `OPsys; end
  //           `OPzeroQ: begin s <= `OPsys; end
  //         endcase
  //       end

  //     `OPhadQ: begin s <= `OPsys; end

  //     `OPtwoQ:
  //       begin
  //         pc <= pc + 1;
  //         case (ir1 [11:8])
  //           `OPcnotQ: begin s <= `OPsys; end
  //           `OPswapQ: begin s <= `OPsys; end
  //         endcase
  //       end

  //     `OPthreeQ:
  //       begin
  //         pc <= pc + 1;
  //         case (ir1 [11:8])
  //           `OPccnotQ: begin s <= `OPsys; end
  //           `OPcswapQ: begin s <= `OPsys; end
  //           `OPandQ: begin s <= `OPsys; end
  //           `OPorQ: begin s <= `OPsys; end
  //           `OPxorQ: begin s <= `OPsys; end
  //         endcase
  //       end

  //     `OPmeasQ: begin s <= `OPsys; end

  //     `OPnextQ: begin s <= `OPsys; end
  //     // End Qat

  //     `OPoneReg: 
  //       begin
  //         case (ir1 [11:8])
  //           `OPjumpr: begin pc <= r[ir `DestReg]; s <= `Start; end
  //           `OPneg:   begin r[ir `DestReg] <= -r[ir `DestReg]; s <= `Start; end
  //           `OPnegf:  begin r[ir `DestReg] <= negfRes; s <= `Start; end 
  //           `OPnot:   begin r[ir `DestReg] <= ~r[ir `DestReg]; s <= `Start; end
  //         endcase
  //       end

  //     `OPbrf: begin pc <= pc + ((|r[ir `SECOND4]) ? (16'b0) : ((ir[7]) ? {{8{1'b1}}, (ir `BOTTOM8)} - 1 : ir `BOTTOM8 - 1)); s <= `Start; end // subtract 1 to offset incrementing the pc in Decode
  //     `OPbrt: begin pc <= pc + ((~|r[ir `SECOND4]) ? (16'b0) : ((ir[7]) ? {{8{1'b1}}, (ir `BOTTOM8)} - 1 : ir `BOTTOM8 - 1)); s <= `Start; end // subtract 1 to offset incrementing the pc in Decode

  //     `OPlex: 
  //     begin 
  //       r[ir `SECOND4] <= {{8{ir[7]}}, ir `BOTTOM8}; s <= `Start; 
  //     end

  //     `OPlhi: begin r[ir `SECOND4] `TOP8 <= ir `BOTTOM8; s <= `Start; end 

  //     `OPnorms: 
  //       begin
  //         case (ir1 [11:8])
  //           `OPadd: begin r[ir `DestReg] <= r[ir `DestReg] + r[ir `SourceReg]; s <= `Start; end
  //           `OPmul: begin r[ir `DestReg] <= r[ir `DestReg] * r[ir `SourceReg]; s <= `Start; end
  //           `OPslt: 
  //           begin 
  //             // r[ir `DestReg] <= (r[ir `DestReg] < r[ir `SourceReg] ? 16'b1 : 16'b0); s <= `Start; 
  //             sltCheck = r[ir `DestReg] - r[ir `SourceReg];
  //             r[ir `DestReg] <= ((sltCheck[15]) ? 16'b1 : 16'b0); s <= `Start; 
  //           end
  //           `OPand: begin r[ir `DestReg] <= r[ir `DestReg] & r[ir `SourceReg]; s <= `Start; end
  //           `OPor:  begin r[ir `DestReg] <= r[ir `DestReg] | r[ir `SourceReg]; s <= `Start; end
  //           `OPxor: begin r[ir `DestReg] <= r[ir `DestReg] ^ r[ir `SourceReg]; s <= `Start; end
  //           `OPshift: begin r[ir `DestReg] <= ((r[ir `SourceReg][15] == 0) ? (r[ir `DestReg] << r[ir `SourceReg]) : (r[ir `DestReg] >> -r[ir `SourceReg])); s <= `Start;
  //           end
  //         endcase
  //       end
   
  //     `OPfloats:
  //       begin
  //         case (ir1 [11:8])
  //           `OPaddf:  begin r[ir `DestReg] <= addfRes; s <= `Start; end
  //           `OPmulf:  begin r[ir `DestReg] <= multfRes; s <= `Start; end
  //           `OPsltf:  begin r[ir `DestReg] <= sltfRes; s <= `Start; end
  //           `OPrecip: begin r[ir `DestReg] <= recipRes; s <= `Start; end
  //           `OPfloat: begin r[ir `DestReg] <= floatRes; s <= `Start; end
  //           `OPint:   begin r[ir `DestReg] <= intRes; s <= `Start; end
  //           `OPcopy: begin r[ir `DestReg] <= r[ir `SourceReg]; s <= `Start; end
  //           `OPstore: begin data[r[ir `SourceReg]] <= r[ir `DestReg]; s <= `Start; end
  //           `OPload: begin r[ir `DestReg] <= data[r[ir `SourceReg]]; s <= `Start; end
  //         endcase
  //       end
  //   endcase
  // end
endmodule

module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted, reset, clk);
initial begin
  $dumpfile("dump.txt");
  $dumpvars(0, PE, PE.r[0], PE.r[1], PE.r[2]); // would normally trace 0, PE
  #1 reset = 1;
  #1 reset = 0;
  while (!halted) begin
    #1 clk = 1;
    #1 clk = 0;
  end
  $finish;
end
endmodule
