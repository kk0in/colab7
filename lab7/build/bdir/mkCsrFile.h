/*
 * Generated by Bluespec Compiler, version 2021.12.1 (build fd50140)
 * 
 * On Wed Jun 15 06:28:21 KST 2022
 * 
 */

/* Generation options: */
#ifndef __mkCsrFile_h__
#define __mkCsrFile_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkCsrFile module */
class MOD_mkCsrFile : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_ConfigReg<tUInt32> INST_coreId;
  MOD_Reg<tUInt8> INST_csrFifo_clearReq_ehrReg;
  MOD_Wire<tUInt8> INST_csrFifo_clearReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_csrFifo_clearReq_ignored_wires_1;
  MOD_Reg<tUInt8> INST_csrFifo_clearReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_csrFifo_clearReq_virtual_reg_1;
  MOD_Wire<tUInt8> INST_csrFifo_clearReq_wires_0;
  MOD_Wire<tUInt8> INST_csrFifo_clearReq_wires_1;
  MOD_Reg<tUWide> INST_csrFifo_data_0;
  MOD_Reg<tUWide> INST_csrFifo_data_1;
  MOD_Reg<tUInt8> INST_csrFifo_deqP;
  MOD_Reg<tUInt8> INST_csrFifo_deqReq_ehrReg;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_ignored_wires_1;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_csrFifo_deqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_csrFifo_deqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_csrFifo_deqReq_virtual_reg_2;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_wires_0;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_wires_1;
  MOD_Wire<tUInt8> INST_csrFifo_deqReq_wires_2;
  MOD_Reg<tUInt8> INST_csrFifo_empty;
  MOD_Reg<tUInt8> INST_csrFifo_enqP;
  MOD_Reg<tUWide> INST_csrFifo_enqReq_ehrReg;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_ignored_wires_0;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_ignored_wires_1;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_csrFifo_enqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_csrFifo_enqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_csrFifo_enqReq_virtual_reg_2;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_wires_0;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_wires_1;
  MOD_Wire<tUWide> INST_csrFifo_enqReq_wires_2;
  MOD_Reg<tUInt8> INST_csrFifo_full;
  MOD_Reg<tUInt32> INST_cycles;
  MOD_ConfigReg<tUInt32> INST_numBPMiss;
  MOD_ConfigReg<tUInt32> INST_numCtr;
  MOD_ConfigReg<tUInt32> INST_numHitBr;
  MOD_ConfigReg<tUInt32> INST_numHitJ;
  MOD_ConfigReg<tUInt32> INST_numHitJr;
  MOD_ConfigReg<tUInt32> INST_numInsts;
  MOD_ConfigReg<tUInt32> INST_numMem;
  MOD_ConfigReg<tUInt32> INST_numMissBr;
  MOD_ConfigReg<tUInt32> INST_numMissJ;
  MOD_ConfigReg<tUInt32> INST_numMissJr;
  MOD_ConfigReg<tUInt8> INST_startReg;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_ehrReg;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_ignored_wires_1;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_clearReq_virtual_reg_1;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_clearReq_wires_1;
  MOD_Reg<tUInt64> INST_toHostFifo_data_0;
  MOD_Reg<tUInt64> INST_toHostFifo_data_1;
  MOD_Reg<tUInt8> INST_toHostFifo_deqP;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_ehrReg;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_1;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_toHostFifo_deqReq_virtual_reg_2;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_0;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_1;
  MOD_Wire<tUInt8> INST_toHostFifo_deqReq_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_empty;
  MOD_Reg<tUInt8> INST_toHostFifo_enqP;
  MOD_Reg<tUInt64> INST_toHostFifo_enqReq_ehrReg;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_0;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_1;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_toHostFifo_enqReq_virtual_reg_2;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_0;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_1;
  MOD_Wire<tUInt64> INST_toHostFifo_enqReq_wires_2;
  MOD_Reg<tUInt8> INST_toHostFifo_full;
 
 /* Constructor */
 public:
  MOD_mkCsrFile(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_toHostFifo_empty__h6171;
  tUInt8 DEF_toHostFifo_full__h6133;
  tUInt8 DEF_startReg_read____d173;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_csrFifo_clearReq_wires_0_whas__09_THEN_csrF_ETC___d112;
  tUInt8 DEF_IF_toHostFifo_clearReq_wires_0_whas__3_THEN_to_ETC___d26;
  tUWide DEF_csrFifo_enqReq_wires_2_wget____d88;
  tUWide DEF_csrFifo_enqReq_wires_1_wget____d90;
  tUWide DEF_csrFifo_enqReq_wires_0_wget____d92;
  tUWide DEF_csrFifo_enqReq_ehrReg___d93;
  tUInt64 DEF_toHostFifo_enqReq_wires_1_wget____d4;
  tUInt64 DEF_toHostFifo_enqReq_wires_0_wget____d6;
  tUInt64 DEF_toHostFifo_enqReq_ehrReg___d7;
  tUInt32 DEF_x__h15201;
  tUInt32 DEF_x__h15199;
  tUInt32 DEF_x__h15197;
  tUInt32 DEF_x__h15187;
  tUInt32 DEF__read__h226;
  tUInt32 DEF_x__h15059;
  tUInt32 DEF_x__h15063;
  tUInt32 DEF__read__h110;
  tUInt32 DEF_x__h14791;
  tUInt8 DEF_csrFifo_clearReq_wires_0_whas____d109;
  tUInt8 DEF_csrFifo_clearReq_wires_0_wget____d110;
  tUInt8 DEF_csrFifo_clearReq_ehrReg__h10833;
  tUInt8 DEF_csrFifo_deqReq_ehrReg__h9840;
  tUInt8 DEF_csrFifo_enqReq_wires_1_whas____d89;
  tUInt8 DEF_csrFifo_enqReq_wires_0_whas____d91;
  tUInt8 DEF_toHostFifo_clearReq_wires_0_whas____d23;
  tUInt8 DEF_toHostFifo_clearReq_wires_0_wget____d24;
  tUInt8 DEF_toHostFifo_clearReq_ehrReg__h4086;
  tUInt8 DEF_toHostFifo_deqReq_ehrReg___d17;
  tUInt8 DEF_toHostFifo_enqReq_wires_1_whas____d3;
  tUInt8 DEF_toHostFifo_enqReq_wires_0_whas____d5;
  tUInt8 DEF_x__h14964;
  tUWide DEF_csrFifo_enqReq_ehrReg_3_BITS_75_TO_0___d165;
  tUWide DEF_csrFifo_enqReq_wires_1_wget__0_BITS_75_TO_0___d163;
  tUWide DEF_csrFifo_enqReq_wires_0_wget__2_BITS_75_TO_0___d164;
  tUWide DEF_IF_csrFifo_enqReq_wires_2_whas__7_THEN_csrFifo_ETC___d96;
  tUWide DEF_IF_csrFifo_enqReq_wires_1_whas__9_THEN_csrFifo_ETC___d95;
  tUWide DEF_IF_csrFifo_enqReq_wires_0_whas__1_THEN_csrFifo_ETC___d94;
  tUWide DEF_IF_csrFifo_enqReq_virtual_reg_2_read__19_OR_IF_ETC___d169;
  tUWide DEF_IF_IF_csrFifo_enqReq_wires_1_whas__9_THEN_csrF_ETC___d168;
  tUWide DEF_IF_csrFifo_enqReq_wires_1_whas__9_THEN_csrFifo_ETC___d167;
  tUWide DEF_IF_csrFifo_enqReq_wires_0_whas__1_THEN_csrFifo_ETC___d166;
  tUInt64 DEF_IF_toHostFifo_enqReq_wires_1_whas_THEN_toHostF_ETC___d9;
  tUInt8 DEF_IF_csrFifo_deqReq_wires_1_whas__9_THEN_csrFifo_ETC___d105;
  tUInt8 DEF_IF_toHostFifo_deqReq_wires_1_whas__3_THEN_toHo_ETC___d19;
  tUWide DEF__0_CONCAT_DONTCARE___d172;
 
 /* Rules */
 public:
  void RL_toHostFifo_enqReq_canonicalize();
  void RL_toHostFifo_deqReq_canonicalize();
  void RL_toHostFifo_clearReq_canonicalize();
  void RL_toHostFifo_canonicalize();
  void RL_csrFifo_enqReq_canonicalize();
  void RL_csrFifo_deqReq_canonicalize();
  void RL_csrFifo_clearReq_canonicalize();
  void RL_csrFifo_canonicalize();
  void RL_count();
 
 /* Methods */
 public:
  void METH_start(tUInt32 ARG_start_id);
  tUInt8 METH_RDY_start();
  tUInt8 METH_started();
  tUInt8 METH_RDY_started();
  tUInt32 METH_rd(tUInt32 ARG_rd_idx);
  tUInt8 METH_RDY_rd();
  void METH_wr(tUInt32 ARG_wr_idx, tUInt32 ARG_wr_val);
  tUInt8 METH_RDY_wr();
  tUInt64 METH_cpuToHost();
  tUInt8 METH_RDY_cpuToHost();
  void METH_incInstTypeCnt(tUInt8 ARG_incInstTypeCnt_inst);
  tUInt8 METH_RDY_incInstTypeCnt();
  void METH_incBPMissCnt();
  tUInt8 METH_RDY_incBPMissCnt();
  void METH_incMissInstTypeCnt(tUInt8 ARG_incMissInstTypeCnt_inst);
  tUInt8 METH_RDY_incMissInstTypeCnt();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCsrFile &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkCsrFile &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkCsrFile &backing);
};

#endif /* ifndef __mkCsrFile_h__ */
