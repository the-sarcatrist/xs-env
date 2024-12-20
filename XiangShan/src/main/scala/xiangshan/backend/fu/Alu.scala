/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTree, LookupTreeDefault, ParallelMux, SignExt, ZeroExt}
import utils._
import xiangshan._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import circt.stage.ChiselStage


///////////////////////////////////////////////////capstone class ////////////////////
  //mlabaf//capstone
  class cap_metadata_t extends Bundle{
    val ty   = UInt(3.W) //new cap_type_t() // ;    // 3 bits
    val perm = UInt(3.W)//new cap_perm_t() // ;  // 3 bits
  }// extra metadata, 6 bits

  // Only use struct when signals have same direction
  // exception
  class exception_t extends Bundle{
    val causeriscv =UInt(XLEN.W)  // cause of exception
    val tvalriscv  =UInt(XLEN.W)  // additional information of causing exception (e.g.: instruction causing it),
    // address of LD/ST fault
    val is_virt =UInt(1.W)  // Capstone: is this a virtual interrupt?
    val valid   =UInt(1.W)
  } 
///////////////////////////////////cap_fat/////////////////////////////////////
  class cap_fat_bounds_t extends Bundle {
   //3*64 bits */
    val top    =UInt(64.W)
    val base   =UInt(64.W)//(Vec(64,Bool()))      
    val cursor =UInt(64.W)//(Vec(64,Bool()))  
   }

  // not including the tag bit
  class cap_fat_t  extends Bundle{
    val renode_id  = cap_renode_id_t// 31 bits
    val meta   = new cap_metadata_t // 6 bits
    val reg_id = UInt(5.W)//(Vec(5,Bool()))// 5 bits
    val async  = UInt(1.W)//Bool   // 1 bit
    val padding= UInt(21.W)//Vec(21,Bool()) // 21 bits
    val bounds = new cap_fat_bounds_t // 64 * 3 bits
   } // fat 256-bit capability
///////////////////////////////////cap_cc//////////////////////////////////////////////
    // low to high
    // the raw bit representation of a compressed bound
    // 27 bits + cursor
  class cap_cc_bounds_t extends Bundle{
    val iE    = UInt(1.W)//(Bool())
    val t     = UInt(9.W) //(Vec(9,Bool())) 
    val tE    = UInt(3.W)//(Vec(3,Bool())) 
    val b     = UInt(11.W)//(Vec(11,Bool())) 
    val bE    = UInt(3.W)//(Vec(3,Bool())) 
    val cursor= UInt(XLEN.W)//(Vec(XLEN,Bool())) 
  }
// compressed 128-bit capability
  class cap_cc_t  extends Bundle{
    val renode_id   = cap_renode_id_t// 31 bits
    val meta         = new cap_metadata_t()// 6 bits
    val bounds       = new cap_cc_bounds_t()// 27 + 64 = 91 bits
   }

  // pack the cap and tag bits to bounce around the different stages
  // the valid bit is used as sort of a write enable
  class  fu_data_t extends Bundle{
    val operand_a = UInt (XLEN.W)
    val operand_b = UInt (XLEN.W)
    val imm       = UInt (XLEN.W)
    val cap_a     = UInt (128.W)//clen_t
    val cap_b     = UInt (128.W)//clen_t
    val cap_c     = UInt (128.W)//clen_t
    val tag_a     = UInt (1.W)
    val tag_b     = UInt (1.W)
    val tag_c     = UInt (1.W)
    val valid     = UInt (1.W) 
    val rd        = UInt (5.W)
    val rs1       = UInt (5.W)
    val rs2       = UInt (5.W)
    val trans_id  = UInt (TRANS_ID_BITS.W) 

   }
 class  cap_result_t extends Bundle
 {
    val cap   = UInt (128.W)//clen_t
    val tag   = UInt (1.W)
    val valid = UInt (1.W)
 }

  // a domain switch request
  class  dom_switch_req_t extends Bundle{
    val is_full       = UInt(1.W)
    val is_return     = UInt(1.W) // if this is return, ra is overwritten
    val dom_base      = UInt(XLEN.W)
    val out_dom_base  = UInt(XLEN.W)
    val pc_out        = UInt(XLEN.W)// pc to swap out
    val trans_id      = UInt(TRANS_ID_BITS.W)
  } 

  class  capstone_mode_switch_t extends Bundle{
    val cap0    = UInt(128.W)//(CLEN.W)
    val cap1    = UInt(128.W)
    val pc_cap  = UInt(128.W)
    val valid   = UInt (1.W)
  }



//////////////////////////////////////node/////////////////
class node_mut_t extends Bundle{
   val node_id = cap_renode_id_t 
   val node_op = UInt (4.W)//node_mut_type_t 
   val alloc   = UInt (1.W)  
   val new_node_id = cap_renode_id_t 
} 

  // mutation does not require a response

class node_query_t extends Bundle{
  val synchronous = UInt (1.W)  // is this synchronous?
  val trans_id    = UInt (TRANS_ID_BITS.W)// only effective when async
  val node_id     = cap_renode_id_t 
  } 
 
class node_query_resp_t extends Bundle{
  val synchronous = UInt (1.W) 
  val trans_id    = UInt (TRANS_ID_BITS.W) 
  val r_valid     = UInt (1.W)  // is this node valid?
  } 

class send_node_query_bundle extends Bundle
  {
    val revnode_id                 = Input(cap_renode_id_t)
    val node_query_sent_q          = Input(UInt(1.W))
    val node_query_ready_i         = Input(UInt(1.W))
    val node_query_resp_valid_i    = Input(UInt(1.W)) 
    val node_query_resp_received_q = Input(UInt(1.W)) 
    val synchronous                = Input(UInt(1.W))
    val node_query_resp_i          = Input(new node_query_resp_t)

    val node_query_sent_d          = Output(UInt(1.W))
    val node_query_valid_o         = Output(UInt(1.W)) 
    val rd_result_o                = Output(new cap_result_t )  
    val node_query_o               = Output(new node_query_t )     
    val node_query_resp_result_d   = Output(UInt(1.W))
    val node_query_resp_received_d = Output(UInt(1.W))
    val node_query_resp_result_q   = Output(UInt(1.W))
  }
   
class send_node_query extends Module
  {
    val ins    =IO(new send_node_query_bundle )

     ins.node_query_sent_d          := 0.U
     ins.node_query_valid_o         := 0.U 
     ins.rd_result_o.cap            := 0.U 
     ins.rd_result_o.valid          := 0.U 
     ins.rd_result_o.tag            := 0.U 
     ins.node_query_o.synchronous   := 0.U 
     ins.node_query_o.trans_id      := 0.U 
     ins.node_query_o.node_id       := 0.U    
     ins.node_query_resp_result_d   := 0.U
     ins.node_query_resp_received_d := 0.U
     ins.node_query_resp_result_q   := 0.U

    ins.rd_result_o.tag:=0.U
    ins.rd_result_o.valid:=0.U

    ins.node_query_sent_d := ins.node_query_sent_q  

    when (ins.node_query_sent_q=== 0.U) 
      {
      when (ins.node_query_ready_i=== 1.U) 
        {
        // ins.node_query_valid_o   := RegNext(1.U(1.W),0.U(1.W))
        ins.node_query_valid_o   := 1.U(1.W)
        ins.node_query_o.node_id := ins.revnode_id
        // ins.node_query_o.node_id := RegNext(ins.revnode_id, 0.U(31.W))
        ins.node_query_o.synchronous := ins.synchronous
        ins.node_query_o.trans_id := 0.U

        ins.node_query_sent_d := 1.U(1.W)
        
        when(ins.synchronous===1.U) 
        {
          ins.node_query_resp_received_d := 1.U(1.W)
          // val ins_w  =IO(new wait_for_this_cycle_bundle )
          // val wait_for_this_cycle _ins =Module(new wait_for_this_cycle )

          // ins_w <> wait_for_this_cycle _ins.ins
          // wait_for_this_cycle(); // for a synchronous query, we still need to wait for the result
        }
       } 
     .otherwise
      {

      //  val ins_w  =IO(new wait_for_this_cycle_bundle )
      //  val wait_for_this_cycle _ins =Module(new wait_for_this_cycle )

      //  ins_w <> wait_for_this_cycle _ins.ins
        // wait_for_this_cycle();
      }
     }

     .elsewhen (ins.synchronous===1.U) 
      {
      when (ins.node_query_resp_received_q===0.U) 
        {
        when ((ins.node_query_resp_valid_i===1.U) && (ins.node_query_resp_i.synchronous===1.U)) 
          {
          ins.node_query_resp_received_d := 1.U(1.W)
          ins.node_query_resp_result_d   := ins.node_query_resp_i.r_valid
          ins.rd_result_o.cap            := (ins.node_query_resp_i.r_valid)
          }
          .otherwise
          { 
          //  val ins_w  =IO(new wait_for_this_cycle_bundle )
          //  val wait_for_this_cycle _ins =Module(new wait_for_this_cycle )

          //  ins_w <> wait_for_this_cycle _ins.ins
          // wait_for_this_cycle()
          }
        }      
        .otherwise
        {
        ins.rd_result_o.cap := ins.node_query_resp_result_q
        }
      }

    //  when ((ins.synchronous=/=1.U) &&  (ins.node_query_sent_q=/= 0.U)) 
    //  {
    //  ins.node_query_sent_d          := 0.U
    //  ins.node_query_valid_o         := 0.U 
    //  ins.rd_result_o.cap            := 0.U 
    //  ins.rd_result_o.valid          := 0.U 
    //  ins.rd_result_o.tag            := 0.U 
    //  ins.node_query_o.synchronous   := 0.U 
    //  ins.node_query_o.trans_id      := 0.U 
    //  ins.node_query_o.node_id       := 0.U    
    //  ins.node_query_resp_result_d   := 0.U
    //  ins.node_query_resp_received_d := 0.U
    //  ins.node_query_resp_result_q   := 0.U
    //  } 
}

class send_node_mut_bundle extends Bundle 
  {
    // this reuses capstone_valid_o so it's safe to always set node_mut_valid_o
    val revnode_id       = Input( cap_renode_id_t)
    val node_alloc_node_id_cur = Input( cap_renode_id_t)
    val mut_ty           = Input(UInt(4.W))
    val have_alloc       = Input(UInt(1.W))

    val node_mut_valid_o = Output(UInt(1.W))
    val node_mut_o       = Output(new node_mut_t)   
  }  

class send_node_mut extends Module 
  {
     val ins  =IO(new send_node_mut_bundle )

    // this reuses capstone_valid_o so it's safe to always set node_mut_valid_o
    // val revnode_id       = IO(Input( cap_renode_id_t))
    // val node_alloc_node_id_cur = IO(Input( cap_renode_id_t))
    // val mut_ty           = IO(Input(UInt(4.W)))
    // val have_alloc       = IO(Input(UInt(1.W)))
    // val node_mut_valid_o = IO(Output(UInt(1.W)))
    // val node_mut_o       = IO(Output(new node_mut_t))
     
     ins.node_mut_valid_o  := 1.U
     ins.node_mut_o.node_id:= ins.revnode_id
     ins.node_mut_o.node_op:= ins.mut_ty
     ins.node_mut_o.alloc  := ins.have_alloc
     ins.node_mut_o.new_node_id:= ins.node_alloc_node_id_cur
  }

class send_node_alloc_bundle extends Bundle 
  {
    // this reuses capstone_valid_o so it's safe to always set node_mut_valid_o
    val node_alloc_state_d      = Output( UInt(4.W))//IO(Output(new node_mut_type_t))
    val node_alloc_valid_o      = Output(UInt(1.W)) 
    val node_alloc_node_id_d    = Output(UInt(1.W)) 

    val node_alloc_ready_i      = Input(UInt(1.W))
    val node_alloc_state_q      = Input(UInt(4.W))
    val node_alloc_resp_valid_i = Input(UInt(1.W))
    val node_alloc_node_id_i    = Input(UInt(1.W))
  }

class send_node_alloc extends Module 
  {
    val ins  =IO(new send_node_alloc_bundle )

    // this reuses capstone_valid_o so it's safe to always set node_mut_valid_o
    ins.node_alloc_state_d  :=0.U
    ins.node_alloc_valid_o  :=0.U
    ins.node_alloc_node_id_d:=0.U

   switch (ins.node_alloc_state_q)
    {
      is (mut_t.NODE_ALLOC_IDLE) 
      {
        when (ins.node_alloc_ready_i===1.U) 
          {
          ins.node_alloc_valid_o := 1.U
          ins.node_alloc_state_d := mut_t.NODE_ALLOC_SENT
          }
      }
      is (mut_t.NODE_ALLOC_SENT) 
      {
        when(ins.node_alloc_resp_valid_i===1.U)
        {
          ins.node_alloc_node_id_d:=ins.node_alloc_node_id_i
          ins.node_alloc_state_d  :=mut_t.NODE_ALLOC_RECEIVED

        }
      }
      is (mut_t.NODE_ALLOC_RECEIVED) 
      {
        // special treatment later
      }  
    }
  }

class wait_for_this_cycle_bundle extends Bundle 
  {
    val capstone_valid_o    = Output(UInt(1.W))
    val cms_result_o        = Output(new capstone_mode_switch_t) 
    val dom_switch_valid_o  = Output(UInt(1.W))
    val valid_last_n        = Output(UInt(1.W))
    val pc_last_n           = Output(UInt(VLEN.W))     
    val fu_data_last_n      = Output(new fu_data_t)
    val pc_cur              = Input(UInt(VLEN.W)) 
    val fu_data_cur         = Input(new fu_data_t)
  }
class wait_for_this_cycle extends Module 
 {
  val ins  =IO(new wait_for_this_cycle_bundle )
  ins.capstone_valid_o := 0.U
  ins.cms_result_o     := 0.U
  ins.dom_switch_valid_o := 0.U
  ins.valid_last_n      := 0.U
  ins.pc_last_n         := ins.pc_cur
  ins.fu_data_last_n    := ins.fu_data_cur
 }    
  
////////////////////////////////////////////////////////capstone class end //////////////////////////////////

/////////////////////////compression/decompression//////////////
// Utilty functions for querying and manipulating the capabilities
// converts a compressed bounds into a fat bounds
//class cap_bounds_uncompres extends Bundle {
class  cap_bounds_uncompres extends Module {
  val cap_cc_bounds  =IO(Input(new cap_cc_bounds_t))
  val cap_fat_bounds =IO(Output(new cap_fat_bounds_t))

  val E       = WireInit(0.U(6.W))
  val B,B1    = WireInit(0.U(14.W))
  val T,T1,T2 = WireInit(0.U(14.W))
  
  val carry_out = WireInit(0.U(1.W))
  val msb  =  WireInit(0.U(1.W))//Bool()
  val bb, tt =  Reg (UInt(XLEN.W))
  val A3, B3, T3, R  =  WireInit(0.U(3.W))

  B := Cat(cap_cc_bounds.b(10,0),0.U(3.W)) 
  T := Cat(0.U(2.W),cap_cc_bounds.t(8,0),0.U(3.W)) 

  // val b_iE= Bool(cap_cc_bounds.iE)
   when (cap_cc_bounds.iE===0.U )
   {
      E := 0.U(6.W)
      T1 := Cat(T(13,3),cap_cc_bounds.tE(2,0) ) 
      B1 := Cat(B(13,3),cap_cc_bounds.bE(2,0) ) 
      msb := 0.U
      carry_out := Mux(T(11,0) < B(11,0), 1.U, 0.U)
   }    
  //  when (cap_cc_bounds.iE=/=0.U )
  .otherwise
   {
   E := Cat(cap_cc_bounds.tE(2,0), cap_cc_bounds.bE(2,0) ) 
   T1 := Cat(T(13,3),0.U(3.W)) 
   B1 := Cat(B(13,3),0.U(3.W)) 
   carry_out := Mux(T(11,3) < B(11,3), 1.U, 0.U)
   msb := 1.U
  } 

   //  T(13,12) := B(13,12) + Cat(0.U(1.W), carry_out) + Cat(0.U(1.W), msb)   
    val K = B1(13,12) + Cat(0.U(1.W), carry_out) + Cat(0.U(1.W), msb)
    T2 := Cat(K,T1(11,0))

    // bb := Cat((cap_cc_bounds.cursor >> (E + 14.U(6.W))), B1(13,0)) << E
    // tt := Cat((cap_cc_bounds.cursor >> (E + 14.U(6.W))), T2(13,0)) << E
    
    val bb1 = cap_cc_bounds.cursor >> (E + 14.U(6.W))//?
    val tt1 = cap_cc_bounds.cursor >> (E + 14.U(6.W))//?If it as more  than 64
  printf("cap_fat_bounds1.top uncompress :%b ,%b, %b ,%b \n",cap_cc_bounds.cursor,bb1, tt1,E)

    val bb2 = Cat(bb1(XLEN-15,0), B1(13,0)) << E
    val tt2 = Cat(tt1(XLEN-15,0), T2(13,0)) << E
  printf("cap_fat_bounds2.top uncompress :%b ,%b, %b  \n",cap_cc_bounds.cursor,bb2, tt2)


  // correction
   val k3 = (cap_cc_bounds.cursor) >> (E + 11.U(6.W))
   A3 := k3(2,0)
   T3 := T2(13,11)//Cat(T(13,11)) 
   B3 := B1(13,11)  
   R  := B3 - 1.U(3.W)       
  printf("cap_fat_bounds3.top uncompress :%b ,%b, %b ,%b,%b \n",k3,A3, T3, B3,R)

  val tt4 = Mux((A3 >= R) && (T3 < R), tt2 + (1.U(64.W) << (E + 14.U(6.W))), (Mux(((A3 < R) && (T3 >= R)), tt2 - (1.U(64.W) << (E + 14.U(6.W))),tt2 ))) //?        
  val bb4 = Mux((A3 >= R) && (B3 < R), bb2 + (1.U(64.W) << (E + 14.U(6.W))), (Mux(((A3 < R) && (B3 >= R)), bb2 - (1.U(64.W) << (E + 14.U(6.W))),bb2 ))) //?   


  cap_fat_bounds.cursor:= cap_cc_bounds.cursor
  cap_fat_bounds.base  := bb4
  cap_fat_bounds.top   := tt4 
  printf("cap_fat_bounds4.top uncompress :%b ,%b, %b  \n",cap_cc_bounds.cursor,bb4, tt4)

}

class shift_find_one extends Module 
{
  val dec           = IO(Input  (UInt(6.W)))
  val len           = IO(Input  (UInt(64.W)))
  val leading_zeros = IO(Input  (UInt(6.W))) 
  val test4         = IO(Output (UInt(1.W)))

  //to find first msb '1'
  val out   = leading_zeros - dec
  val test1 = (out > 12.U(6.W)) 
  val test2 = (len >> out)
  val test3 = Mux(test2(0),false.B,true.B)
  test4     := test1 && test3
  printf("shift_find_one :%b ,%b, %b , %b ,%b,%b, %b,%b \n",out,test2,len, dec,leading_zeros,test4,test1, test3)

}

// converts a fat bounds into a compressed bounds
class cap_bounds_compress extends Module 
{

  val cap_cc_bounds  =IO(Output(new cap_cc_bounds_t))
  val cap_fat_bounds =IO(Input(new cap_fat_bounds_t))

  val leading_zeros = Reg(UInt(6.W))//63.U(6.W)//??//UInt(6.W)// 0.U//"b000000".U(6.W)//VecInit.fill(6)(true.B)// //VecInit.fill(6)(true.B) //Vec(6, Bool())
  val E  = Reg(UInt(6.W))
  val iE = WireInit(0.U(1.W))
  val B1  = WireInit(0.U(14.W))
  val T1  = WireInit(0.U(12.W))
  val test2 = Reg(UInt(64.W))
 
  val test4,test1 =Reg(Bool())
  val test3 =Reg(Bool())  
  
  val len = Wire(UInt(64.W))
  len:= cap_fat_bounds.top - cap_fat_bounds.base 
  
  leading_zeros := 63.U

  E:= 0.U


  // //  val test1=(leading_zeros > 12.U(6.W)) && ((len >> leading_zeros) & 1.U(XLEN.W))
  // test1:=(leading_zeros > 12.U(6.W)) 
  // test2:=(len >> leading_zeros)
  // test3:=Mux(test2(0),false.B,true.B)
  // test4:= test1 && test3
   
  // // while(((leading_zeros > 12.U(6.W)) & ((len >> leading_zeros) & 1.U(XLEN.W))) === 0.U(XLEN.W)) 
  // when(test4)
  // {//to find first msb '1'
  //   leading_zeros := leading_zeros - 1.U
  //   test1:=(leading_zeros > 12.U(6.W)) 
  //   test2:=(len >> leading_zeros)
  //   test3:=Mux(test2(0),false.B,true.B)
  //   test4:= test1 && test3
  // }
  // printf("after for1: %b, %b , %b \n", leading_zeros, test4 , len )
 //  val flag      = RegInit(0.U)
 //  val flag      = RegInit(0.U(64.W))
 //  val flag      = Vec(64, UInt(1.W))
 val flag      = RegInit(VecInit(Seq.fill(64)(0.U(1.W))))//Vec(64,UInt(1.W))
 val index     = RegInit(0.U)
 val leading_zero_f = RegInit(63.U)
 val leading_zero_m = RegInit(VecInit(Seq.fill(64)(0.U(63.W))))//RegInit(63.U)
 val exe_units = for (i <- 0 until 52) yield
  {
   Module(new shift_find_one)
  }
    // flag:=0.U
    leading_zero_f:=0.U
    // when((len<4096.U)===1.U)//2 power 12
    when(len<4096.U)//2 power 12
    {
     flag(0):=1.U
     leading_zero_f:= 12.U
     printf("after when1: %b,%b ,%b , %b ,%b,  \n",cap_fat_bounds.top,len ,flag(0),  E ,len(12))
    }
   printf("origin bound: %b, %b , %b , %b\n", cap_fat_bounds.top, cap_fat_bounds.base,flag(0),flag(1) )

   for (i <- 0 until 52) //63-11
   {
    // flag(i+1):=0.U

    exe_units(i).dec           := i.U
    exe_units(i).len           := len
    exe_units(i).leading_zeros :=leading_zeros-i.U  
    leading_zero_m(i):= leading_zeros-i.U 
    // printf("after for1:%b, %b ,%b, %b , %b ,%b,%b,  \n",i.U, leading_zeros,leading_zero_m, flag, len, E ,len(12),leading_zero_f)
    printf("after for1:%b, %b ,%b, %b , %b ,%b,%b, %b ,%b,%b\n",i.U, leading_zeros,leading_zeros - i.U, flag(i), len, E ,len(12),leading_zero_f,leading_zero_m(i),exe_units(i).test4)
    // leading_zero_f:=Mux(len<4096.U, 12.U,(((exe_units(i).test4===1.U)||((leading_zeros - i.U)===12.U))&&(flag===0.U),leading_zero_m,leading_zero_f))
    // flag:=Mux(len<4096.U, 1.U,(((exe_units(i).test4===1.U)||((leading_zeros - i.U)===12.U)),1.U,0.U))
    // when(((exe_units(i).test4===1.U)||(leading_zero_m===12.U))&&(flag(i)===0.U))
    // when(((exe_units(i).test4===1.U)||((leading_zeros-i.U)===12.U))&&(flag(i)===0.U))
    when((exe_units(i).test4===0.U)&&(flag(i)===0.U))
    {
     flag(i+1):=1.U
     leading_zero_f:=leading_zeros-i.U//leading_zero_m
     printf("after when2: %b ,%b, %b , %b ,%b,%b,  \n", leading_zeros,leading_zero_f, flag(i+1), len, E ,len(12))
    
    }
    when(flag(i)===1.U)
    {
      printf("flag1 is: %b  \n",flag(i+1))

     flag(i+1):=1.U
    }
    // .otherwise{
    //  flag(i+1):=0.U
    //  printf("flag2 is: %b  \n",flag(i+1))

    // }
    // when(((len<4096.U)===1.U)&&(flag===0.U))//2 power 12
    // {//it is better to move outside
    //  flag:=1.U
    //  leading_zero_f:= 12.U
    //  printf("after when2: %b,%b ,%b , %b ,%b,%b,  \n", i.U,leading_zeros, flag, len, E ,len(12))
    // }
   }
  
 
 
  // var flag = 0
  // var leading_zeross = 63
  // var lenn =196//cap_fat_bounds.top - cap_fat_bounds.base//196//5439//196//len.litValue
  // var k = lenn
  // for (i <- 0 until 52)//63-11
  // {
  //   if (lenn<4096)
  //   {
  //     leading_zeross=12
  //     flag=1
  //   }
  //   else if (flag==0)
  //   {
  //    if ((leading_zeross > 12) && ((lenn>> leading_zeross) == 0))
  //     {
  //      leading_zeross = leading_zeross - 1
  //      k = lenn>> leading_zeross
  //      printf("after for3: %b ,%b , %b , %b \n", leading_zeross.U, flag.U,i.U, k.U)

  //     }
  //     else 
  //     {
  //      flag=1
  //     }
  //   }
  // }

  // printf("after for2: %b ,%b , %b ,%b,%b,  \n", leading_zeross.U, flag.U, len, E,len(12))
  printf("after for2: %b ,%b , %b ,%b,  \n", leading_zero_f, len, E ,len(12))


  E := leading_zero_f - 12.U(6.W)
  // E := leading_zeross.U - 12.U(6.W)

  when((E ===0.U(6.W))&&(len(12) === 0.U(1.W))) 
   {
    iE := 0.U
    cap_cc_bounds.iE:=0.U(1.W)
    B1 := cap_fat_bounds.base(13,0)
    T1 := cap_fat_bounds.top(11,0)
    
    printf("cap_bounds_compress len 12 zero1" )
   }     
  .otherwise
  // else
  //when((E ===1.U(6.W))||(len(12) === 1.U(1.W)))    
  {
    iE := 1.U(1.W)
    cap_cc_bounds.iE:=1.U(1.W)
    val k1 = (cap_fat_bounds.base >> E)
    val k2 = (cap_fat_bounds.top >> E)
    B1:=Cat(k1(13,3),E(2,0))
    
    printf("cap_fat_bounds.top %b , %b,%b,%b", cap_fat_bounds.top, k1, k2, E)

    if(((cap_fat_bounds.top >> (E + 3.U)) << (E + 3.U)) != cap_fat_bounds.top)
    {
      T1:=Cat(k2(11,3)+1.U,E(5,3))     
      printf("cap_fat_bounds.top yes %b ", T1)

    }    
     else 
     {
      T1:=Cat(k2(11,3),E(5,3))
      printf("cap_fat_bounds.top no %b ", T1)

     }
    // TODO: more corrections
  }

  cap_cc_bounds.cursor:= cap_fat_bounds.cursor

  cap_cc_bounds.bE := B1(2,0)
  cap_cc_bounds.b  := B1(13,3)
  cap_cc_bounds.tE := T1(2,0)
  cap_cc_bounds.t  := T1(11,3)
   printf("cap_fat_bounds.top final %b,%b, %b ,%b ", cap_cc_bounds.bE, cap_cc_bounds.b, cap_cc_bounds.tE,cap_cc_bounds.t)


}

// converts a compressed cap into a fat cap
class cap_uncompress extends Module {

 val cap_fat =IO(Output(new cap_fat_t))
 val cap_cc  =IO(Input(new cap_cc_t))
 
 val  cap_uncompress_cap = Module( new cap_bounds_uncompres)
 when ((cap_cc.meta.ty === cap_type_t.CAP_TYPE_SEALED )||(cap_cc.meta.ty === cap_type_t.CAP_TYPE_SEALEDRET)) 
 { 
  cap_uncompress_cap.cap_cc_bounds.iE := 0.U  
  cap_uncompress_cap.cap_cc_bounds.t  := 0.U      
  cap_uncompress_cap.cap_cc_bounds.tE := 0.U     
  cap_uncompress_cap.cap_cc_bounds.b  := 0.U      
  cap_uncompress_cap.cap_cc_bounds.bE := 0.U      
  cap_uncompress_cap.cap_cc_bounds.cursor := 0.U 

  cap_fat.bounds.cursor:= 0.U
  cap_fat.bounds.base:= cap_cc.bounds.cursor
  cap_fat.bounds.top:= 0.U
  cap_fat.reg_id:= cap_cc.bounds.b(5,1)
  cap_fat.async:= cap_cc.bounds.b(0)
  cap_fat.padding:= 0.U
  cap_fat.meta.ty:= cap_cc.meta.ty
  cap_fat.meta.perm:= cap_cc.meta.perm
  cap_fat.renode_id:= cap_cc.renode_id
 }
 .otherwise
 {

  cap_uncompress_cap.cap_cc_bounds.iE := cap_cc.bounds.iE   
  cap_uncompress_cap.cap_cc_bounds.t  := cap_cc.bounds.t     
  cap_uncompress_cap.cap_cc_bounds.tE := cap_cc.bounds.tE     
  cap_uncompress_cap.cap_cc_bounds.b  := cap_cc.bounds.b     
  cap_uncompress_cap.cap_cc_bounds.bE := cap_cc.bounds.bE     
  cap_uncompress_cap.cap_cc_bounds.cursor := cap_cc.bounds.cursor

  cap_fat.bounds.top    := cap_uncompress_cap.cap_fat_bounds.top
  cap_fat.bounds.base   := cap_uncompress_cap.cap_fat_bounds.base
  cap_fat.bounds.cursor := cap_uncompress_cap.cap_fat_bounds.cursor

  cap_fat.reg_id:= 0.U
  cap_fat.async:= 0.U
  cap_fat.padding:= 0.U
  cap_fat.renode_id:= cap_cc.renode_id
  cap_fat.meta.ty  := cap_cc.meta.ty
  cap_fat.meta.perm:= cap_cc.meta.perm
  }
}

// converts a fat cap into a compressed cap
class cap_compress extends Module {
  val cap_fat =IO(Input(new cap_fat_t()))
  val cap_cc  =IO(Output(new cap_cc_t()))
  val cap_compress_cap = Module( new cap_bounds_compress)


  // if ((cap_fat.meta.ty == cap_type_t.CAP_TYPE_SEALED) || (cap_fat.meta.ty == cap_type_t.CAP_TYPE_SEALEDRET)) 
  when ((cap_fat.meta.ty === cap_type_t.CAP_TYPE_SEALED) || (cap_fat.meta.ty === cap_type_t.CAP_TYPE_SEALEDRET)) 
  {
  // printf("cap_compress  if 1")

  // we don't need to store full bounds. Instead, we reuse the bits
  // for async and reg
  cap_compress_cap.cap_fat_bounds.top    :=0.U
  cap_compress_cap.cap_fat_bounds.base   :=0.U
  cap_compress_cap.cap_fat_bounds.cursor :=0.U
  
  //  cap_cc.bounds:= 0.U
   cap_cc.meta.ty:= cap_fat.meta.ty
   cap_cc.meta.perm:= cap_fat.meta.perm
   cap_cc.bounds.iE  := 0.U    //1
   cap_cc.bounds.t   := 0.U//9
   cap_cc.bounds.tE  := 0.U //3
   cap_cc.bounds.b   := Cat(0.U(5.W),cap_fat.reg_id,cap_fat.async)
   cap_cc.bounds.bE  := 0.U//3
   cap_cc.bounds.cursor  := cap_fat.bounds.base//Cat(cap_fat.bounds.base)
   cap_cc.renode_id:= cap_fat.renode_id


  }
  // else 
  .otherwise
   {
     cap_compress_cap.cap_fat_bounds.top    :=cap_fat.bounds.top
     cap_compress_cap.cap_fat_bounds.base   :=cap_fat.bounds.base
     cap_compress_cap.cap_fat_bounds.cursor :=cap_fat.bounds.cursor

     cap_cc.bounds.iE    := cap_compress_cap.cap_cc_bounds.iE 
     cap_cc.bounds.t     := cap_compress_cap.cap_cc_bounds.t 
     cap_cc.bounds.tE    := cap_compress_cap.cap_cc_bounds.tE 
     cap_cc.bounds.b     := cap_compress_cap.cap_cc_bounds.b 
     cap_cc.bounds.bE    := cap_compress_cap.cap_cc_bounds.bE 
     cap_cc.bounds.cursor:= cap_compress_cap.cap_cc_bounds.cursor

     cap_cc.renode_id:= cap_fat.renode_id
     cap_cc.meta.ty  := cap_fat.meta.ty
     cap_cc.meta.perm:= cap_fat.meta.perm
   }
  }

class cap_cc_set_async_reg_id extends Module {

  val async   =  IO(Input(0.U(1.W)))
  val reg_id  =  IO(Input(0.U(5.W)))
  val cap_cc_n= IO(Output(new cap_cc_t()))
  val cap_cc  =  IO(Input (new cap_cc_t()))

    cap_cc_n := cap_cc;
    cap_cc_n.bounds.b(5,1) := reg_id
    cap_cc_n.bounds.b(0) := async
    
  }

class cap_cc_get_async  extends Module {
 
  val cap_cc =IO(Input(new cap_cc_t()))
  val Outputt=IO(Output(0.U(1.W)))
   Outputt := cap_cc.bounds.b(0)
}

class cap_cc_is_cap extends Module { 
  val cap_cc =IO(Input(new cap_cc_t()))
  // val cap_cc =Wire(new cap_cc_t())
  val out    =IO(Output(0.U(1.W)))
  
    out:= cap_cc; // FIXME: temporary hack//??
  }

/////////////////////////end compression/decompression//////////////

//mlabaf//capstone
class crevokeModule(implicit p: Parameters) extends XSModule {
  val src       = IO(Input(new fu_data_t))
  val crevoke   = IO(Output(new cap_result_t ))
  val crevoke2  = IO(Output(new cap_result_t ))
  val  ex_o_v   = IO(Output (UInt(1.W) ))

  //node port
   val send_node_query_ins  =IO(new send_node_query_bundle())
   val send_node_mut_ins    =IO(new send_node_mut_bundle())

   val send_node_query_c    = Module(new send_node_query)
   val send_node_mut_c      = Module(new send_node_mut)

   send_node_query_ins<>send_node_query_c.ins
   send_node_mut_ins<>send_node_mut_c.ins

   ex_o_v:=0.U

   val rs1_cc = Reg(new cap_cc_t)
   val rs1_c  = Reg(new cap_fat_t) 
   val rs2_cc = Reg(new cap_cc_t) 
   val cap_uncompress_cap = Module(new cap_uncompress)
   val cap_compress_cap   = Module(new cap_compress)

   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6  
   rs1_cc.bounds.iE  := src.cap_a(37)    //1
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64)//64

   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 

  rs1_c.bounds.cursor := src.operand_a
  rs1_c.bounds.base   := src.operand_a
  rs1_c.bounds.top    := src.operand_b



  printf("revoke origin type is=%b\n",rs1_cc.meta.ty)

  when ((src.tag_a===0.U) && (rs1_cc.meta.ty === cap_type_t.CAP_TYPE_REVOKE))
  {

   cap_compress_cap.cap_fat.meta.ty:=cap_type_t.CAP_TYPE_LINEAR

   cap_compress_cap.cap_fat.bounds.base  :=rs1_c.bounds.base
   cap_compress_cap.cap_fat.bounds.top   :=rs1_c.bounds.top  
   cap_compress_cap.cap_fat.bounds.cursor:=rs1_c.bounds.cursor
   cap_compress_cap.cap_fat.meta.perm   :=rs1_c.meta.perm
   cap_compress_cap.cap_fat.renode_id   :=rs1_c.renode_id
   cap_compress_cap.cap_fat.reg_id      :=rs1_c.reg_id
   cap_compress_cap.cap_fat.async       :=rs1_c.async
   cap_compress_cap.cap_fat.padding     :=rs1_c.padding

   rs2_cc.bounds.iE   := cap_compress_cap.cap_cc.bounds.iE
   rs2_cc.bounds.t    := cap_compress_cap.cap_cc.bounds.t
   rs2_cc.bounds.tE   := cap_compress_cap.cap_cc.bounds.tE  
   rs2_cc.bounds.b    := cap_compress_cap.cap_cc.bounds.b  
   rs2_cc.bounds.bE   := cap_compress_cap.cap_cc.bounds.bE  
   rs2_cc.renode_id     := cap_compress_cap.cap_cc.renode_id
   rs2_cc.meta.perm     := cap_compress_cap.cap_cc.meta.perm
   rs2_cc.meta.ty       := cap_compress_cap.cap_cc.meta.ty



  // rd_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1}; 
    crevoke.cap  := Cat(rs2_cc.bounds.cursor , rs2_cc.bounds.bE , rs2_cc.bounds.b , rs2_cc.bounds.tE , rs2_cc.bounds.t , rs2_cc.bounds.iE , rs2_cc.meta.perm, rs2_cc.meta.ty , rs2_cc.renode_id )
    crevoke.tag  := 1.U(1.W)
    crevoke.valid:= 1.U(1.W) 

    // send_node_query(rs1_cc.renode_id, 1'b0);
    // send_node_mut(rs1_cc.renode_id, NODE_REVOKE)
    send_node_query_c.ins.revnode_id:=rs2_cc.renode_id
    send_node_query_c.ins.synchronous:=0.U
    //initial
    // send_node_query_c.ins.node_query_sent_q          := 0.U
    // send_node_query_c.ins.node_query_ready_i         := 0.U
    // send_node_query_c.ins.node_query_resp_valid_i    := 0.U
    // send_node_query_c.ins.node_query_resp_received_q := 0.U 
    // send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
    // send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
    // send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U  

    send_node_mut_c.ins.revnode_id:=rs2_cc.renode_id
    send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_REVOKE
    //initial
    send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
    send_node_mut_c.ins.have_alloc             := 0.U 

     printf("revokation is done correctly\n")
     printf( "type convert to %b\n",rs2_cc.meta.ty)
     printf( "crevokeModule.revnode_id %b\n",send_node_query_c.ins.revnode_id)
     }
  //  else
   .otherwise
     {

     cap_compress_cap.cap_fat.bounds.base  :=0.U
     cap_compress_cap.cap_fat.bounds.top   :=0.U 
     cap_compress_cap.cap_fat.bounds.cursor:=0.U
     cap_compress_cap.cap_fat.meta.ty      :=0.U
     cap_compress_cap.cap_fat.meta.perm   :=0.U
     cap_compress_cap.cap_fat.renode_id   :=0.U
     cap_compress_cap.cap_fat.reg_id      :=0.U
     cap_compress_cap.cap_fat.async       :=0.U
     cap_compress_cap.cap_fat.padding     :=0.U

      printf("revokation exception\n")

      crevoke.cap  := 0.U 
      crevoke.tag  := 0.U(1.W)
      crevoke.valid:= 0.U(1.W)  
      ex_o_v := 1.U
     }
       

   crevoke2.cap  := 0.U 
   crevoke2.tag  := 0.U(1.W)
   crevoke2.valid:= 0.U(1.W)  

}

class cshrinkModule(implicit p: Parameters) extends XSModule {
    val src      = IO(Input(new fu_data_t))
    val cshrink  = IO(Output(new cap_result_t )) 
    val cshrink2 = IO(Output(new cap_result_t )) 
    val  ex_o_v= IO(Output (UInt(1.W) ))
    ex_o_v := 0.U


   //unpacked capability
   val rs1_cc =Reg(new cap_cc_t)  
   val rs2_cc =Reg(new cap_cc_t)  
   val rs1_c  =Reg(new cap_fat_t)
   val cursor =Wire(UInt(XLEN.W))

   rs1_cc.renode_id  := src.cap_c(30,0)//31
   rs1_cc.meta.ty    := src.cap_c(33,31) //6
   rs1_cc.meta.perm  := src.cap_c(36,34) //6  
   rs1_cc.bounds.iE  := src.cap_c(37)    //1
   rs1_cc.bounds.t   := src.cap_c(46,38) //9
   rs1_cc.bounds.tE  := src.cap_c(49,47) //3
   rs1_cc.bounds.b   := src.cap_c(60,50) //11
   rs1_cc.bounds.bE  := src.cap_c(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_c(127,64)//64
   
  val cap_uncompress_cap=Module(new cap_uncompress)
  val cap_compress_cap  =Module(new cap_compress)

  // cap_uncompress_cap.cap_cc:=rs1_cc
  // rs1_c := cap_uncompress_cap.cap_fat
   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id

   //these three ones(base, top, cursor) will be changed after when statement, so below assighnment do not affect
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor     
  //  rs1_c.bounds.base   := 100230132.U
  //  rs1_c.bounds.top    := 100230328.U
  //  rs1_c.bounds.cursor := 100230256.U   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty


  cursor := cap_uncompress_cap.cap_fat.bounds.cursor
  printf("origin base top cursor: %b , %b , %b \n ",cap_uncompress_cap.cap_fat.bounds.base, cap_uncompress_cap.cap_fat.bounds.top ,cap_uncompress_cap.cap_fat.bounds.cursor )
  printf("shrinked PARAMETERS: %b , %b , %b , %b , %b\n ",src.cap_c, src.tag_a,src.tag_b ,src.tag_c,rs1_c.meta.ty )

  when ((src.tag_a=/=0.U) || (src.tag_b=/=0.U) || (src.tag_c===0.U) ||
     ((rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_LINEAR) && (rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_NONLIN) && (rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_UNINIT)) ||
     (src.operand_a >= src.operand_b)===1.U || (src.operand_a < cap_uncompress_cap.cap_fat.bounds.base)===1.U ||
     (src.operand_b > cap_uncompress_cap.cap_fat.bounds.top)===1.U) 
  {            
    ex_o_v := 1.U
    printf("shrink exception\n")

    //TODO: set tval and cause
  }

   //so, we execute this portion if exception is valid or skip and handle exception in ex_stage?
   rs1_c.bounds.base   := src.operand_a
   rs1_c.bounds.top    := src.operand_b
   rs1_c.bounds.cursor := Mux(cursor<src.operand_a, src.operand_a,Mux(cursor>src.operand_b, src.operand_b, cursor))
  
   printf("shrinked new base top cursor: %b , %b , %b \n ",rs1_c.bounds.base, rs1_c.bounds.top , rs1_c.bounds.cursor )

  //  rs1_cc = capstone::cap_compress(rs1_c);
  //  cap_compress_cap.cap_fat:=rs1_c
  //  rs1_cc:= cap_compress_cap.cap_cc
   cap_compress_cap.cap_fat.bounds.base  :=rs1_c.bounds.base
   cap_compress_cap.cap_fat.bounds.top   :=rs1_c.bounds.top  
   cap_compress_cap.cap_fat.bounds.cursor:=rs1_c.bounds.cursor
   cap_compress_cap.cap_fat.meta.ty     :=rs1_c.meta.ty
   cap_compress_cap.cap_fat.meta.perm   :=rs1_c.meta.perm
   cap_compress_cap.cap_fat.renode_id   :=rs1_c.renode_id
   cap_compress_cap.cap_fat.reg_id      :=rs1_c.reg_id
   cap_compress_cap.cap_fat.async       :=rs1_c.async
   cap_compress_cap.cap_fat.padding     :=rs1_c.padding

   
   rs2_cc.bounds.iE   := cap_compress_cap.cap_cc.bounds.iE
   rs2_cc.bounds.t    := cap_compress_cap.cap_cc.bounds.t
   rs2_cc.bounds.tE   := cap_compress_cap.cap_cc.bounds.tE  
   rs2_cc.bounds.b    := cap_compress_cap.cap_cc.bounds.b  
   rs2_cc.bounds.bE   := cap_compress_cap.cap_cc.bounds.bE  
   rs2_cc.renode_id   := cap_compress_cap.cap_cc.renode_id
   rs2_cc.meta.perm   := cap_compress_cap.cap_cc.meta.perm
   rs2_cc.meta.ty     := cap_compress_cap.cap_cc.meta.ty


  // rd_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};

    cshrink2.cap  := Cat(rs2_cc.bounds.cursor , rs2_cc.bounds.bE , rs2_cc.bounds.b , rs2_cc.bounds.tE , rs2_cc.bounds.t , rs2_cc.bounds.iE , rs2_cc.meta.perm, rs2_cc.meta.ty , rs2_cc.renode_id)
    cshrink2.tag  := 0.U 
    cshrink2.valid:= 1.U

    cshrink.cap        := 0.U
    cshrink.tag        := 0.U 
    cshrink.valid      := 0.U
}


class cshrinktoModule(implicit p: Parameters) extends XSModule {
 val src        = IO(Input(new fu_data_t))
 val cshrinkto  = IO(Output(new cap_result_t ))
 val cshrinkto2 = IO(Output(new cap_result_t )) 
 val  ex_o_v    = IO(Output (UInt(1.W) ))
 ex_o_v := 0.U

 //unpacked capability
 val rs1_cc =Reg(new cap_cc_t)  
 val rs2_cc =Reg(new cap_cc_t)  
 val rs1_c  =Reg(new cap_fat_t)

 rs1_cc.renode_id  := src.cap_c(30,0)//31
 rs1_cc.meta.ty    := src.cap_c(33,31) //6
 rs1_cc.meta.perm  := src.cap_c(36,34) //6  
 rs1_cc.bounds.iE  := src.cap_c(37)    //1
 rs1_cc.bounds.t   := src.cap_c(46,38) //9
 rs1_cc.bounds.tE  := src.cap_c(49,47) //3
 rs1_cc.bounds.b   := src.cap_c(60,50) //11
 rs1_cc.bounds.bE  := src.cap_c(63,61)//3
 rs1_cc.bounds.cursor  := src.cap_c(127,64)//64
   
 val cap_uncompress_cap=Module(new cap_uncompress)
 val cap_compress_cap  =Module(new cap_compress) 
 // cap_uncompress_cap.cap_cc:=rs1_cc
 // rs1_c := cap_uncompress_cap.cap_fat
  cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
  cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
  cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
  cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
  cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
  cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
  cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
  cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
  cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id 
  //these three ones(base, top, cursor) will be changed after when statement, so below assighnment do not affect
  rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
  rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
  rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor//200.U    
  rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
  rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
  rs1_c.async         := cap_uncompress_cap.cap_fat.async
  rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
  rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
  rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

  val k1= cap_uncompress_cap.cap_fat.bounds.base
  val k2= cap_uncompress_cap.cap_fat.bounds.top

  when ((src.tag_a=/=0.U) || (src.tag_b=/=0.U) || (src.tag_c===0.U) ||
     ((rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_LINEAR) && (rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_NONLIN) && (rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_UNINIT)) ||
     (rs1_c.bounds.cursor < k1)===1.U ||
     ((rs1_c.bounds.cursor +  src.imm(11,0) )> k2)===1.U) 
  {            
    ex_o_v := 1.U
    printf("shrinkto exception\n")
  }

   //so, we execute this portion if exception is valid or skip and handle exception in ex_stage?
   rs1_c.bounds.base   := rs1_c.bounds.cursor
   rs1_c.bounds.top    := rs1_c.bounds.cursor + src.imm(11,0)
  
   printf("shrinkto new base top cursor: %b , %b , %b , %b \n ",rs1_c.bounds.base, rs1_c.bounds.top , rs1_c.bounds.cursor, src.imm(11,0) )

  //  rs1_cc = capstone::cap_compress(rs1_c);
  //  cap_compress_cap.cap_fat:=rs1_c
  //  rs1_cc:= cap_compress_cap.cap_cc
   cap_compress_cap.cap_fat.bounds.base  :=rs1_c.bounds.base
   cap_compress_cap.cap_fat.bounds.top   :=rs1_c.bounds.top  
   cap_compress_cap.cap_fat.bounds.cursor:=rs1_c.bounds.cursor
   cap_compress_cap.cap_fat.meta.ty     :=rs1_c.meta.ty
   cap_compress_cap.cap_fat.meta.perm   :=rs1_c.meta.perm
   cap_compress_cap.cap_fat.renode_id   :=rs1_c.renode_id
   cap_compress_cap.cap_fat.reg_id      :=rs1_c.reg_id
   cap_compress_cap.cap_fat.async       :=rs1_c.async
   cap_compress_cap.cap_fat.padding     :=rs1_c.padding

   
   rs2_cc.bounds.iE   := cap_compress_cap.cap_cc.bounds.iE
   rs2_cc.bounds.t    := cap_compress_cap.cap_cc.bounds.t
   rs2_cc.bounds.tE   := cap_compress_cap.cap_cc.bounds.tE  
   rs2_cc.bounds.b    := cap_compress_cap.cap_cc.bounds.b  
   rs2_cc.bounds.bE   := cap_compress_cap.cap_cc.bounds.bE  
   rs2_cc.renode_id   := cap_compress_cap.cap_cc.renode_id
   rs2_cc.meta.perm   := cap_compress_cap.cap_cc.meta.perm
   rs2_cc.meta.ty     := cap_compress_cap.cap_cc.meta.ty


  // rd_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};

    cshrinkto2.cap  := Cat(rs2_cc.bounds.cursor , rs2_cc.bounds.bE , rs2_cc.bounds.b , rs2_cc.bounds.tE , rs2_cc.bounds.t , rs2_cc.bounds.iE , rs2_cc.meta.perm, rs2_cc.meta.ty , rs2_cc.renode_id)
    cshrinkto2.tag  := 0.U 
    cshrinkto2.valid:= 1.U

    cshrinkto.cap  := 0.U
    cshrinkto.tag  := 0.U(1.W)
    cshrinkto.valid:= 0.U(1.W)  

}


class ctightenModule(implicit p: Parameters) extends XSModule {
   val src          = IO(Input(new fu_data_t))
   val ctighten     = IO(Output(new cap_result_t ))   
   val ctighten2    = IO(Output(new cap_result_t ))
   val  ex_o_v      = IO(Output (UInt(1.W) ))

   ex_o_v:=0.U

   val rs1_cc = Wire(new cap_cc_t)  
   val res    = Wire(new cap_cc_t)  
   val imm    = src.operand_b(4,0)//5bit

   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6  
   rs1_cc.bounds.iE  := src.cap_a(37)    //1
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64
        
   printf("tighten parameter imm, perm =%b ,%b  \n",imm, rs1_cc.meta.perm)

   when (src.tag_a===0.U || (rs1_cc.meta.ty=/=cap_type_t.CAP_TYPE_LINEAR && rs1_cc.meta.ty=/=cap_type_t.CAP_TYPE_NONLIN && rs1_cc.meta.ty=/=cap_type_t.CAP_TYPE_UNINIT)
   ||(((rs1_cc.meta.perm >imm)===1.U)&&((8.U >imm)===1.U)))
    {
        printf("tighten exception\n")
        ex_o_v := 1.U
    }

    res:= rs1_cc
    // res<> rs1_cc
    res.meta.perm := Mux (imm > 7.U(3.W) , 0.U(3.W) , imm )
    printf("tighten new perm %b:\n",res.meta.perm)
        
    when (src.rs1 === src.rd) 
    {
      ctighten.cap  := 0.U
      ctighten.tag  := 0.U(1.W)
      ctighten.valid:= 0.U(1.W)

    // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1};
      ctighten2.cap  := Cat(res.bounds.cursor , res.bounds.bE , res.bounds.b , res.bounds.tE , res.bounds.t , res.bounds.iE , res.meta.perm, res.meta.ty , res.renode_id)
      ctighten2.tag  := 1.U(1.W)
      ctighten2.valid:= 1.U(1.W) 
      printf("tighten rs1==rd\n")
 
    }
    .elsewhen (rs1_cc.meta.ty === cap_type_t.CAP_TYPE_NONLIN)
    {
    // rs1_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};
    // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1}; 

      ctighten.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.bE , rs1_cc.bounds.b , rs1_cc.bounds.tE , rs1_cc.bounds.t , rs1_cc.bounds.iE , rs1_cc.meta.perm, rs1_cc.meta.ty , rs1_cc.renode_id )
      ctighten.tag  := 1.U(1.W)
      ctighten.valid:= 1.U(1.W)

      ctighten2.cap  := Cat(res.bounds.cursor , res.bounds.bE , res.bounds.b , res.bounds.tE , res.bounds.t , res.bounds.iE , res.meta.perm, res.meta.ty , res.renode_id)
      ctighten2.tag  := 1.U(1.W)
      ctighten2.valid:= 1.U(1.W) 
      printf("tighten CAP_TYPE_NONLIN\n")

    }
    .otherwise 
    { // rs1 is a linear type
      // rs1_result_o = '{cap: '0, tag: '0, valid: '1};
      // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1};
      ctighten.cap  := 0.U(128.W)
      ctighten.tag  := 0.U(1.W)
      ctighten.valid:= 1.U(1.W) 

      ctighten2.cap   := Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
      ctighten2.tag         := 1.U(1.W)
      ctighten2.valid       := 1.U(1.W) 

     }

}
class cdelinModule extends Module {
  val src     = IO(Input(new fu_data_t))
  val cdelin  = IO(Output(new cap_result_t ))   
  val cdelin2 = IO(Output(new cap_result_t ))   
  val  ex_o_v = IO(Output (UInt(1.W) ))

   ex_o_v:=0.U


   //unpacked capability
   val rs1_cc=Wire(new cap_cc_t)  
   val send_node_mut_ins    =IO(new send_node_mut_bundle())
   val send_node_mut_c      =Module(new send_node_mut)
   send_node_mut_ins<>send_node_mut_c.ins
   
   val k = src.cap_c(36,31)

   rs1_cc.renode_id  := (src.cap_c(30,0))//31
   rs1_cc.meta.ty    := (src.cap_c(36,31)) //6
   rs1_cc.meta.perm  := (src.cap_c(36,34)) //6  
   rs1_cc.bounds.iE  := (src.cap_c(37))    //1
   rs1_cc.bounds.t   := (src.cap_c(46,38)) //9
   rs1_cc.bounds.tE  := (src.cap_c(49,47)) //3
   rs1_cc.bounds.b   := (src.cap_c(60,50)) //11
   rs1_cc.bounds.bE  := (src.cap_c(63,61))//3
   rs1_cc.bounds.cursor  := (src.cap_c(127,64)) //64

   printf( "befor delin type is: %b\n",k )

  //  when (src.tag_c===0.U || (rs1_cc.meta.ty =/= cap_type_t.CAP_TYPE_LINEAR)) 
   when (src.tag_c===0.U || (k =/= cap_type_t.CAP_TYPE_LINEAR)) 
   {
    ex_o_v := 1.U
    printf("delin exception\n")

   }

   rs1_cc.meta.ty := cap_type_t.CAP_TYPE_NONLIN
   printf( "delin is done right. type is: %b\n",rs1_cc.meta.ty)

   cdelin2.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
   cdelin2.tag  := 1.U(1.W)
   cdelin2.valid:= 1.U(1.W)   

   cdelin.cap  := 0.U 
   cdelin.tag  := 0.U(1.W)
   cdelin.valid:= 0.U(1.W)  
   printf( "befor delin cdelin2.cap: %b\n",cdelin2.cap )

    ////send_node_mut(rs1_cc.renode_id, NODE_DELIN);
    //node port
    send_node_mut_c.ins.revnode_id:=rs1_cc.renode_id
    send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_DELIN
    //initial
    send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
    send_node_mut_c.ins.have_alloc             := 0.U 

    printf( "delin send_node_mut_c.ins.mut_ty %b\n",send_node_mut_c.ins.mut_ty)

}
class clccModule extends Module {

    val src    = IO(Input(new fu_data_t))
    val clcc   = IO(Output(new cap_result_t ))   
    val clcc2  = IO(Output(new cap_result_t )) 
    val  ex_o_v= IO(Output (UInt(1.W) ))
    
    ex_o_v:=0.U


   //unpacked capability
   val rs1_cc=Wire(new cap_cc_t)  
   val rs1_c =Wire(new cap_fat_t)
   val imm   = Wire(UInt(3.W))//Wire(UInt(XLEN.W))
   val ty    = Wire(UInt(3.W))//Wire(UInt(XLEN.W))
   val res   = Wire(UInt(XLEN.W))

   val cap_uncompress_cap=Module(new cap_uncompress)

   val send_node_query_ins  =IO(new send_node_query_bundle())

   val send_node_query_c    = Module(new send_node_query)


   send_node_query_ins<>send_node_query_c.ins



   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6
   rs1_cc.bounds.iE  := src.cap_a(37)   //1
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  //  cap_uncompress_cap.cap_cc:=rs1_cc
  //  rs1_c := cap_uncompress_cap.cap_fat

   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 

   imm   := src.operand_b(2,0)
   ty    := (rs1_c.meta.ty)
         
   when (src.tag_a===0.U || (imm ===2.U(3.W) && ty === 4.U(3.W)) || ((imm === 4.U(3.W) || imm === 5.U(3.W)) && (ty===4.U(3.W) || ty===5.U(3.W) || ty===6.U(3.W))) ||
      (imm === 6.U(3.W) && (ty=/=4.U(3.W) && ty=/=5.U(3.W))) || (imm === 7.U(3.W) && (ty =/= 5.U(3.W)))) 
      {
       ex_o_v := 1.U
       printf("clcc exception\n")

      }
    printf("imm ,in clcc is : %b\n", imm )

    res :=0.U(XLEN.W)
    switch (imm)
    {
      is (0.U) 
      {
        // special treatment later
      }
      is (1.U)
      { 
        res:= rs1_c.meta.ty
        printf("clcc convert type\n")
      }
      is(2.U)
      {
        res := rs1_c.bounds.cursor
        printf("clcc convert cursor\n")

      }
      is (3.U)
      {
        res := rs1_c.bounds.base
        printf("clcc convert base\n")

      }
      is (4.U)
      {  
        res:= rs1_c.bounds.top
        printf("clcc convert top\n")

      }
      is (5.U)
      {
        res:= rs1_c.meta.perm
        printf("clcc convert perm\n")

      }
      is (6.U)
      {
       res := rs1_c.async
       printf("clcc convert async\n")

      }
      is (7.U) 
      {
        res := rs1_c.reg_id
        printf("clcc convert reg_id\n")

      }
    }

    clcc2.cap := res
    clcc2.tag:= 0.U(1.W)
    clcc2.valid:= 1.U(1.W)   

    clcc.cap := 0.U
    clcc.tag:= 0.U(1.W)
    clcc.valid:= 1.U(1.W) 

    when (imm === 0.U) 
    {
    // send_node_query(rs1_c.renode_id, 1'b1);
      send_node_query_c.ins.revnode_id:=rs1_c.renode_id
      send_node_query_c.ins.synchronous:=1.U
      //initial
      // send_node_query_c.ins.node_query_sent_q          := 0.U
      // send_node_query_c.ins.node_query_ready_i         := 0.U
      // send_node_query_c.ins.node_query_resp_valid_i    := 0.U
      // send_node_query_c.ins.node_query_resp_received_q := 0.U 
      // send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
      // send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
      // send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U 
     }
}

class csccModule extends Module {
   val src   = IO(Input(new fu_data_t))
   val cscc  = IO(Output(new cap_result_t ))   
   val cscc2 = IO(Output(new cap_result_t ))
   val  ex_o_v= IO(Output (UInt(1.W) ))

   ex_o_v:=0.U
  
   //unpacked capability
   val rs1_cc=Wire(new cap_cc_t)  
   val res   =Wire(new cap_cc_t)
   val rs1_c =Wire(new cap_fat_t)

   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6
   rs1_cc.bounds.iE  := src.cap_a(37)    //1
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64
   
  val cap_uncompress_cap = Module(new cap_uncompress)
  val cap_compress_cap   = Module(new cap_compress)

  // cap_uncompress_cap.cap_cc:=rs1_cc
  // rs1_c := cap_uncompress_cap.cap_fat

   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 
 
  when(src.tag_a===0.U || src.tag_b=/=0.U || (rs1_c.meta.ty===cap_type_t.CAP_TYPE_UNINIT) || (rs1_c.meta.ty===cap_type_t.CAP_TYPE_SEALED)) 
   {    
    ex_o_v := 1.U
    printf("SCC exception\n")
   }

   rs1_c.bounds.cursor := src.operand_b
   printf("SCC convert the cursor to %b:\n",src.operand_b )

  //  cap_compress_cap.cap_fat:=rs1_c
  //  res:= cap_compress_cap.cap_cc

  cap_compress_cap.cap_fat.bounds.base:= rs1_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rs1_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
  cap_compress_cap.cap_fat.async      := rs1_c.async         
  cap_compress_cap.cap_fat.padding := rs1_c.padding
  cap_compress_cap.cap_fat.meta.perm := rs1_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty := rs1_c.meta.ty 

  res.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  res.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  res.bounds.tE := cap_compress_cap.cap_cc.bounds.tE
  res.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  res.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  res.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  res.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  res.meta.perm := cap_compress_cap.cap_cc.meta.perm
  res.renode_id :=  cap_compress_cap.cap_cc.renode_id

   when (src.rs1 === src.rd) 
   {

    cscc.cap  := 0.U
    cscc.tag  := 0.U(1.W)
    cscc.valid:= 0.U(1.W)

    cscc2.cap  :=Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
    cscc2.tag  := 1.U(1.W)
    cscc2.valid:= 1.U(1.W)
   } 
    .elsewhen (rs1_cc.meta.ty === cap_type_t.CAP_TYPE_NONLIN)
   {

    cscc.cap  :=Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
    cscc.tag  := 1.U(1.W)
    cscc.valid:= 1.U(1.W)

    cscc2.cap  := Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
    cscc2.tag  := 1.U(1.W)
    cscc2.valid:= 1.U(1.W)    
    
    
    } 
    .otherwise 
    { // rs1 is a linear type
    cscc.cap       := 0.U(128.W)
    cscc.tag       := 0.U(1.W)
    cscc.valid     := 1.U(1.W)

    cscc2.cap:= Cat(res.renode_id,res.meta.ty,res.meta.perm, res.renode_id, res.meta.ty , res.meta.perm, res.bounds.iE , res.bounds.t , res.bounds.tE , res.bounds.b , res.bounds.bE ,res.bounds.cursor)
    cscc2.tag:= 1.U(1.W)
    cscc2.valid:= 1.U(1.W)      
    }
}

class csplitModule extends Module {
 
 val src      = IO(Input(new fu_data_t))
 val csplit   = IO(Output(new cap_result_t ))  
 val csplit2  = IO(Output(new cap_result_t ))
 val  ex_o_v  = IO(Output (UInt(1.W) )) 
 ex_o_v:=0.U 
 val send_node_query_ins  =IO(new send_node_query_bundle())
 val send_node_mut_ins    =IO(new send_node_mut_bundle())
 val send_node_alloc_ins  =IO(new send_node_alloc_bundle()) 
 val send_node_query_c    = Module(new send_node_query)
 val send_node_mut_c      = Module(new send_node_mut)
 val send_node_alloc_c    = Module(new send_node_alloc) 
 send_node_query_ins<>send_node_query_c.ins
 send_node_mut_ins<>send_node_mut_c.ins
 send_node_alloc_ins<>send_node_alloc_c.ins 
 //unpacked capability
 val rs1_cc=Reg(new cap_cc_t)
 val rs2_cc=Reg(new cap_cc_t)
 val rd_cc =Reg(new cap_cc_t)
 val rs1_c =Reg(new cap_fat_t)
 val rd_c  =Reg(new cap_fat_t)
 
 val rs2 = Reg(UInt (XLEN.W))
 rs2:=src.operand_b 
 rs1_cc.renode_id  := src.cap_a(30,0)//31
 rs1_cc.meta.ty    := src.cap_a(33,31) //6
 rs1_cc.meta.perm  := src.cap_a(36,34) //6
 rs1_cc.bounds.iE  := src.cap_a(37)    //1
 rs1_cc.bounds.t   := src.cap_a(46,38) //9
 rs1_cc.bounds.tE  := src.cap_a(49,47) //3
 rs1_cc.bounds.b   := src.cap_a(60,50) //11
 rs1_cc.bounds.bE  := src.cap_a(63,61)//3
 rs1_cc.bounds.cursor  := src.cap_a(127,64) //64
 
 val cap_uncompress_cap=Module(new cap_uncompress)
 val cap_compress_cap  =Module(new cap_compress) 
 // cap_uncompress_cap.cap_cc:=rs1_cc
 // rs1_c := cap_uncompress_cap.cap_fat
 cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
 cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
 cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
 cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
 cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
 cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
 cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
 cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
 cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
 
 rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
 rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
 rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
 rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
 rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
 rs1_c.async         := cap_uncompress_cap.cap_fat.async
 rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
 rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
 rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 

 val k = cap_uncompress_cap.cap_fat.bounds.top
 
  // rd_c  := rs1_c
  // rd_c  <> rs1_c
 rd_c.bounds.base   := rs1_c.bounds.base   
 rd_c.bounds.top    := k//rs1_c.bounds.top    
 rd_c.bounds.cursor := rs1_c.bounds.cursor 
 rd_c.renode_id     := rs1_c.renode_id     
 rd_c.reg_id        := rs1_c.reg_id        
 rd_c.async         := rs1_c.async         
 rd_c.padding       := rs1_c.padding       
 rd_c.meta.perm     := rs1_c.meta.perm     
 rd_c.meta.ty       := rs1_c.meta.ty       

  // printf("split rd_c %b\n", rd_c.bounds.base)

  // printf("split rs1_c %b\n",rs1_c.bounds.base)


  //validity check
  when ((src.tag_a===0.U) || (src.tag_b===1.U) || ((rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_LINEAR) && (rs1_c.meta.ty=/=cap_type_t.CAP_TYPE_NONLIN))) 
  {
    ex_o_v := 1.U
    printf( "split ex_o %b\n",ex_o_v)

  }

  // if ((rs2 <= rs1_c.bounds.base) || (rs2 >= rs1_c.bounds.top)) 
  // when ((( rs2 <= rs1_c.bounds.base)===1.U) || (( rs2 >= rs1_c.bounds.top)===1.U)) 
  when ((( rs2 <= rs1_c.bounds.base)===1.U) || (( rs2 >= k)===1.U)) 
  {
   ex_o_v := 1.U
   printf( "split ex_o %b\n",ex_o_v)

  }

  // if RS1 == RD, insn is a nop: need to handle this case separately
  rs1_c.bounds.top    := rs2
  rs1_c.bounds.cursor := rs1_c.bounds.base
  rd_c.bounds.base    := rs2
  rd_c.bounds.cursor  := rs2
  printf( "split , new bound for two cap top,base: %b,%b,%b,%b\n",rs1_c.bounds.top,rs1_c.bounds.base,rd_c.bounds.top ,rd_c.bounds.base)
 
 // cap_compress_cap.cap_fat:=rs1_c
 // rs1_cc:= cap_compress_cap.cap_cc
 cap_compress_cap.cap_fat.bounds.base   := rs1_c.bounds.base   
 cap_compress_cap.cap_fat.bounds.top    := rs1_c.bounds.top     
 cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
 cap_compress_cap.cap_fat.renode_id     := rs1_c.renode_id      
 cap_compress_cap.cap_fat.reg_id        := rs1_c.reg_id         
 cap_compress_cap.cap_fat.async         := rs1_c.async         
 cap_compress_cap.cap_fat.padding       := rs1_c.padding
 cap_compress_cap.cap_fat.meta.perm     := rs1_c.meta.perm 
 cap_compress_cap.cap_fat.meta.ty       := rs1_c.meta.ty 
 rs2_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
 rs2_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
 rs2_cc.bounds.tE := cap_compress_cap.cap_cc.bounds.tE
 rs2_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
 rs2_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
 rs2_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
 rs2_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
 rs2_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
 rs2_cc.renode_id := cap_compress_cap.cap_cc.renode_id
 
 
  // cap_compress_cap.cap_fat:=rd_c
  // rd_cc:= cap_compress_cap.cap_cc
  cap_compress_cap.cap_fat.bounds.base:= rd_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rd_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rd_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rd_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     := rd_c.reg_id         
  cap_compress_cap.cap_fat.async      := rd_c.async         
  cap_compress_cap.cap_fat.padding    := rd_c.padding
  cap_compress_cap.cap_fat.meta.perm  := rd_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty    := rd_c.meta.ty 
 
  rd_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rd_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rd_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rd_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rd_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rd_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rd_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rd_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rd_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id
 
 
 // rs1_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};
 // rd_result_o = '{cap: 128'(rd_cc), tag: '1, valid: '1};
 
 csplit.cap:= Cat(rs2_cc.bounds.cursor , rs2_cc.bounds.b,rs2_cc.bounds.bE,rs2_cc.bounds.tE,rs2_cc.bounds.t,rs2_cc.bounds.iE,rs2_cc.meta.perm,rs2_cc.meta.ty,rs2_cc.renode_id)
 csplit.tag:= 1.U(1.W)
 csplit.valid:= 1.U(1.W)
 
 csplit2.cap  := Cat(rd_cc.bounds.cursor , rd_cc.bounds.b,rd_cc.bounds.bE,rd_cc.bounds.tE,rd_cc.bounds.t,rd_cc.bounds.iE,rd_cc.meta.perm,rd_cc.meta.ty,rd_cc.renode_id)
 csplit2.tag  := 1.U(1.W)
 csplit2.valid:= 1.U(1.W)
 
 //send_node_query(rs1_cc.renode_id, 1'b0);
 //send_node_alloc();
 //send_node_mut(rs1_cc.renode_id, NODE_SPLIT);
 send_node_query_c.ins.revnode_id:=rs2_cc.renode_id
 send_node_query_c.ins.synchronous:=0.U
 //initial
 //  send_node_query_c.ins.node_query_sent_q          := 0.U
 //  send_node_query_c.ins.node_query_ready_i         := 0.U
 //  send_node_query_c.ins.node_query_resp_valid_i    := 0.U
 //  send_node_query_c.ins.node_query_resp_received_q := 0.U 
 //  send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
 //  send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
 //  send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U 
 
 send_node_mut_c.ins.revnode_id:=rs2_cc.renode_id
 send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_SPLIT
 //initial
 //  send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
 //  send_node_mut_c.ins.have_alloc             := 0.U  
 printf( "split send_node_mut_c.ins.mut_ty,%b , %b , %b %b\n",rs1_c.renode_id ,rs2_cc.renode_id, rs1_cc.renode_id, send_node_mut_c.ins.mut_ty)
 printf( "split send_node_mut output %b,%b,%b\n",send_node_mut_c.ins.node_mut_valid_o, send_node_alloc_c.ins.node_alloc_valid_o,send_node_query_c.ins.node_query_valid_o )


}

class csealModule(implicit p: Parameters) extends XSModule {
    val src    = IO(Input(new fu_data_t))
    val cseal  = IO(Output(new cap_result_t )) 
    val cseal2 = IO(Output(new cap_result_t ))
    val  ex_o_v= IO(Output (UInt(1.W) ))
 
    ex_o_v:=0.U


   //unpacked capability
   val rs1_cc=Reg(new cap_cc_t)
   val rs1_c =Reg(new cap_fat_t)

   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31)//6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6
   rs1_cc.bounds.iE  := src.cap_a(37) //1
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64
   
  val cap_uncompress_cap=Module(new cap_uncompress)
  val cap_compress_cap  =Module(new cap_compress)

  // cap_uncompress_cap.cap_cc:=rs1_cc
  // rs1_c := cap_uncompress_cap.cap_fat
   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 

  printf( "seal param %b , %b , %b\n",src.tag_a, rs1_c.meta.ty,  rs1_cc.meta.perm)

  when ((src.tag_a=/=0.U) && (rs1_c.meta.ty =/= cap_type_t.CAP_TYPE_LINEAR) && (rs1_cc.meta.perm =/=cap_perm_t.CAP_PERM_RW))
  {
     printf( "seal ex_o %b\n",ex_o_v)
     ex_o_v := 1.U
  }          
  
  val size = rs1_c.bounds.top - rs1_c.bounds.base

  //if ((size < (CLENBYTES * 33 ))!=0.U || ((rs1_c.bounds.base & (CLENBYTES - 1.U))!=0.U))
  when ((size < 528.U )=/=0.U || ((rs1_c.bounds.base & 15.U)=/=0.U))//ToDo check condition again
  {
   printf( "seal ex_o %b\n",ex_o_v)
   ex_o_v := 1.U
  }

  rs1_c.meta.ty := cap_type_t.CAP_TYPE_SEALED
  rs1_c.reg_id  := 0.U(5.W)
  rs1_c.async   := 0.U(1.W)
  printf( "seal changed type, ID and Async %b , %b , %b\n",rs1_c.meta.ty, rs1_c.reg_id,  rs1_c.async)
       
 // check if base + clen, base + 2*clen mem location contains a capability
 // this probably means moving this insn to some other non-FLU unit
                
 // TODO: update this to work when RS1 == RD

 

  cseal.cap  := 0.U(128.W)
  cseal.tag  := 0.U(1.W)
  cseal.valid:= 1.U(1.W)

  // cap_compress_cap.cap_fat:=rs1_c
  // rs1_cc:= cap_compress_cap.cap_cc
  cap_compress_cap.cap_fat.bounds.base:= rs1_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rs1_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
  cap_compress_cap.cap_fat.async      := rs1_c.async         
  cap_compress_cap.cap_fat.padding := rs1_c.padding
  cap_compress_cap.cap_fat.meta.perm := rs1_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty := rs1_c.meta.ty 

  rs1_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rs1_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rs1_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rs1_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rs1_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rs1_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rs1_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rs1_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rs1_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id

  cseal2.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  cseal2.tag  := 1.U(1.W)
  cseal2.valid:= 1.U(1.W)

}

class cmrevModule (implicit p: Parameters) extends XSModule {
   val src     = IO(Input(new fu_data_t))
   val cmrev   = IO(Output(new cap_result_t ))  
   val cmrev2  = IO(Output(new cap_result_t )) 
   val  ex_o_v= IO(Output (UInt(1.W) ))

   ex_o_v:=0.U


   val send_node_query_ins  =IO(new send_node_query_bundle())
   val send_node_mut_ins    =IO(new send_node_mut_bundle())
   val send_node_alloc_ins  =IO(new send_node_alloc_bundle())

   val send_node_query_c    = Module(new send_node_query)
   val send_node_mut_c      = Module(new send_node_mut)
   val send_node_alloc_c    = Module(new send_node_alloc)

   send_node_query_ins<>send_node_query_c.ins
   send_node_mut_ins<>send_node_mut_c.ins
   send_node_alloc_ins<>send_node_alloc_c.ins
  

  //unpacked capability
  // val rs1_cc=Wire(new cap_cc_t)
  val rs1_cc=Reg(new cap_cc_t)

  val k = src.cap_a(33,31) //6

  rs1_cc.renode_id  := src.cap_a(30,0)//31
  rs1_cc.meta.ty    := src.cap_a(33,31) //6
  rs1_cc.meta.perm  := src.cap_a(36,34) //6
  rs1_cc.bounds.iE  := src.cap_a(37)   //1
  rs1_cc.bounds.t   := src.cap_a(46,38) //9
  rs1_cc.bounds.tE  := src.cap_a(49,47) //3
  rs1_cc.bounds.b   := src.cap_a(60,50) //11
  rs1_cc.bounds.bE  := src.cap_a(63,61)//3
  rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  // when ((src.tag_a=/=0.U)&&(rs1_cc.meta.ty === cap_type_t.CAP_TYPE_LINEAR)) 
  when ((src.tag_a=/=0.U)&&(k === cap_type_t.CAP_TYPE_LINEAR)) 
    {
    rs1_cc.meta.ty := cap_type_t.CAP_TYPE_REVOKE
    printf( "mrev new type  %b\n",rs1_cc.meta.ty)

    cmrev2.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
    cmrev2.tag  := 1.U(1.W)
    cmrev2.valid:= 1.U(1.W)

    // send_node_query(rs1_cc.renode_id, 1'b0);
    // send_node_alloc();
    // send_node_mut(rs1_cc.renode_id, NODE_MREV);
     send_node_query_c.ins.revnode_id:=rs1_cc.renode_id
     send_node_query_c.ins.synchronous:=0.U
     //initial
     send_node_query_c.ins.node_query_sent_q          := 0.U
     send_node_query_c.ins.node_query_ready_i         := 0.U
     send_node_query_c.ins.node_query_resp_valid_i    := 0.U
     send_node_query_c.ins.node_query_resp_received_q := 0.U 
     send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
     send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
     send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U

     send_node_mut_c.ins.revnode_id:=rs1_cc.renode_id
     send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_MREV
     //initial
     send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
     send_node_mut_c.ins.have_alloc             := 0.U    
    }
  .otherwise
  {

    cmrev2.cap  := 0.U
    cmrev2.tag  := 0.U(1.W)
    cmrev2.valid:= 0.U(1.W)
    printf( "mrev ex_o\n")

    //error
    ex_o_v := 1.U
  } 

    cmrev.cap  := 0.U
    cmrev.tag  := 0.U(1.W)
    cmrev.valid:= 0.U(1.W)

}

class cinitModule (implicit p: Parameters) extends XSModule {

  val src = IO(Input(new fu_data_t))
  val cinit  = IO(Output(new cap_result_t ))
  val cinit2 = IO(Output(new cap_result_t ))
  val ex_o_v = IO(Output (UInt(1.W) ))
  ex_o_v := 0.U

  //unpacked capability
  val rs1_cc=Reg(new cap_cc_t)
  val rs1_c =Reg(new cap_fat_t)

  val k1,k2 =Reg(UInt(64.W))
  val cap_uncompress_cap=Module(new cap_uncompress)
  val cap_compress_cap  =Module(new cap_compress)

  rs1_cc.renode_id  := src.cap_a(30,0)//31
  rs1_cc.meta.ty    := src.cap_a(33,31) //6
  rs1_cc.meta.perm  := src.cap_a(36,34) //6
  rs1_cc.bounds.iE  := src.cap_a(37)   //1
  rs1_cc.bounds.t   := src.cap_a(46,38) //9
  rs1_cc.bounds.tE  := src.cap_a(49,47) //3
  rs1_cc.bounds.b   := src.cap_a(60,50) //11
  rs1_cc.bounds.bE  := src.cap_a(63,61)//3
  rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  // cap_uncompress_cap.cap_cc:=rs1_cc
  // rs1_c := cap_uncompress_cap.cap_fat
  cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
  cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
  cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
  cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
  cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
  cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
  cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
  cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
  cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
  
  rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
  //  rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
  //  rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor  
   k1                  := cap_uncompress_cap.cap_fat.bounds.top
   k2                  := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
  //  rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 
   val k3       = cap_uncompress_cap.cap_fat.meta.ty 

   k1:=0.U
   k2:=0.U

  // when ((src.tag_a===0.U) || (src.tag_b===1.U) || (rs1_c.meta.ty =/= cap_type_t.CAP_TYPE_UNINIT) || (rs1_c.bounds.cursor =/= rs1_c.bounds.top))         
  when ((src.tag_a===0.U) || (src.tag_b===1.U) || (k3 =/= cap_type_t.CAP_TYPE_UNINIT) || (k1 =/= k2))         
  {
    printf( "cinit ex_o\n")
    ex_o_v := 1.U
  }

  rs1_c.meta.ty := cap_type_t.CAP_TYPE_LINEAR
  rs1_c.bounds.cursor := rs1_c.bounds.base + src.operand_b(4,0)
  
  printf( "cinit, new type , base, vaue, cursor: %b, %b , %b , %b\n", rs1_c.meta.ty,rs1_c.bounds.base, src.operand_b(4,0), rs1_c.bounds.cursor )

  // cap_compress_cap.cap_fat:=rs1_c
  // rs1_cc:= cap_compress_cap.cap_cc
  cap_compress_cap.cap_fat.bounds.base:= rs1_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rs1_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
  cap_compress_cap.cap_fat.async      := rs1_c.async         
  cap_compress_cap.cap_fat.padding := rs1_c.padding
  cap_compress_cap.cap_fat.meta.perm := rs1_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty := rs1_c.meta.ty 

  rs1_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rs1_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rs1_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rs1_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rs1_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rs1_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rs1_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rs1_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rs1_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id

  cinit.cap  := 0.U(128.W)
  cinit.tag  := 0.U(1.W)
  cinit.valid:= 1.U(1.W)


  cinit2.cap:= Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  cinit2.tag:= 1.U(1.W)
  cinit2.valid:= 1.U(1.W) 
  
  }
class cmovcModule (implicit p: Parameters) extends XSModule {

  val src    = IO(Input(new fu_data_t))
  val cmovc  = IO(Output(new cap_result_t ))
  val cmovc2 = IO(Output(new cap_result_t ))
  val ex_o_v = IO(Output (UInt(1.W) ))

  ex_o_v := 0.U

  //unpacked capability
  val rs1_cc=Wire(new cap_cc_t)

  rs1_cc.renode_id := src.cap_a(30,0)//31
  rs1_cc.meta.ty   := src.cap_a(33,31) //6
  rs1_cc.meta.perm  :=src.cap_a(36,34) //6
  rs1_cc.bounds.iE  :=src.cap_a(37) //3
  rs1_cc.bounds.t   :=src.cap_a(46,38) //9
  rs1_cc.bounds.tE  :=src.cap_a(49,47) //3
  rs1_cc.bounds.b   :=src.cap_a(60,50) //11
  rs1_cc.bounds.bE  :=src.cap_a(63,61)//3
  rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  when (src.tag_a===0.U)
  {
  //error/exeption
  ex_o_v := 1.U
  printf( "mov ex_o \n")
  }
  printf( "mov rs,rd = %b, %b \n", src.rs1 , src.rd)

  when (src.rs1 =/= src.rd)
  {
   when(rs1_cc.meta.ty=== cap_type_t.CAP_TYPE_NONLIN) 
    {
     printf( "mov , done \n")
     cmovc.cap:= 0.U(CLEN.W)
     cmovc.tag:= 0.U(1.W)
     cmovc.valid:= 0.U(1.W)

     cmovc2.cap:= src.cap_a
     cmovc2.tag:= 1.U(1.W)
     cmovc2.valid:= 1.U(1.W)
    }
    // else 
    .otherwise
    {
     printf( "mov , done \n")
     cmovc.cap:= 0.U(CLEN.W)
     cmovc.tag:= 0.U(1.W)
     cmovc.valid:= 1.U(1.W)

     cmovc2.cap:= src.cap_a
     cmovc2.tag:= 1.U(1.W)
     cmovc2.valid:= 1.U(1.W)
    }
  }
  .otherwise{
    printf( "mov , no_op \n")
    cmovc.cap:= 0.U(CLEN.W)
    cmovc.tag:= 0.U(1.W)
    cmovc.valid:= 0.U(1.W)

    cmovc2.cap:= 0.U
    cmovc2.tag:= 0.U(1.W)
    cmovc2.valid:= 0.U(1.W)
  }

  }

class cdropModule (implicit p: Parameters) extends XSModule {

   val src = IO(Input(new fu_data_t))
   val cdrop  = IO(Output(new cap_result_t ))
   val cdrop2 = IO(Output(new cap_result_t ))
   val  ex_o_v= IO(Output (UInt(1.W) ))

   ex_o_v:=0.U

   val rs1_cc=Wire(new cap_cc_t)

   rs1_cc.renode_id := Cat(src.cap_a(30,0))//31
   rs1_cc.meta.ty   := Cat(src.cap_a(33,31)) //6
   rs1_cc.meta.perm := Cat(src.cap_a(36,34)) //6
   rs1_cc.bounds.iE  := Cat(src.cap_a(37)) //3 
   rs1_cc.bounds.t   := Cat(src.cap_a(46,38)) //9
   rs1_cc.bounds.tE  := Cat(src.cap_a(49,47)) //3
   rs1_cc.bounds.b   := Cat(src.cap_a(60,50)) //11
   rs1_cc.bounds.bE  := Cat(src.cap_a(63,61))//3
   rs1_cc.bounds.cursor  := Cat(src.cap_a(127,64)) //64

   when (src.tag_a===0.U)
   {
      printf( "drop ex_o %b\n",ex_o_v)
      ex_o_v := 1.U
   }
    // validity check first?
    // followed by a nodedrop 
    // cannot change src as input data?!//ToDo//Capstone ?
   when (src.valid===1.U)
   {
    printf( "drop , src is valid, change to invalid\n")
    // src.valid:=0.U
   }
   .otherwise
   {
    printf( "drop , src is not valid, no-op\n")
   }

   cdrop.cap :=0.U 
   cdrop.tag:= 0.U(1.W)
   cdrop.valid:= 0.U(1.W)

   cdrop2.cap :=0.U 
   cdrop2.tag:= 0.U(1.W)
   cdrop2.valid:= 0.U(1.W)
}
class ccincoffsetModule (implicit p: Parameters) extends XSModule 
{
   val src = IO(Input(new fu_data_t))
   val ccincoffset  = IO(Output(new cap_result_t ))
   val ccincoffset2 = IO(Output(new cap_result_t ))
   val  ex_o_v= IO(Output (UInt(1.W) ))

   ex_o_v:=0.U

   //unpacked capability
   val rs1_cc=Reg(new cap_cc_t)
   val res   =Reg(new cap_cc_t)
   val rs1_c =Reg(new cap_fat_t)
   val rs2   =Reg(UInt(XLEN.W))
    
   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6
   rs1_cc.bounds.iE  := src.cap_a(37) //3    
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

   
   val cap_uncompress_cap=Module(new cap_uncompress)
   val cap_compress_cap  =Module(new cap_compress)

   // cap_uncompress_cap.cap_cc:=rs1_cc
   // rs1_c := cap_uncompress_cap.cap_fat

   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty 

   val k  = cap_uncompress_cap.cap_fat.bounds.cursor   


   when (src.tag_a===0.U || (src.tag_b=/=0.U) || (rs1_c.meta.ty===cap_type_t.CAP_TYPE_UNINIT || rs1_c.meta.ty===cap_type_t.CAP_TYPE_SEALED))
   {
    ex_o_v := 1.U
    printf( "cincoffset ex_o \n")

   }

   rs2 := src.operand_b(4,0)
   rs1_c.bounds.cursor := k + rs2
   printf("cincoffset old cursor, new cursor, offset : %b, %b, %b \n", k ,rs1_c.bounds.cursor, rs2)

   // cap_compress_cap.cap_fat:=rs1_c
   // res := cap_compress_cap.cap_cc
   cap_compress_cap.cap_fat.bounds.base   := rs1_c.bounds.base   
   cap_compress_cap.cap_fat.bounds.top    := rs1_c.bounds.top     
   cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
   cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
   cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
   cap_compress_cap.cap_fat.async      := rs1_c.async         
   cap_compress_cap.cap_fat.padding    := rs1_c.padding
   cap_compress_cap.cap_fat.meta.perm  := rs1_c.meta.perm 
   cap_compress_cap.cap_fat.meta.ty    := rs1_c.meta.ty 

   res.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
   res.bounds.t  := cap_compress_cap.cap_cc.bounds.t
   res.bounds.tE := cap_compress_cap.cap_cc.bounds.tE
   res.bounds.b  := cap_compress_cap.cap_cc.bounds.b
   res.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
   res.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
   res.meta.ty   :=  cap_compress_cap.cap_cc.meta.ty
   res.meta.perm :=  cap_compress_cap.cap_cc.meta.perm
   res.renode_id :=  cap_compress_cap.cap_cc.renode_id


   when (src.rs1 === src.rd) 
   {
   ccincoffset.cap  := 0.U
   ccincoffset.tag  := 0.U(1.W)
   ccincoffset.valid:= 0.U(1.W)

   ccincoffset2.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
   ccincoffset2.tag  := 1.U(1.W)
   ccincoffset2.valid:= 1.U(1.W)
   }
   .elsewhen (rs1_cc.meta.ty === cap_type_t.CAP_TYPE_NONLIN) 
   {
    ccincoffset.cap  := Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
    ccincoffset.tag  := 1.U(1.W)
    ccincoffset.valid:= 1.U(1.W)

    ccincoffset2.cap  := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
    ccincoffset2.tag  := 1.U(1.W)
    ccincoffset2.valid:= 1.U(1.W)

   }
   .otherwise 
   {// rs1 is a linear type
    ccincoffset.cap  := 0.U(128.W)
    ccincoffset.tag  := 0.U(1.W)
    ccincoffset.valid:= 1.U(1.W)
 

   ccincoffset2.cap  :=Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
   ccincoffset2.tag  := 1.U(1.W)
   ccincoffset2.valid:= 1.U(1.W)
    
   }

}

class ccincoffsetimModule(implicit p: Parameters) extends XSModule {
   val src = IO(Input(new fu_data_t))
   val ccincoffsetim  = IO(Output(new cap_result_t ))
   val ccincoffsetim2  = IO(Output(new cap_result_t ))
   val ccincoffsetim3  = IO(Output(new cap_result_t ))
   val  ex_o_v= IO(Output (UInt(1.W) ))

   ex_o_v:=0.U

   val rs1_cc =Reg(new cap_cc_t)
   val res    =Reg(new cap_cc_t)
   val rs1_c  =Reg(new cap_fat_t)
   val cap_uncompress_cap=Module(new cap_uncompress)
   val cap_compress_cap  =Module(new cap_compress)


  rs1_cc.renode_id := src.cap_a(30,0)//31
  rs1_cc.meta.ty    := src.cap_a(33,31) //6
  rs1_cc.meta.perm  := src.cap_a(36,34) //6
  rs1_cc.bounds.iE  := src.cap_a(37) //3    
  rs1_cc.bounds.t   := src.cap_a(46,38) //9
  rs1_cc.bounds.tE  := src.cap_a(49,47) //3
  rs1_cc.bounds.b   := src.cap_a(60,50) //11
  rs1_cc.bounds.bE  := src.cap_a(63,61)//3
  rs1_cc.bounds.cursor  :=src.cap_a(127,64) //64


  //  cap_uncompress_cap.cap_cc:=rs1_cc
  //  rs1_c := cap_uncompress_cap.cap_fat
   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

   val k = cap_uncompress_cap.cap_fat.bounds.cursor   

   when (src.tag_a===0.U || (rs1_c.meta.ty===cap_type_t.CAP_TYPE_UNINIT || rs1_c.meta.ty===cap_type_t.CAP_TYPE_SEALED)) 
   {
    ex_o_v := 1.U
    printf( "cincoffset ex_o_v \n")
   }

   rs1_c.bounds.cursor := k + src.imm(11,0)
   printf( "cincoffsetimm old cursor, new cursor, imm : %b, %b, %b\n", k ,rs1_c.bounds.cursor, src.imm(11,0) )
    
   cap_compress_cap.cap_fat:=rs1_c
   res := cap_compress_cap.cap_cc 
  //  if (src.rs1 == src.rd)        
   when (src.rs1 === src.rd)
   {
    // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1};
    ccincoffsetim.cap  := 0.U(128.W)
    ccincoffsetim.tag  := 0.U(1.W)
    ccincoffsetim.valid:= 0.U(1.W) 

    ccincoffsetim2.cap  :=Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
    ccincoffsetim2.tag  := 1.U(1.W)
    ccincoffsetim2.valid:= 1.U(1.W)     
  
    ccincoffsetim3.cap  := 0.U(128.W)
    ccincoffsetim3.tag  := 0.U(1.W)
    ccincoffsetim3.valid:= 0.U(1.W) 
   } 
   .elsewhen (rs1_cc.meta.ty === cap_type_t.CAP_TYPE_NONLIN)
   {
    // rs1_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};
    // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1}; 

    ccincoffsetim.cap:= Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
    ccincoffsetim.tag  := 1.U(1.W)
    ccincoffsetim.valid:= 1.U(1.W) 

    ccincoffsetim2.cap  :=Cat(res.bounds.cursor , res.bounds.b,res.bounds.bE,res.bounds.tE,res.bounds.t,res.bounds.iE,res.meta.perm,res.meta.ty,res.renode_id)
    ccincoffsetim2.tag  := 1.U(1.W)
    ccincoffsetim2.valid:= 1.U(1.W) 
   
    ccincoffsetim3.cap  := 0.U(128.W)
    ccincoffsetim3.tag  := 0.U(1.W)
    ccincoffsetim3.valid:= 0.U(1.W) 
   }
   .otherwise 
   {// rs1 is a linear type
    // rs1_result_o = '{cap: '0, tag: '0, valid: '1};
    // rd_result_o = '{cap: 128'(res), tag: '1, valid: '1};

    ccincoffsetim.cap  := 0.U(128.W)
    ccincoffsetim.tag  := 0.U(1.W)
    ccincoffsetim.valid:= 1.U(1.W) 

    ccincoffsetim2.cap  := 0.U(128.W)
    ccincoffsetim2.tag  := 0.U(1.W)
    ccincoffsetim2.valid:= 0.U(1.W) 

    ccincoffsetim3.cap  := src.operand_b
    ccincoffsetim3.tag  := 1.U(1.W)
    ccincoffsetim3.valid:= 1.U(1.W)  
   }
}

class ccallModule (implicit p: Parameters) extends XSModule 
{
  val src = IO(Input(new fu_data_t))
  val ccall  = IO(Output(new cap_result_t ))
  val ccall2  = IO(Output(new cap_result_t ))
  val ccall3  = IO(Output(new cap_result_t ))
  val  ex_o_v= IO(Output (UInt(1.W) ))

  ex_o_v:=0.U

  val rs1_cc =Reg(new cap_cc_t)
  val rs1_c  =Reg(new cap_fat_t)
  val cap_uncompress_cap=Module(new cap_uncompress)
  val cap_compress_cap  =Module(new cap_compress)


  val send_node_query_ins  =IO(new send_node_query_bundle())

  val send_node_query_c    = Module(new send_node_query)

  send_node_query_ins<>send_node_query_c.ins

   when (src.rs1 === 0.U(5.W))
   {
     // rs1_cc = capstone::cap_cc_t'(cih_i);
     // TODO: mark cih for later invalidation
   }
   .otherwise
   {
    rs1_cc.renode_id := src.cap_a(30,0)//31
    rs1_cc.meta.ty    := src.cap_a(33,31) //6
    rs1_cc.meta.perm  := src.cap_a(36,34) //6
    rs1_cc.bounds.iE  := Cat(src.cap_a(37)) //3    
    rs1_cc.bounds.t   := Cat(src.cap_a(46,38)) //9
    rs1_cc.bounds.tE  := Cat(src.cap_a(49,47)) //3
    rs1_cc.bounds.b   := Cat(src.cap_a(60,50)) //11
    rs1_cc.bounds.bE  := Cat(src.cap_a(63,61))//3
    rs1_cc.bounds.cursor  := Cat(src.cap_a(127,64)) //64

    }
   
  //  cap_uncompress_cap.cap_cc:=rs1_cc
  //  rs1_c := cap_uncompress_cap.cap_fat   
   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
   rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

   when (rs1_c.meta.ty =/= cap_type_t.CAP_TYPE_SEALED) 
   {
    ex_o_v := 1.U
   }

  rs1_c.meta.ty :=cap_type_t.CAP_TYPE_SEALEDRET
  rs1_c.bounds.cursor := rs1_c.bounds.base
  rs1_c.reg_id := src.rd

  // cap_compress_cap.cap_fat:=rs1_c
  // rs1_cc := cap_compress_cap.cap_cc

  cap_compress_cap.cap_fat.bounds.base:= rs1_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rs1_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
  cap_compress_cap.cap_fat.async      := rs1_c.async         
  cap_compress_cap.cap_fat.padding   := rs1_c.padding
  cap_compress_cap.cap_fat.meta.perm := rs1_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty   := rs1_c.meta.ty 

  rs1_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rs1_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rs1_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rs1_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rs1_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rs1_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rs1_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rs1_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rs1_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id

  ccall3.cap  :=  Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  ccall3.tag := 1.U(1.W)
  ccall3.valid:= 1.U(1.W)    


  ccall.cap  :=  0.U
  ccall.tag  := 0.U(1.W)
  ccall.valid:= 0.U(1.W)    

  ccall2.cap  := 0.U 
  ccall2.tag  := 0.U(1.W)
  ccall2.valid:= 0.U(1.W) 

  // capstone_valid_o = '1;
  // dom_switch_valid_o = 1'b1;
  // dom_switch_req_o.dom_base = rs1_c.bounds.base;
  // dom_switch_req_o.out_dom_base = rs1_c.bounds.base;
  //dom_switch_req_o.trans_id = fu_data_cur.trans_id;
  // dom_switch_req_o.is_full = 1'b0;
  // dom_switch_req_o.is_return = 1'b0;
  // dom_switch_req_o.pc_out = pc_cur + 'd4;

  // // send_node_query(rs1_c.renode_id, 1'b0);
  send_node_query_c.ins.revnode_id:=rs1_c.renode_id
  send_node_query_c.ins.synchronous                :=0.U
  //initial value
  // send_node_query_c.ins.node_query_sent_q          := 0.U
  // send_node_query_c.ins.node_query_ready_i         := 0.U
  // send_node_query_c.ins.node_query_resp_valid_i    := 0.U
  // send_node_query_c.ins.node_query_resp_received_q := 0.U 
  // send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
  // send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
  // send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U    
}

class creturnModule (implicit p: Parameters) extends XSModule 
{
   val src = IO(Input(new fu_data_t))
   val creturn   = IO(Output(new cap_result_t ))
   val creturn2  = IO(Output(new cap_result_t ))
   val creturn3  = IO(Output(new cap_result_t ))
   val  ex_o_v= IO(Output (UInt(1.W) ))
   ex_o_v:=0.U

   val send_node_query_ins  =IO(new send_node_query_bundle())
   val send_node_query_c    = Module(new send_node_query)
   send_node_query_ins<>send_node_query_c.ins

   val rd_cc =Reg(new cap_cc_t)
   val rd_c  =Reg(new cap_fat_t)
   val cap_uncompress_cap=Module(new cap_uncompress)
   val cap_compress_cap  =Module(new cap_compress)

   rd_cc.renode_id  := src.cap_c(30,0)//31
   rd_cc.meta.ty    := src.cap_c(33,31) //6
   rd_cc.meta.perm  := src.cap_c(36,34) //6
   rd_cc.bounds.iE  := Cat(src.cap_c(37)) //3    
   rd_cc.bounds.t   := Cat(src.cap_c(46,38)) //9
   rd_cc.bounds.tE  := Cat(src.cap_c(49,47)) //3
   rd_cc.bounds.b   := Cat(src.cap_c(60,50)) //11
   rd_cc.bounds.bE  := Cat(src.cap_c(63,61))//3
   rd_cc.bounds.cursor  := Cat(src.cap_a(127,64)) //64

  //  cap_uncompress_cap.cap_cc:=rd_cc
  //  rd_c := cap_uncompress_cap.cap_fat
   cap_uncompress_cap.cap_cc.bounds.iE  :=rd_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rd_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rd_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rd_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rd_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rd_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rd_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rd_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rd_cc.renode_id
   
   rd_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
   rd_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
   rd_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rd_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rd_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rd_c.async         := cap_uncompress_cap.cap_fat.async
   rd_c.padding       := cap_uncompress_cap.cap_fat.padding
   rd_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rd_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

   when (rd_c.meta.ty =/= cap_type_t.CAP_TYPE_SEALEDRET)
   {
     ex_o_v := 1.U
   }

  rd_c.meta.ty := cap_type_t.CAP_TYPE_SEALED
  rd_c.bounds.cursor := rd_c.bounds.base

  // When async == 1, we write back to cih in commit

  // cap_compress_cap.cap_fat:=rd_c
  // rd_cc := cap_compress_cap.cap_cc 
  cap_compress_cap.cap_fat.bounds.base:= rd_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rd_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rd_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rd_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rd_c.reg_id         
  cap_compress_cap.cap_fat.async      := rd_c.async         
  cap_compress_cap.cap_fat.padding   := rd_c.padding
  cap_compress_cap.cap_fat.meta.perm := rd_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty   := rd_c.meta.ty 

  rd_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rd_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rd_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rd_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rd_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rd_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rd_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rd_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rd_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id

  // $display("Return to %s", cap_type_t.cap_cc_format(rd_cc));
  creturn2.cap  := Cat(rd_cc.bounds.cursor , rd_cc.bounds.b,rd_cc.bounds.bE,rd_cc.bounds.tE,rd_cc.bounds.t,rd_cc.bounds.iE,rd_cc.meta.perm,rd_cc.meta.ty,rd_cc.renode_id)
  creturn2.tag  := 1.U(1.W)
  creturn2.valid:= 1.U(1.W)   

  // $display("capstone return (writing to %d: %s) @ %t", rd_c.reg_id, capstone::cap_fat_format(rd_c), $time);

  // dom_switch_valid_o = 1'b1;
  // dom_switch_req_o.dom_base = rd_c.bounds.base;
  // dom_switch_req_o.out_dom_base = rd_c.bounds.base;
  // dom_switch_req_o.trans_id = fu_data_cur.trans_id;
  // dom_switch_req_o.is_full = rd_c.async;
  // dom_switch_req_o.is_return = 1'b1;
  // dom_switch_req_o.pc_out = fu_data_cur.operand_a; // PC is in rs1
  // capstone_valid_o = '1;

  // rs2 provides the posted interrupts, which will be set in the
  // commit stage
  creturn3.cap  := src.operand_b
  creturn3.tag  := 0.U(1.W)
  creturn3.valid:= 1.U(1.W) 

  creturn.cap  := 0.U
  creturn.tag  := 0.U(1.W)
  creturn.valid:= 1.U(1.W) 

  // send_node_query(rd_c.renode_id, 1'b0);
  send_node_query_c.ins.revnode_id:=rd_c.renode_id
  send_node_query_c.ins.synchronous:=0.U
  
  // //initial value
  // send_node_query_c.ins.node_query_sent_q          := 0.U
  // send_node_query_c.ins.node_query_ready_i         := 0.U
  // send_node_query_c.ins.node_query_resp_valid_i    := 0.U
  // send_node_query_c.ins.node_query_resp_received_q := 0.U 
  // send_node_query_c.ins.node_query_resp_i.synchronous   := 0.U   
  // send_node_query_c.ins.node_query_resp_i.trans_id      := 0.U    
  // send_node_query_c.ins.node_query_resp_i.r_valid       := 0.U  
}

class ccjalrModule (implicit p: Parameters) extends XSModule {
  val src = IO(Input(new fu_data_t))
  val ccjalr  = IO(Output(new cap_result_t ))
    ccjalr.cap  := 0.U
    ccjalr.tag  := 0.U(1.W)
    ccjalr.valid:= 0.U(1.W)   
}
class ccbnzModule (implicit p: Parameters) extends XSModule {

  val src = IO(Input(new fu_data_t))
  val ccbnz  = IO(Output(new cap_result_t ))
    ccbnz.cap  := 0.U
    ccbnz.tag  := 0.U(1.W)
    ccbnz.valid:= 0.U(1.W)   
}
class cccsrrwModule (implicit p: Parameters) extends XSModule {
 
  val src = IO(Input(new fu_data_t))
  val cccsrrw  = IO(Output(new cap_result_t ))
    cccsrrw.cap  := 0.U
    cccsrrw.tag  := 0.U(1.W)
    cccsrrw.valid:= 0.U(1.W)    
}


class ccapenterModule(implicit p: Parameters) extends XSModule {
  val send_node_mut_ins  =IO(new send_node_mut_bundle())
  val send_node_mut_c    = Module(new send_node_mut)
  send_node_mut_ins<>send_node_mut_c.ins

  val src = IO(Input(new fu_data_t))
  val ccapenter  = IO(Output(new cap_result_t ))

  val cap0, cap1, pc_cap  =Wire(new cap_fat_t)

  
  val lo = Wire(UInt(XLEN.W))
  lo:=0.U
  val hi = Wire(UInt(XLEN.W))
  hi:=0.U
  when((src.rs1 === 0.U(5.W)) && (src.rs2 === 0.U(5.W))) 
  {
   lo := 0x40.U(XLEN.W)
   hi := 0x40.U(XLEN.W)
   }
   .otherwise//else
   {
    lo := src.operand_a
    hi := src.operand_b
   }
   cap0.bounds.cursor:=  0.U(64.W)
   cap0.bounds.base:=  0.U(64.W)
   cap0.bounds.top:= lo
   cap0.reg_id   := 0.U(1.W)
   cap0.async    := 0.U(1.W)
   cap0.meta.ty  :=cap_type_t.CAP_TYPE_LINEAR
   cap0.meta.perm:=cap_perm_t.CAP_PERM_RWX
   cap0.renode_id:= 0.U(31.W)
   cap0.padding  := 0.U
           
   cap1.bounds.cursor:=  hi
   cap1.bounds.base  :=  hi
   cap1.bounds.top:=0.U(64.W)
   cap1.reg_id   := 0.U(1.W)
   cap1.async    := 1.U(1.W)
   cap1.meta.ty  :=cap_type_t.CAP_TYPE_LINEAR
   cap1.meta.perm:=cap_perm_t.CAP_PERM_RWX
   cap1.renode_id:= 1.U(31.W)
   cap1.padding  := 0.U
   pc_cap.bounds.cursor:= lo
   pc_cap.bounds.base:=  lo
   pc_cap.bounds.top:= hi
   pc_cap.reg_id   := 0.U(1.W)
   pc_cap.async    := 1.U(1.W)
   pc_cap.meta.ty  :=cap_type_t.CAP_TYPE_LINEAR
   pc_cap.meta.perm:=cap_perm_t.CAP_PERM_RWX
   pc_cap.renode_id:= 2.U(31.W)
   pc_cap.padding  := 0.U
   // cms_result_o = '{
   //  cap0: capstone::cap_compress(cap0),
   //  cap1: capstone::cap_compress(cap1),
   //  pc_cap: capstone::cap_compress(pc_cap),
   //  valid: '1
  // send_node_mut(capstone::CAP_renode_id_NULL, NODE_INIT);
   send_node_mut_c.ins.revnode_id:=CAP_REVNODE_ID_NULL
   send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_INIT
    //initial
  send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
  send_node_mut_c.ins.have_alloc             := 0.U 
  
  ccapenter.cap  := 0.U(128.W) 
  ccapenter.tag  := 0.U(1.W)
  ccapenter.valid:= 0.U(1.W)
}

class ccreateModule(implicit p: Parameters) extends XSModule {
  val src = IO(Input(new fu_data_t))
  val ccreate  = IO(Output(new cap_result_t ))
  val ccreate2  = IO(Output(new cap_result_t ))

  val send_node_mut_ins  =IO(new send_node_mut_bundle())
  val send_node_mut_c    = Module(new send_node_mut)
  send_node_mut_ins<>send_node_mut_c.ins
 
  ccreate.cap  := 0.U(128.W) 
  ccreate.tag  := 0.U(1.W)
  ccreate.valid:= 0.U(1.W)

  ccreate2.cap  := 0.U(128.W) 
  ccreate2.tag  := 1.U(1.W)
  ccreate2.valid:= 1.U(1.W)

  // send_node_alloc();
  // send_node_mut(capstone::CAP_renode_id_NULL, NODE_CREATE);
  send_node_mut_c.ins.revnode_id:=CAP_REVNODE_ID_NULL
  send_node_mut_c.ins.mut_ty:=node_mut_type_t.NODE_CREATE 
  //initial
  send_node_mut_c.ins.node_alloc_node_id_cur := 0.U 
  send_node_mut_c.ins.have_alloc             := 0.U   
  }
class ctypeModule(implicit p: Parameters) extends XSModule {
    val src = IO(Input(new fu_data_t))
    val ctype  = IO(Output(new cap_result_t ))
    val ctype2  = IO(Output(new cap_result_t ))
    val rs1_cc=Wire(new cap_cc_t)

   rs1_cc.renode_id   := src.cap_a(30,0)//31
   rs1_cc.meta.ty     := src.operand_a(2,0)//src.cap_a(33,31) //6
   rs1_cc.meta.perm   := src.cap_a(36,34) //6
   rs1_cc.bounds.iE   := src.cap_a(37) //3  
   rs1_cc.bounds.t    := src.cap_a(46,38) //9
   rs1_cc.bounds.tE   := src.cap_a(49,47) //3
   rs1_cc.bounds.b    := src.cap_a(60,50) //11
   rs1_cc.bounds.bE   := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  //rd_result_o = '{cap: 128'(rs1_cc), tag: '1, valid: '1};

  ctype.cap  := 0.U(128.W) 
  ctype.tag  := 0.U(1.W)
  ctype.valid:= 0.U(1.W)

  ctype2.cap  :=Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  ctype2.tag  := 1.U(1.W)
  ctype2.valid:= 1.U(1.W)  
  }


class cnodeModule(implicit p: Parameters) extends XSModule {

   val src = IO(Input(new fu_data_t))
   val cnode  = IO(Output(new cap_result_t ))
   val cnode2  = IO(Output(new cap_result_t ))
   val rs1_cc=Wire(new cap_cc_t)


   rs1_cc.renode_id  := src.cap_a(30,0)//31
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6
   rs1_cc.bounds.iE  := src.cap_a(37) //3
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.renode_id := src.operand_a
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  cnode.cap  := 0.U(128.W) 
  cnode.tag  := 0.U(1.W)
  cnode.valid:= 0.U(1.W)

  cnode2.cap := Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  cnode2.tag := 1.U(1.W)
  cnode2.valid:= 1.U(1.W) 

  }
class cpermModule(implicit p: Parameters) extends XSModule {

   val src = IO(Input(new fu_data_t))
   val cperm  = IO(Output(new cap_result_t ))
   val cperm2  = IO(Output(new cap_result_t ))
   val rs1_cc=Wire(new cap_cc_t)

   rs1_cc.renode_id := src.cap_c(30,0)//31
   rs1_cc.meta.ty   := src.operand_a(2,0)
   rs1_cc.meta.perm := src.operand_a(2,0)
   rs1_cc.bounds.iE  := src.cap_c(37) //3
   rs1_cc.bounds.t   := src.cap_c(46,38) //9
   rs1_cc.bounds.tE  := src.cap_c(49,47) //3
   rs1_cc.bounds.b   := src.cap_c(60,50) //11
   rs1_cc.bounds.bE  := src.cap_c(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_c(127,64) //64

  cperm.cap  := 0.U(128.W) 
  cperm.tag  := 0.U(1.W)
  cperm.valid:= 0.U(1.W)

  cperm2.cap:= Cat(rs1_cc.renode_id,rs1_cc.meta.ty,rs1_cc.meta.perm, rs1_cc.renode_id, rs1_cc.meta.ty , rs1_cc.meta.perm, rs1_cc.bounds.iE , rs1_cc.bounds.t , rs1_cc.bounds.tE , rs1_cc.bounds.b , rs1_cc.bounds.bE ,rs1_cc.bounds.cursor)    
  cperm2.tag:= 1.U(1.W)
  cperm2.valid:= 1.U(1.W) 

}
class cboundModule(implicit p: Parameters) extends XSModule {
   val src      = IO(Input(new fu_data_t))
   val cbound   = IO(Output(new cap_result_t ))
   val cbound2  = IO(Output(new cap_result_t ))
   val rs1_cc=Reg(new cap_cc_t)
   val rs1_c =Reg(new cap_fat_t)
   val cap_uncompress_cap=Module(new cap_uncompress)
   val cap_compress_cap  =Module(new cap_compress)


   rs1_cc.renode_id  := Cat(src.cap_a(30,0))//31
   rs1_cc.meta.ty    := Cat(src.cap_a(33,31)) //6
   rs1_cc.meta.perm  := Cat(src.cap_a(36,34)) //6
   rs1_cc.bounds.iE  := Cat(src.cap_a(37)) //3    
   rs1_cc.bounds.t   := Cat(src.cap_c(46,38)) //9
   rs1_cc.bounds.tE  := Cat(src.cap_c(49,47)) //3
   rs1_cc.bounds.b   := Cat(src.cap_c(60,50)) //11
   rs1_cc.bounds.bE  := Cat(src.cap_c(63,61))//3
   rs1_cc.bounds.cursor  := Cat(src.cap_c(127,64)) //64


   // cap_uncompress_cap.cap_cc:=rs1_cc
   // rs1_c := cap_uncompress_cap.cap_fat
   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
  //  rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
  //  rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
  //  rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

  rs1_c.bounds.cursor := src.operand_a
  rs1_c.bounds.base   := src.operand_a
  rs1_c.bounds.top    := src.operand_b

  cap_compress_cap.cap_fat.bounds.base:= rs1_c.bounds.base   
  cap_compress_cap.cap_fat.bounds.top := rs1_c.bounds.top     
  cap_compress_cap.cap_fat.bounds.cursor := rs1_c.bounds.cursor     
  cap_compress_cap.cap_fat.renode_id  := rs1_c.renode_id      
  cap_compress_cap.cap_fat.reg_id     :=rs1_c.reg_id         
  cap_compress_cap.cap_fat.async      := rs1_c.async         
  cap_compress_cap.cap_fat.padding    := rs1_c.padding
  cap_compress_cap.cap_fat.meta.perm  := rs1_c.meta.perm 
  cap_compress_cap.cap_fat.meta.ty    := rs1_c.meta.ty 

  rs1_cc.bounds.iE := cap_compress_cap.cap_cc.bounds.iE
  rs1_cc.bounds.t  := cap_compress_cap.cap_cc.bounds.t
  rs1_cc.bounds.tE :=cap_compress_cap.cap_cc.bounds.tE
  rs1_cc.bounds.b  := cap_compress_cap.cap_cc.bounds.b
  rs1_cc.bounds.bE := cap_compress_cap.cap_cc.bounds.bE   
  rs1_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
  rs1_cc.meta.ty   := cap_compress_cap.cap_cc.meta.ty
  rs1_cc.meta.perm := cap_compress_cap.cap_cc.meta.perm
  rs1_cc.renode_id :=  cap_compress_cap.cap_cc.renode_id

  cbound.cap  := 0.U(128.W) 
  cbound.tag  := 0.U(1.W)
  cbound.valid:= 0.U(1.W)

  cbound2.cap:= Cat(rs1_cc.bounds.cursor , rs1_cc.bounds.b,rs1_cc.bounds.bE,rs1_cc.bounds.tE,rs1_cc.bounds.t,rs1_cc.bounds.iE,rs1_cc.meta.perm,rs1_cc.meta.ty,rs1_cc.renode_id)
  cbound2.valid:= 1.U(1.W) 
  cbound2.tag  := 1.U(1.W)

}
class cprintModule(implicit p: Parameters) extends XSModule {
    val src     = IO(Input(new fu_data_t))
    val cprint  = IO(Output(new cap_result_t ))


   val rs1_cc=Wire(new cap_cc_t)
   val rs1_c =Wire(new cap_fat_t)

   val cap_uncompress_cap=Module(new cap_uncompress)
   rs1_cc.renode_id  := src.cap_a(30,0)//31d
   rs1_cc.meta.ty    := src.cap_a(33,31) //6
   rs1_cc.meta.perm  := src.cap_a(36,34) //6  
   rs1_cc.bounds.iE  := src.cap_a(37) //3    
   rs1_cc.bounds.t   := src.cap_a(46,38) //9
   rs1_cc.bounds.tE  := src.cap_a(49,47) //3
   rs1_cc.bounds.b   := src.cap_a(60,50) //11
   rs1_cc.bounds.bE  := src.cap_a(63,61)//3
   rs1_cc.bounds.cursor  := src.cap_a(127,64) //64

  // cap_uncompress_cap.cap_cc:=rs1_cc
  // rs1_c := cap_uncompress_cap.cap_fat

   cap_uncompress_cap.cap_cc.bounds.iE  :=rs1_cc.bounds.iE
   cap_uncompress_cap.cap_cc.bounds.t   :=rs1_cc.bounds.t
   cap_uncompress_cap.cap_cc.bounds.tE  :=rs1_cc.bounds.tE
   cap_uncompress_cap.cap_cc.bounds.b   :=rs1_cc.bounds.b
   cap_uncompress_cap.cap_cc.bounds.bE  :=rs1_cc.bounds.bE   
   cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
   cap_uncompress_cap.cap_cc.meta.ty     :=rs1_cc.meta.ty
   cap_uncompress_cap.cap_cc.meta.perm   :=rs1_cc.meta.perm
   cap_uncompress_cap.cap_cc.renode_id   :=rs1_cc.renode_id
   
  //  rs1_c.bounds.base   := cap_uncompress_cap.cap_fat.bounds.base
  //  rs1_c.bounds.top    := cap_uncompress_cap.cap_fat.bounds.top
  //  rs1_c.bounds.cursor := cap_uncompress_cap.cap_fat.bounds.cursor   
   rs1_c.renode_id     := cap_uncompress_cap.cap_fat.renode_id
   rs1_c.reg_id        := cap_uncompress_cap.cap_fat.reg_id
   rs1_c.async         := cap_uncompress_cap.cap_fat.async
   rs1_c.padding       := cap_uncompress_cap.cap_fat.padding
   rs1_c.meta.perm     := cap_uncompress_cap.cap_fat.meta.perm
   rs1_c.meta.ty       := cap_uncompress_cap.cap_fat.meta.ty

  rs1_c.bounds.cursor:= src.operand_a
  rs1_c.bounds.base  := src.operand_a
  rs1_c.bounds.top   := src.operand_b
  when (src.tag_a =/=0.U) 
  {
  cprint.cap  := 0.U(128.W) 
  cprint.tag  := 0.U(1.W)
  cprint.valid:= 0.U(1.W)
  }

  cprint.cap  := 0.U(128.W) 
  cprint.tag  := 0.U(1.W)
  cprint.valid:= 0.U(1.W)

}
class cregprintModule(implicit p: Parameters) extends XSModule {
  val src = IO(Input(new fu_data_t))
  val cregprint  = IO(Output(new cap_result_t )) 

  cregprint.cap  := 0.U(128.W) 
  cregprint.tag  := 0.U(1.W)
  cregprint.valid:= 0.U(1.W)


}

class getrandModule(implicit p: Parameters) extends XSModule {
  val src      = IO(Input(new fu_data_t))
  val getrand  = IO(Output(new cap_result_t ))

  getrand.cap  := 0.U(128.W) 
  getrand.tag  := 0.U(1.W)
  getrand.valid:= 0.U(1.W)

}
class tagsetModule(implicit p: Parameters) extends XSModule {
  val src     = IO(Input(new fu_data_t))
  val tagset  = IO(Output(new cap_result_t ))

  tagset.cap  := 0.U(128.W) 
  tagset.tag  := 0.U(1.W)
  tagset.valid:= 0.U(1.W)

}
class taggetModule(implicit p: Parameters) extends XSModule {

    val src = IO(Input(new fu_data_t))
    val tagget  = IO(Output(new cap_result_t ))

  tagget.cap  := 0.U(128.W) 
  tagget.tag  := 0.U(1.W)
  tagget.valid:= 0.U(1.W)
}

class AddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val srcw = Input(UInt((XLEN/2).W))
    val add  = Output(UInt(XLEN.W))
    val addw = Output(UInt((XLEN/2).W))
  })
  io.add := io.src(0) + io.src(1)
  // TODO: why this extra adder?
  io.addw := io.srcw + io.src(1)(31,0)
  // Debug Info//mlabaf  
  XSDebug("instruction is additon XSD=%b\n",io.src(0))
  
}

class SubModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val sub = Output(UInt((XLEN+1).W))
  })
  io.sub := (io.src(0) +& (~io.src(1)).asUInt()) + 1.U
 }

class LeftShiftModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val revShamt = Input(UInt(6.W))
    val sllSrc = Input(UInt(XLEN.W))
    val sll = Output(UInt(XLEN.W))
    val revSll = Output(UInt(XLEN.W))
  })
  io.sll := io.sllSrc << io.shamt
  io.revSll := io.sllSrc << io.revShamt
}

class LeftShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val revShamt = Input(UInt(5.W))
    val sllSrc = Input(UInt((XLEN/2).W))
    val sllw = Output(UInt((XLEN/2).W))
    val revSllw = Output(UInt((XLEN/2).W))
  })
  io.sllw := io.sllSrc << io.shamt
  io.revSllw := io.sllSrc << io.revShamt
}

class RightShiftModule(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle() {
    val shamt = Input(UInt(6.W))
    val revShamt = Input(UInt(6.W))
    val srlSrc, sraSrc = Input(UInt(XLEN.W))
    val srl, sra = Output(UInt(XLEN.W))
    val revSrl = Output(UInt(XLEN.W))
  })
  io.srl  := io.srlSrc >> io.shamt
  io.sra  := (io.sraSrc.asSInt() >> io.shamt).asUInt()
  io.revSrl  := io.srlSrc >> io.revShamt
}

class RightShiftWordModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val shamt = Input(UInt(5.W))
    val revShamt = Input(UInt(5.W))
    val srlSrc, sraSrc = Input(UInt((XLEN/2).W))
    val srlw, sraw = Output(UInt((XLEN/2).W))
    val revSrlw = Output(UInt((XLEN/2).W))
  })

  io.srlw := io.srlSrc >> io.shamt
  io.sraw := (io.sraSrc.asSInt() >> io.shamt).asUInt()
  io.revSrlw := io.srlSrc >> io.revShamt
}


class MiscResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(6.W))
    val and, or, xor, orcb, orh48, sextb, packh, sexth, packw, revb, rev8, pack = Input(UInt(XLEN.W))
    val src = Input(UInt(XLEN.W))
    val miscRes = Output(UInt(XLEN.W))
  })

  val logicRes = VecInit(Seq(
    io.and,
    io.or,
    io.xor,
    io.orcb
  ))(io.func(2, 1))
  val miscRes = VecInit(Seq(io.sextb, io.packh, io.sexth, io.packw))(io.func(1, 0))
  val logicBase = Mux(io.func(3), miscRes, logicRes)

  val revRes = VecInit(Seq(io.revb, io.rev8, io.pack, io.orh48))(io.func(1, 0))
  val customRes = VecInit(Seq(
    Cat(0.U(31.W), io.src(31, 0), 0.U(1.W)),
    Cat(0.U(30.W), io.src(31, 0), 0.U(2.W)),
    Cat(0.U(29.W), io.src(31, 0), 0.U(3.W)),
    Cat(0.U(56.W), io.src(15, 8))))(io.func(1, 0))
  val logicAdv = Mux(io.func(3), customRes, revRes)

  val mask = Cat(Fill(15, io.func(0)), 1.U(1.W))
  val maskedLogicRes = mask & logicRes

  io.miscRes := Mux(io.func(5), maskedLogicRes, Mux(io.func(4), logicAdv, logicBase))
}

 class ShiftResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt(4.W))
    val sll, srl, sra, rol, ror, bclr, bset, binv, bext = Input(UInt(XLEN.W))
    val shiftRes = Output(UInt(XLEN.W))
  })

  // val leftBit  = Mux(io.func(1), io.binv, Mux(io.func(0), io.bset, io.bclr))
  // val leftRes  = Mux(io.func(2), leftBit, io.sll)
  // val rightRes = Mux(io.func(1) && io.func(0), io.sra, Mux(io.func(1), io.bext, io.srl))
  val resultSource = VecInit(Seq(
    io.sll,
    io.sll,
    io.bclr,
    io.bset,
    io.binv,
    io.srl,
    io.bext,
    io.sra
  ))
  val simple = resultSource(io.func(2, 0))

  io.shiftRes := Mux(io.func(3), Mux(io.func(1), io.ror, io.rol), simple)
}

class WordResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())
    val sllw, srlw, sraw, rolw, rorw, addw, subw = Input(UInt((XLEN/2).W))
    val wordRes = Output(UInt(XLEN.W))
  })

  val addsubRes = Mux(!io.func(2) && io.func(1), io.subw, io.addw)
  val shiftRes = Mux(io.func(2), Mux(io.func(0), io.rorw, io.rolw),
                  Mux(io.func(1), io.sraw, Mux(io.func(0), io.srlw, io.sllw)))
  val wordRes = Mux(io.func(3), shiftRes, addsubRes)
  io.wordRes := SignExt(wordRes, XLEN)
}

class capResultSelect(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val func = Input(UInt())

  })
 
    val   crevoke,cshrink,ctighten,cdelin,clcc,cscc,csplit,cseal,cmrev,cinit,cmovc,cdrop,ccincoffset,
          ccall,creturn,ccjalr,ccbnz,cccsrrw,ccapenter,ccincoffsetim,cshrinkto,ccreate,ctype,
          cnode,cperm,cbound,cprint,cregprint,getrand,tagset,tagget= IO(Input(new cap_result_t))
    // val capRes = Output(UInt(XLEN.W))
    val capRes = IO(Output(new cap_result_t))

  capRes.cap        := 0.U
  capRes.tag        := 0.U 
  capRes.valid      := 0.U

  switch(io.func)
  {
  is (0.U)
   {
    capRes.cap:=crevoke.cap
    capRes.tag:=crevoke.tag
    capRes.valid:=crevoke.valid
    }
  is (1.U){
    capRes.cap:=cshrink.cap
    capRes.tag:=cshrink.tag
    capRes.valid:=cshrink.valid
  
  }
  is (2.U){
    capRes.cap:=ctighten.cap
    capRes.tag:=ctighten.tag
    capRes.valid:=ctighten.valid
  }
  is (3.U){
    capRes.cap:=cdelin.cap
    capRes.tag:=cdelin.tag
    capRes.valid:=cdelin.valid
  }    
  is (4.U){
    capRes.cap:=clcc.cap
    capRes.tag:=clcc.tag
    capRes.valid:=clcc.valid
  }

  is (5.U){
    capRes.cap:=cscc.cap
    capRes.tag:=cscc.tag
    capRes.valid:=cscc.valid
  }
  is (6.U){
    capRes.cap:=csplit.cap
    capRes.tag:=csplit.tag
    capRes.valid:=csplit.valid
  }
  is (7.U){
    capRes.cap:=cseal.cap
    capRes.tag:=cseal.tag
    capRes.valid:=cseal.valid
  }
  is (8.U){
    capRes.cap:=cmrev.cap
    capRes.tag:=cmrev.tag
    capRes.valid:=cmrev.valid
  }
  is (9.U){
    capRes.cap:=cinit.cap
    capRes.tag:=cinit.tag
    capRes.valid:=cinit.valid
  }
  is (10.U){
    capRes.cap:=cmovc.cap
    capRes.tag:=cmovc.tag
    capRes.valid:=cmovc.valid
  }
  is (11.U){
    capRes.cap:=cdrop.cap
    capRes.tag:=cdrop.tag
    capRes.valid:=cdrop.valid
  }    
  is (12.U){
    capRes.cap:=ccincoffset.cap
    capRes.tag:=ccincoffset.tag
    capRes.valid:=ccincoffset.valid
  }
  is (13.U){
    capRes.cap:=ccall.cap
    capRes.tag:=ccall.tag
    capRes.valid:=ccall.valid
  }    
  is (14.U){
    capRes.cap:=creturn.cap
    capRes.tag:=creturn.tag
    capRes.valid:=creturn.valid
  }
  is (15.U){
    capRes.cap:=ccjalr.cap
    capRes.tag:=ccjalr.tag
    capRes.valid:=ccjalr.valid
  }
  is (16.U){
    capRes.cap:=ccbnz.cap
    capRes.tag:=ccbnz.tag
    capRes.valid:=ccbnz.valid
  }
  is (17.U){
    capRes.cap:=cccsrrw.cap
    capRes.tag:=cccsrrw.tag
    capRes.valid:=cccsrrw.valid
  }  
  is (18.U){
    capRes.cap:=ccapenter.cap
    capRes.tag:=ccapenter.tag
    capRes.valid:=ccapenter.valid
  }
  is (19.U){
    capRes.cap:=ccincoffsetim.cap
    capRes.tag:=ccincoffsetim.tag
    capRes.valid:=ccincoffsetim.valid  
  }
  is (20.U){
    capRes.cap:=cshrinkto.cap
    capRes.tag:=cshrinkto.tag
    capRes.valid:=cshrinkto.valid
  }  
  is (21.U){
    capRes.cap:=ccreate.cap
    capRes.tag:=ccreate.tag
    capRes.valid:=ccreate.valid
  }  
  is (22.U){
    capRes.cap:=ctype.cap
    capRes.tag:=ctype.tag
    capRes.valid:=ctype.valid
  }  
  is (23.U){
    capRes.cap:=cnode.cap
    capRes.tag:=cnode.tag
    capRes.valid:=cnode.valid
  }  
  is (24.U){
    capRes.cap:=cperm.cap
    capRes.tag:=cperm.tag
    capRes.valid:=cperm.valid
  }  
  is (25.U){
    capRes.cap:=cbound.cap
    capRes.tag:=cbound.tag
    capRes.valid:=cbound.valid
  }  
  is (26.U){
    capRes.cap:=cprint.cap
    capRes.tag:=cprint.tag
    capRes.valid:=cprint.valid
  }  
  is (27.U){
    capRes.cap:=cregprint.cap
    capRes.tag:=cregprint.tag
    capRes.valid:=cregprint.valid
  }  
  is (28.U){
    capRes.cap:=getrand.cap
    capRes.tag:=getrand.tag
    capRes.valid:=getrand.valid
  }  
  is (29.U){
    capRes.cap:=tagset.cap
    capRes.tag:=tagset.tag
    capRes.valid:=tagset.valid
  }  
  is (30.U){
    capRes.cap:=tagget.cap
    capRes.tag:=tagget.tag
    capRes.valid:=tagget.valid
  }  
  //is (31.U){io.capRes:=SignExt(io.sdd, XLEN)}
  }

  // io.capRes := res// SignExt(res, XLEN)
   //io.capRes:=SignExt(io.ccapenter, XLEN)
  //printf("instruction code in capstone 1 what")  
  XSDebug("instruction in alu capstone_select =%b\n",io.func)

}
 class AluResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
  val func = Input(UInt(3.W))
   // val func = Input(UInt(4.W))
  val addRes, shiftRes, miscRes, compareRes, wordRes = Input(UInt(XLEN.W))
   // val capRes,addRes, shiftRes, miscRes, compareRes, wordRes = Input(UInt(XLEN.W))
  val aluRes = Output(UInt(XLEN.W))
  })

  val res = Mux(io.func(2, 1) === 0.U, Mux(io.func(0), io.wordRes, io.shiftRes),
           Mux(!io.func(2), Mux(io.func(0), io.compareRes, io.addRes), io.miscRes))
  // val res = Mux(io.func(3),io.capRes,Mux(io.func(2, 1) === 0.U, Mux(io.func(0), io.wordRes, io.shiftRes),
  //           Mux(!io.func(2), Mux(io.func(0), io.compareRes, io.addRes), io.miscRes)))            
  io.aluRes := res

  //printf("instruction in aluresselect is capstone print")  
  XSDebug("instruction is aluresselect=%b\n",io.func)
  
}

//capstone//mlabaf
class AluCapResSel(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
  //  val func = Input(UInt(3.W))
    // val func = Input(UInt(4.W))
    val func = Input(UInt(9.W))
  //  val addRes, shiftRes, miscRes, compareRes, wordRes = Input(UInt(XLEN.W))
    val capRes = Input (new cap_result_t)
    val AluRes = Input(UInt(XLEN.W))
    val ExRes = Output(UInt(XLEN.W))
    val ExResValid = Output(UInt(1.W))
    val ExResTag = Output(UInt(1.W))
    
  })

  val Res      = Mux(io.func(7),io.capRes.cap,io.AluRes)
  val ResValid = Mux(io.func(7),io.capRes.valid,0.U)
  val ResTag   = Mux(io.func(7),io.capRes.tag,0.U)

  io.ExRes := Res
  io.ExResValid := ResValid
  io.ExResTag := ResTag

  XSDebug("AluCapResSel =%b\n",io.func)
  
}

 class CapExeModule_bundle extends Bundle {
    val flush_i =Input( UInt (1.W))
    val pc_i    =Input( UInt (VLEN.W))               // PC of instruction

    val  ex_o               =Output ( new exception_t )
    val  cms_result_o       =Output ( new capstone_mode_switch_t )
    val capstone_valid_i    =Input  ( UInt (1.W))     // FU validity signal
    val capstone_valid_o    =Output ( UInt (1.W))      // Result validity signal to issue
    val capstone_ready_o    =Output ( UInt (1.W))                    
    val capstone_trans_id_o =Output ( UInt (TRANS_ID_BITS.W)) 
    val cih_i               =Input  ( UInt (128.W)) //clen_t          
    // domain switcher
    val dom_switch_valid_o  =Output ( UInt (1.W))                   
    val dom_switch_req_o    =Output(new dom_switch_req_t)          
    // node unit interface
    // query
    val node_query_valid_o  = Output ( UInt (1.W))  
    val node_query_ready_i  = Input  ( UInt (1.W))  
    val node_query_o        = Output ( new node_query_t) 
    val node_query_resp_valid_i= Input ( UInt (1.W)) 
    val node_query_resp_i   = Input  ( new node_query_resp_t) 
    // allocation
    val node_alloc_valid_o = Output ( UInt (1.W))  
    val node_alloc_ready_i = Input  ( UInt (1.W))  
    val node_alloc_resp_valid_i = Input  ( UInt (1.W))  
    val node_alloc_node_id_i   = Input (UInt(31.W))//cap_revnode_id_t)//? 
    // mutation
    val node_mut_valid_o = Output ( UInt (1.W))  
    val node_mut_o       = Output ( new node_mut_t )
    // val fu_data_i        = Input ( new fu_data_t )  
    }


//Capstone//mlabaf
class CapExeModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
  val func      = Input(FuOpType())
  val operand_a =Input( UInt (XLEN.W))
  val operand_b =Input( UInt (XLEN.W))
  val imm       =Input( UInt (XLEN.W))
  val cap_a     =Input( UInt (128.W))//clen_t
  val cap_b     =Input( UInt (128.W))//clen_t
  val cap_c     =Input( UInt (128.W))//clen_t
  val tag_a     =Input( UInt (1.W))
  val tag_b     =Input( UInt (1.W))
  val tag_c     =Input( UInt (1.W))
  val valid     =Input( UInt (1.W)) 
  val rd        =Input( UInt (5.W))
  val rs1       =Input( UInt (5.W))
  val rs2       =Input( UInt (5.W))  
  val trans_id  =Input( UInt (TRANS_ID_BITS.W))  
 //////////////////////////////////////////////////

 })  

 val CapExe_ins  =IO(new CapExeModule_bundle())
 val capRes  = IO(Output( new cap_result_t ))



 //  def is_full   = UInt (1.W)
 def is_return = UInt (1.W) // if this is return, ra is overwritten
 def dom_base  = UInt (XLEN.W)
 def out_dom_base = UInt (XLEN.W)
 def pc_out    = UInt (XLEN.W) // pc to swap out
 def trans_id  = UInt (TRANS_ID_BITS.W)


 
  CapExe_ins.capstone_valid_o    :=0.U
  CapExe_ins.capstone_ready_o    :=0.U                   
  CapExe_ins.capstone_trans_id_o :=0.U

  CapExe_ins.dom_switch_valid_o  :=0.U 

  CapExe_ins.node_alloc_valid_o  :=0.U 
  CapExe_ins.node_mut_valid_o    :=0.U   

  CapExe_ins.ex_o.causeriscv :=0.U
  CapExe_ins.ex_o.tvalriscv  :=0.U
  CapExe_ins.ex_o.is_virt    :=0.U
  CapExe_ins.ex_o.valid      :=0.U
  
  CapExe_ins.dom_switch_req_o.is_full     :=1.U(1.W)         
  CapExe_ins.dom_switch_req_o.is_return   :=0.U         
  CapExe_ins.dom_switch_req_o.dom_base    :=0.U         
  CapExe_ins.dom_switch_req_o.out_dom_base:=0.U         
  CapExe_ins.dom_switch_req_o.pc_out      :=0.U         
  CapExe_ins.dom_switch_req_o.trans_id    :=0.U   
  
  CapExe_ins.node_mut_o.node_id       :=0.U
  CapExe_ins.node_mut_o.node_op       :=0.U
  CapExe_ins.node_mut_o.alloc         :=0.U
  CapExe_ins.node_mut_o.new_node_id   :=0.U

  CapExe_ins.node_query_valid_o  :=0.U

  CapExe_ins.node_query_o.synchronous:=0.U
  CapExe_ins.node_query_o.trans_id   :=0.U
  CapExe_ins.node_query_o.node_id    :=0.U

  CapExe_ins.cms_result_o.pc_cap  :=0.U(128.W)
  CapExe_ins.cms_result_o.valid   :=0.U(1.W)   
  CapExe_ins.cms_result_o.cap0    :=0.U(128.W)
  CapExe_ins.cms_result_o.cap1    :=0.U(128.W)
  // val src  = IO(Input(new fu_data_t))
  //src is fu_data_i
  val src  = Wire(new fu_data_t)
  src.operand_a := io.operand_a 
  src.operand_b := io.operand_b
  src.imm   := io.imm    
  src.cap_a := io.cap_a   
  src.cap_b := io.cap_b   
  src.cap_c := io.cap_c   
  src.tag_a := io.tag_a   
  src.tag_b := io.tag_b   
  src.tag_c := io.tag_c    
  src.valid := io.valid  
  src.rd    := io.rd    
  src.rs1   := io.rs1   
  src.rs2   := io.rs2 
  src.trans_id   := io.trans_id  
 //////////////////////output initial value//////////////////

 //////////////////needed node value///////////////
  val node_alloc_state_d   = Reg(UInt(4.W))
  val node_alloc_valid_o   = Reg(UInt(1.W)) 
  val node_alloc_ready_i   = Reg(UInt(1.W))
  val node_alloc_state_q   = RegInit(0.U(4.W))
  val node_alloc_resp_valid_i = Reg(UInt(31.W))//cap_renod_id_t
  val node_alloc_node_id_i    = Reg(UInt(31.W))//cap_renod_id_t
  
  val revnode_id       = RegInit(0.U(31.W))//Wire( cap_renode_id_t)
  val node_alloc_node_id_cur = RegInit(0.U(31.W))//Reg( cap_renode_id_t)
  val mut_ty           = Reg(UInt(4.W))
  val have_alloc       = RegInit(0.U(1.W)) 
  val node_mut_valid_o = Reg(UInt(1.W))
  val node_mut_o       = RegInit({val bundle = Wire(new node_mut_t)
                                      bundle.node_id  := 0.U           
                                      bundle.node_op  := 0.U           
                                      bundle.alloc    := 0.U            
                                      bundle.new_node_id :=0.U
                                      bundle})  

  val node_query_sent_q          = RegInit(0.U(1.W))
  val node_query_ready_i         = Reg(UInt(1.W))
  val node_query_resp_valid_i    = Reg(UInt(1.W)) 
  val node_query_resp_received_q = RegInit(0.U(1.W)) 
  val synchronous                = Reg(UInt(1.W))
  val node_query_resp_i          = Reg(new node_query_resp_t)

  val node_query_sent_d          = Reg(UInt(1.W))
  val node_query_valid_o         = Reg(UInt(1.W)) 
  val rd_result_o                = Reg(new cap_result_t )  
  val node_query_o               = RegInit(0.U.asTypeOf(new node_query_t ))     
  val node_query_resp_result_d   = RegInit(0.U(1.W))
  val node_query_resp_received_d = RegInit(0.U(1.W))
  val node_query_resp_result_q   = RegInit(0.U(1.W))
 ///////////////////////end node variable/////////// 

 //////////////////////Capstone_unit variable/////////////////////
  val node_alloc_node_id_q, node_alloc_node_id_d = RegInit(0.U(31.W)) //cap_renode_id_t

  node_alloc_node_id_cur := Mux((node_alloc_state_q === mut_t.NODE_ALLOC_RECEIVED ), node_alloc_node_id_q , CapExe_ins.node_alloc_node_id_i)

  val valid_last_q, valid_last_n, valid_cur = RegInit(0.U(1.W))
  val pc_last_q, pc_last_n, pc_cur = RegInit(0.U(VLEN.W))
  val fu_data_last_q, fu_data_last_n, fu_data_cur = RegInit(0.U.asTypeOf(new fu_data_t))

  valid_cur := valid_last_q | CapExe_ins.capstone_valid_i
  pc_cur := Mux(valid_last_q===1.U , pc_last_q , CapExe_ins.pc_i)
  // fu_data_cur := Mux(valid_last_q===1.U , fu_data_last_q , CapExe_ins.fu_data_i)
  fu_data_cur := Mux(valid_last_q===1.U , fu_data_last_q , src)

  CapExe_ins.capstone_ready_o := ~valid_last_q


  CapExe_ins.node_query_o.trans_id := fu_data_cur.trans_id

  CapExe_ins.capstone_trans_id_o := fu_data_cur.trans_id

  node_alloc_state_q := node_alloc_state_d
  node_query_sent_q := node_query_sent_d
  node_query_resp_received_q := node_query_resp_received_d
  node_query_resp_result_q := node_query_resp_result_d
  node_alloc_node_id_q := node_alloc_node_id_d
  valid_last_q := valid_last_n
  pc_last_q := pc_last_n
  fu_data_last_q := fu_data_last_n

  CapExe_ins.capstone_valid_o := valid_cur
 /////////////////////////////////////////////////////////////////////
  node_query_ready_i         := CapExe_ins.node_query_ready_i 
  node_query_resp_valid_i    := CapExe_ins.node_query_resp_valid_i 
  node_query_resp_i          := CapExe_ins.node_query_resp_i 

  node_alloc_resp_valid_i   := CapExe_ins.node_alloc_resp_valid_i
  node_alloc_ready_i        := CapExe_ins.node_alloc_ready_i
  node_alloc_node_id_i      := CapExe_ins.node_alloc_node_id_i
  
  
  //////////////////////////////////////////////////////////////////

   

  val crevokeModule = Module(new crevokeModule)
  crevokeModule.src := src
  val crevoke  = crevokeModule.crevoke
  //assign input mut
  crevokeModule.send_node_mut_ins.revnode_id       := revnode_id
  crevokeModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  crevokeModule.send_node_mut_ins.mut_ty           := mut_ty
  crevokeModule.send_node_mut_ins.have_alloc       := have_alloc 
  //assign input query
  crevokeModule.send_node_query_ins.revnode_id                 := revnode_id
  crevokeModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  crevokeModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  crevokeModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  crevokeModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  crevokeModule.send_node_query_ins.synchronous                := synchronous
  crevokeModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i
 
  val cshrinkModule = Module(new cshrinkModule)
  cshrinkModule.src := src
  val cshrink =  cshrinkModule.cshrink
 

  val ctightenModule = Module(new ctightenModule)
  ctightenModule.src :=  src
  val ctighten =  ctightenModule.ctighten  

  val cdelinvModule  = Module(new cdelinModule)
  cdelinvModule.src :=  src
  val cdelin =  cdelinvModule.cdelin  
  //assign input mut
  cdelinvModule.send_node_mut_ins.revnode_id       := revnode_id
  cdelinvModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  cdelinvModule.send_node_mut_ins.mut_ty           := mut_ty
  cdelinvModule.send_node_mut_ins.have_alloc       := have_alloc   

  val clccModule    = Module(new clccModule)
  clccModule.src :=  src
  val clcc =  clccModule.clcc  
  //assign input query
  clccModule.send_node_query_ins.revnode_id                 := revnode_id
  clccModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  clccModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  clccModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  clccModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  clccModule.send_node_query_ins.synchronous                := synchronous
  clccModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i 

  val csccModule    = Module(new csccModule)
  csccModule.src :=  src
  val cscc =  csccModule.cscc
  
  val csplitModule  = Module(new csplitModule)
  csplitModule.src :=  src
  val csplit =  csplitModule.csplit
  //assign input mut
  csplitModule.send_node_mut_ins.revnode_id       := revnode_id
  csplitModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  csplitModule.send_node_mut_ins.mut_ty           := mut_ty
  csplitModule.send_node_mut_ins.have_alloc       := have_alloc 
   //assign input query
  csplitModule.send_node_query_ins.revnode_id                 := revnode_id
  csplitModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  csplitModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  csplitModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  csplitModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  csplitModule.send_node_query_ins.synchronous                := synchronous
  csplitModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i
  //assign input alloc
  csplitModule.send_node_alloc_ins.node_alloc_ready_i      := node_alloc_ready_i
  csplitModule.send_node_alloc_ins.node_alloc_state_q      := node_alloc_state_q 
  csplitModule.send_node_alloc_ins.node_alloc_resp_valid_i := node_alloc_resp_valid_i
  csplitModule.send_node_alloc_ins.node_alloc_node_id_i    := node_alloc_node_id_i
  val csealModule   = Module(new csealModule)
  csealModule.src :=  src
  val cseal =  csealModule.cseal 

  val cmrevModule   = Module(new cmrevModule)
  cmrevModule.src :=  src
  val cmrev =  cmrevModule.cmrev 
  //assign input mut
  cmrevModule.send_node_mut_ins.revnode_id       := revnode_id
  cmrevModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  cmrevModule.send_node_mut_ins.mut_ty           := mut_ty
  cmrevModule.send_node_mut_ins.have_alloc       := have_alloc 
  //assign input query
  cmrevModule.send_node_query_ins.revnode_id                 := revnode_id
  cmrevModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  cmrevModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  cmrevModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  cmrevModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  cmrevModule.send_node_query_ins.synchronous                := synchronous
  cmrevModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i
  //assign input alloc
  cmrevModule.send_node_alloc_ins.node_alloc_ready_i      := node_alloc_ready_i
  cmrevModule.send_node_alloc_ins.node_alloc_state_q      := node_alloc_state_q 
  cmrevModule.send_node_alloc_ins.node_alloc_resp_valid_i := node_alloc_resp_valid_i
  cmrevModule.send_node_alloc_ins.node_alloc_node_id_i    := node_alloc_node_id_i

  val cinitModule   = Module(new cinitModule)
  cinitModule.src :=  src
  val cinit =  cinitModule.cinit 

  val cmovcModule   = Module(new cmovcModule)
  cmovcModule.src :=  src
  val cmovc =  cmovcModule.cmovc 

  val cdropModule   = Module(new cdropModule)
  cdropModule.src :=  src
  val cdrop =  cdropModule.cdrop 

  val ccincoffsetModule = Module(new ccincoffsetModule)
  ccincoffsetModule.src :=  src
  val ccincoffset =  ccincoffsetModule.ccincoffset

  val ccallModule   = Module(new ccallModule)
  ccallModule.src :=  src
  val ccall =  ccallModule.ccall   
  //assign input query
  ccallModule.send_node_query_ins.revnode_id                 := revnode_id
  ccallModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  ccallModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  ccallModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  ccallModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  ccallModule.send_node_query_ins.synchronous                := synchronous
  ccallModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i

  val creturnModule = Module(new creturnModule)
  creturnModule.src :=  src
  val creturn =  creturnModule.creturn 
  //assign input query
  creturnModule.send_node_query_ins.revnode_id                 := revnode_id
  creturnModule.send_node_query_ins.node_query_sent_q          := node_query_sent_q
  creturnModule.send_node_query_ins.node_query_ready_i         := node_query_ready_i 
  creturnModule.send_node_query_ins.node_query_resp_valid_i    := node_query_resp_valid_i 
  creturnModule.send_node_query_ins.node_query_resp_received_q := node_query_resp_received_q 
  creturnModule.send_node_query_ins.synchronous                := synchronous
  creturnModule.send_node_query_ins.node_query_resp_i          := node_query_resp_i
 
  val ccjalrModule  = Module(new ccjalrModule)
  ccjalrModule.src :=  src
  val ccjalr =  ccjalrModule.ccjalr   

  val ccbnzModule   = Module(new ccbnzModule)
  ccbnzModule.src :=  src
  val ccbnz =  ccbnzModule.ccbnz   

  val cccsrrwModule = Module(new cccsrrwModule)
  cccsrrwModule.src :=  src
  val cccsrrw =  cccsrrwModule.cccsrrw  

  val ccapenterModule = Module(new ccapenterModule)
  ccapenterModule.src :=  src
  val ccapenter =  ccapenterModule.ccapenter  
  //assign input mut
  ccapenterModule.send_node_mut_ins.revnode_id       := revnode_id
  ccapenterModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  ccapenterModule.send_node_mut_ins.mut_ty           := mut_ty
  ccapenterModule.send_node_mut_ins.have_alloc       := have_alloc 

  val ccincoffsetimModule = Module(new ccincoffsetimModule)
  ccincoffsetimModule.src :=  src
  val ccincoffsetim =  ccincoffsetimModule.ccincoffsetim

  val cshrinktoModule = Module(new cshrinktoModule)
  cshrinktoModule.src :=  src
  val cshrinkto =  cshrinktoModule.cshrinkto   

  val ccreateModule   = Module(new ccreateModule)
  ccreateModule.src :=  src
  val ccreate =  ccreateModule.ccreate   
  //assign input mut
  ccreateModule.send_node_mut_ins.revnode_id       := revnode_id
  ccreateModule.send_node_mut_ins.node_alloc_node_id_cur := node_alloc_node_id_cur
  ccreateModule.send_node_mut_ins.mut_ty           := mut_ty
  ccreateModule.send_node_mut_ins.have_alloc       := have_alloc 

  val ctypeModule     = Module(new ctypeModule)
  ctypeModule.src :=  src
  val ctype =  ctypeModule.ctype  

  val cnodeModule     = Module(new cnodeModule)
  cnodeModule.src :=  src
  val cnode =  cnodeModule.cnode

  val cpermModule     = Module(new cpermModule)
  cpermModule.src :=  src
  val cperm =  cpermModule.cperm 

  val cboundModule    = Module(new cboundModule)
  cboundModule.src :=  src
  val cbound =  cboundModule.cbound 

  val cprintModule    = Module(new cprintModule)
  cprintModule.src :=  src
  val cprint =  cprintModule.cprint 

  val cregprintModule = Module(new cregprintModule)
  cregprintModule.src := src
  val cregprint =  cregprintModule.cregprint 

  val getrandModule   = Module(new getrandModule)
  getrandModule.src :=  src
  val getrand =  getrandModule.getrand

  val tagsetModule  = Module(new tagsetModule)
  tagsetModule.src :=  src
  val tagset =  tagsetModule.tagset

  val taggetModule  = Module(new taggetModule)
  taggetModule.src :=  src
  val tagget =  taggetModule.tagget


  // Result capstone//mlabaf//capstone
  val capResSel = Module(new capResultSelect)
  capResSel.io.func := io.func(6, 0)
  capResSel.crevoke  := crevoke
  capResSel.cshrink  := cshrink
  capResSel.ctighten  := ctighten
  capResSel.cdelin  := cdelin
  capResSel.clcc  := clcc
  capResSel.cscc := cscc
  capResSel.csplit := csplit
  capResSel.cseal := cseal
  capResSel.cmrev := cmrev
  capResSel.cinit  := cinit
  capResSel.cmovc  := cmovc
  capResSel.cdrop  := cdrop
  capResSel.ccincoffset  := ccincoffset
  capResSel.ccall  := ccall
  capResSel.creturn := creturn
  capResSel.ccjalr := ccjalr
  capResSel.ccbnz := ccbnz
  capResSel.cccsrrw := cccsrrw
  capResSel.ccapenter  := ccapenter
  capResSel.ccincoffsetim  := ccincoffsetim
  capResSel.cshrinkto  := cshrinkto
  capResSel.ccreate := ccreate
  capResSel.ctype := ctype
  capResSel.cnode := cnode
  capResSel.cperm := cperm
  capResSel.cbound  := cbound
  capResSel.cprint  := cprint
  capResSel.cregprint := cregprint
  capResSel.getrand := getrand
  capResSel.tagset := tagset
  capResSel.tagget := tagget
  // val capRes = capResSel.io.capRes
   capRes := capResSel.capRes

 //select node result
  switch(io.func(6, 0))
  {
  is (0.U)
   {//crevoke
    CapExe_ins.ex_o.valid:=  crevokeModule.ex_o_v
    // crevokeModule.send_node_query_ins  <>exe_node_query_ins
    // crevokeModule.send_node_mut_ins    <>exe_node_mut_ins
    //output from mut
    node_mut_valid_o        := crevokeModule.send_node_mut_ins.node_mut_valid_o
    node_mut_o.node_id      := crevokeModule.send_node_mut_ins.node_mut_o.node_id          
    node_mut_o.node_op      := crevokeModule.send_node_mut_ins.node_mut_o.node_op          
    node_mut_o.alloc        := crevokeModule.send_node_mut_ins.node_mut_o.alloc         
    node_mut_o.new_node_id  := crevokeModule.send_node_mut_ins.node_mut_o.new_node_id
    //output from query    
    node_query_sent_d          := crevokeModule.send_node_query_ins.node_query_sent_d
    node_query_valid_o         := crevokeModule.send_node_query_ins.node_query_valid_o
    rd_result_o                := crevokeModule.send_node_query_ins.rd_result_o
    node_query_o               := crevokeModule.send_node_query_ins.node_query_o    
    node_query_resp_result_d   := crevokeModule.send_node_query_ins.node_query_resp_result_d
    node_query_resp_received_d := crevokeModule.send_node_query_ins.node_query_resp_received_d
    node_query_resp_result_q   := crevokeModule.send_node_query_ins.node_query_resp_result_q
 

    
    printf( "crevokeModule.send_node_mut_ins.node_mut_o %b\n",crevokeModule.send_node_mut_ins.node_mut_o.node_op)
    
  }
  is (1.U){//cshrink
   CapExe_ins.ex_o.valid:=cshrinkModule.ex_o_v 

  
  }
  is (2.U){//ctighten
   CapExe_ins.ex_o.valid:=ctightenModule.ex_o_v 

  }
  is (3.U){//cdelin
  //  cdelinvModule.send_node_mut_ins    <>exe_node_mut_ins
   CapExe_ins.ex_o.valid:=cdelinvModule.ex_o_v 
   //output from mut
   node_mut_valid_o        := cdelinvModule.send_node_mut_ins.node_mut_valid_o
   node_mut_o.node_id      := cdelinvModule.send_node_mut_ins.node_mut_o.node_id          
   node_mut_o.node_op      := cdelinvModule.send_node_mut_ins.node_mut_o.node_op          
   node_mut_o.alloc        := cdelinvModule.send_node_mut_ins.node_mut_o.alloc         
   node_mut_o.new_node_id  := cdelinvModule.send_node_mut_ins.node_mut_o.new_node_id

   printf( "cdelinModule.send_node_mut_ins.node_mut_o %b\n",cdelinvModule.send_node_mut_ins.node_mut_o.node_op)

  }    
  is (4.U){//clcc
  //  clccModule.send_node_query_ins  <>exe_node_query_ins
   CapExe_ins.ex_o.valid:=clccModule.ex_o_v 

    //output from query    
    node_query_sent_d          := clccModule.send_node_query_ins.node_query_sent_d
    node_query_valid_o         := clccModule.send_node_query_ins.node_query_valid_o
    rd_result_o                := clccModule.send_node_query_ins.rd_result_o
    node_query_o               := clccModule.send_node_query_ins.node_query_o    
    node_query_resp_result_d   := clccModule.send_node_query_ins.node_query_resp_result_d
    node_query_resp_received_d := clccModule.send_node_query_ins.node_query_resp_received_d
    node_query_resp_result_q   := clccModule.send_node_query_ins.node_query_resp_result_q  
  }

  is (5.U){//cscc
   CapExe_ins.ex_o.valid:=csccModule.ex_o_v 

  }
  is (6.U){//csplit
   CapExe_ins.ex_o.valid:=csplitModule.ex_o_v 
  //   csplitModule.send_node_query_ins  <>exe_node_query_ins
  //   csplitModule.send_node_mut_ins    <>exe_node_mut_ins
  //   csplitModule.send_node_alloc_ins  <>exe_node_alloc_ins   
  //output from mut 
   node_mut_valid_o        := csplitModule.send_node_mut_ins.node_mut_valid_o
   node_mut_o.node_id      := csplitModule.send_node_mut_ins.node_mut_o.node_id          
   node_mut_o.node_op      := csplitModule.send_node_mut_ins.node_mut_o.node_op          
   node_mut_o.alloc        := csplitModule.send_node_mut_ins.node_mut_o.alloc         
   node_mut_o.new_node_id  := csplitModule.send_node_mut_ins.node_mut_o.new_node_id    
    //output from alloc
    node_alloc_state_d := csplitModule.send_node_alloc_ins.node_alloc_state_d   
    node_alloc_valid_o := csplitModule.send_node_alloc_ins.node_alloc_valid_o
    node_alloc_node_id_d := csplitModule.send_node_alloc_ins.node_alloc_node_id_d

    //output from query    
    node_query_sent_d          := csplitModule.send_node_query_ins.node_query_sent_d
    node_query_valid_o         := csplitModule.send_node_query_ins.node_query_valid_o
    rd_result_o                := csplitModule.send_node_query_ins.rd_result_o
    node_query_o               := csplitModule.send_node_query_ins.node_query_o    
    node_query_resp_result_d   := csplitModule.send_node_query_ins.node_query_resp_result_d
    node_query_resp_received_d := csplitModule.send_node_query_ins.node_query_resp_received_d
    node_query_resp_result_q   := csplitModule.send_node_query_ins.node_query_resp_result_q 

  }
  is (7.U){//cseal
    CapExe_ins.ex_o.valid:=csealModule.ex_o_v 
  }
  is (8.U){//cmrev
    CapExe_ins.ex_o.valid:=cmrevModule.ex_o_v 
  //   cmrevModule.send_node_query_ins  <>exe_node_query_ins
  //   cmrevModule.send_node_mut_ins    <>exe_node_mut_ins
  //   cmrevModule.send_node_alloc_ins  <>exe_node_alloc_ins    
  //output from mut
   node_mut_valid_o        := cmrevModule.send_node_mut_ins.node_mut_valid_o
   node_mut_o.node_id      := cmrevModule.send_node_mut_ins.node_mut_o.node_id          
   node_mut_o.node_op      := cmrevModule.send_node_mut_ins.node_mut_o.node_op          
   node_mut_o.alloc        := cmrevModule.send_node_mut_ins.node_mut_o.alloc         
   node_mut_o.new_node_id  := cmrevModule.send_node_mut_ins.node_mut_o.new_node_id 
  //output from alloc
   node_alloc_state_d := cmrevModule.send_node_alloc_ins.node_alloc_state_d   
   node_alloc_valid_o := cmrevModule.send_node_alloc_ins.node_alloc_valid_o
   node_alloc_node_id_d := cmrevModule.send_node_alloc_ins.node_alloc_node_id_d
   
    //output from query    
    node_query_sent_d          := cmrevModule.send_node_query_ins.node_query_sent_d
    node_query_valid_o         := cmrevModule.send_node_query_ins.node_query_valid_o
    rd_result_o                := cmrevModule.send_node_query_ins.rd_result_o
    node_query_o               := cmrevModule.send_node_query_ins.node_query_o    
    node_query_resp_result_d   := cmrevModule.send_node_query_ins.node_query_resp_result_d
    node_query_resp_received_d := cmrevModule.send_node_query_ins.node_query_resp_received_d
    node_query_resp_result_q   := cmrevModule.send_node_query_ins.node_query_resp_result_q

   

  }
  is (9.U){//cinit
   CapExe_ins.ex_o.valid:=cinitModule.ex_o_v 
  }
  is (10.U){//cmovc
   CapExe_ins.ex_o.valid:=cmovcModule.ex_o_v 
  }
  is (11.U){//cdrop
   CapExe_ins.ex_o.valid:=cdropModule.ex_o_v 
  }    
  is (12.U){//ccincoffset
   CapExe_ins.ex_o.valid:=ccincoffsetModule.ex_o_v 

  }
  is (13.U){//ccall
  //  ccallModule.send_node_query_ins  <>exe_node_query_ins
   CapExe_ins.ex_o.valid := ccallModule.ex_o_v
    //output from query    
    node_query_sent_d          := ccallModule.send_node_query_ins.node_query_sent_d
    node_query_valid_o         := ccallModule.send_node_query_ins.node_query_valid_o
    rd_result_o                := ccallModule.send_node_query_ins.rd_result_o
    node_query_o               := ccallModule.send_node_query_ins.node_query_o    
    node_query_resp_result_d   := ccallModule.send_node_query_ins.node_query_resp_result_d
    node_query_resp_received_d := ccallModule.send_node_query_ins.node_query_resp_received_d
    node_query_resp_result_q   := ccallModule.send_node_query_ins.node_query_resp_result_q


  }    
  is (14.U){//creturn
  //  creturnModule.send_node_query_ins  <>exe_node_query_ins
   CapExe_ins.ex_o.valid := creturnModule.ex_o_v
   //output from query    
   node_query_sent_d          := creturnModule.send_node_query_ins.node_query_sent_d
   node_query_valid_o         := creturnModule.send_node_query_ins.node_query_valid_o
   rd_result_o                := creturnModule.send_node_query_ins.rd_result_o
   node_query_o               := creturnModule.send_node_query_ins.node_query_o    
   node_query_resp_result_d   := creturnModule.send_node_query_ins.node_query_resp_result_d
   node_query_resp_received_d := creturnModule.send_node_query_ins.node_query_resp_received_d
   node_query_resp_result_q   := creturnModule.send_node_query_ins.node_query_resp_result_q

  }  
  is (18.U){//capenter  
   
  //  ccapenterModule.send_node_mut_ins  <>exe_node_mut_ins
   //output from mut  
   node_mut_valid_o        := ccapenterModule.send_node_mut_ins.node_mut_valid_o
   node_mut_o.node_id      := ccapenterModule.send_node_mut_ins.node_mut_o.node_id          
   node_mut_o.node_op      := ccapenterModule.send_node_mut_ins.node_mut_o.node_op          
   node_mut_o.alloc        := ccapenterModule.send_node_mut_ins.node_mut_o.alloc         
   node_mut_o.new_node_id  := ccapenterModule.send_node_mut_ins.node_mut_o.new_node_id 
  }
  is (19.U){//ccincoffsetim
    CapExe_ins.ex_o.valid := ccincoffsetimModule.ex_o_v  
  } 
  is (21.U){//ccreate
  
  //  ccreateModule.send_node_mut_ins  <>exe_node_mut_ins
  //output from mut  
   node_mut_valid_o        := ccreateModule.send_node_mut_ins.node_mut_valid_o
   node_mut_o.node_id      := ccreateModule.send_node_mut_ins.node_mut_o.node_id          
   node_mut_o.node_op      := ccreateModule.send_node_mut_ins.node_mut_o.node_op          
   node_mut_o.alloc        := ccreateModule.send_node_mut_ins.node_mut_o.alloc         
   node_mut_o.new_node_id  := ccreateModule.send_node_mut_ins.node_mut_o.new_node_id 

  }
  }
  //assign selected output mut/query/alloc accoring to muk_node_unit output 
   // query 
    CapExe_ins.node_query_valid_o         := node_query_valid_o 
    CapExe_ins.node_query_o               := node_query_o   
    //allocation
    
    CapExe_ins.node_alloc_valid_o := node_alloc_valid_o    
    // mutation
    CapExe_ins.node_mut_valid_o           := node_mut_valid_o    
    CapExe_ins.node_mut_o.node_id         := node_mut_o.node_id         
    CapExe_ins.node_mut_o.node_op         := node_mut_o.node_op         
    CapExe_ins.node_mut_o.alloc           := node_mut_o.alloc         
    CapExe_ins.node_mut_o.new_node_id     := node_mut_o.new_node_id         
    printf( " f node_mut_valid_o %b\n",node_mut_valid_o)   
    printf( " f CapExe_ins.node_mut_valid_o  %b\n",CapExe_ins.node_mut_valid_o  )

    printf( " f node_mut_o.node_op  %b\n",node_mut_o.node_op)
    printf( " f CapExe_ins.node_mut_o.node_op %b\n",CapExe_ins.node_mut_o.node_op)

}


class AluDataModule (implicit p: Parameters) extends XSModule {
   val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(XLEN.W)))
    val func = Input(FuOpType())
    val pred_taken, isBranch = Input(Bool())
    val result = Output(UInt(XLEN.W))
    val taken, mispredict = Output(Bool())
   })   


  val (src1, src2, func) = (io.src(0), io.src(1), io.func)

  val shamt = src2(5, 0)
  val revShamt = ~src2(5,0) + 1.U

  // slliuw, sll
  val leftShiftModule = Module(new LeftShiftModule)
  val sll = leftShiftModule.io.sll
  val revSll = leftShiftModule.io.revSll
  leftShiftModule.io.sllSrc := Cat(Fill(32, func(0)), Fill(32, 1.U)) & src1
  leftShiftModule.io.shamt := shamt
  leftShiftModule.io.revShamt := revShamt

  // bclr, bset, binv
  val bitShift = 1.U << src2(5, 0)
  val bclr = src1 & ~bitShift
  val bset = src1 | bitShift
  val binv = src1 ^ bitShift

  // srl, sra, bext
  val rightShiftModule = Module(new RightShiftModule)
  val srl = rightShiftModule.io.srl
  val revSrl = rightShiftModule.io.revSrl
  val sra = rightShiftModule.io.sra
  rightShiftModule.io.shamt := shamt
  rightShiftModule.io.revShamt := revShamt
  rightShiftModule.io.srlSrc := src1
  rightShiftModule.io.sraSrc := src1
  val bext = srl(0)

  val rol = revSrl | sll
  val ror = srl | revSll

  // addw
  val addModule = Module(new AddModule)
  addModule.io.srcw := Mux(!func(2) && func(0), ZeroExt(src1(0), XLEN), src1(31, 0))
  val addwResultAll = VecInit(Seq(
    ZeroExt(addModule.io.addw(0), XLEN),
    ZeroExt(addModule.io.addw(7, 0), XLEN),
    ZeroExt(addModule.io.addw(15, 0), XLEN),
    SignExt(addModule.io.addw(15, 0), XLEN)
  ))
  val addw = Mux(func(2), addwResultAll(func(1, 0)), addModule.io.addw)

  // subw
  val subModule = Module(new SubModule)
  val subw = subModule.io.sub

  // sllw
  val leftShiftWordModule = Module(new LeftShiftWordModule)
  val sllw = leftShiftWordModule.io.sllw
  val revSllw = leftShiftWordModule.io.revSllw
  leftShiftWordModule.io.sllSrc := src1
  leftShiftWordModule.io.shamt := shamt
  leftShiftWordModule.io.revShamt := revShamt

  val rightShiftWordModule = Module(new RightShiftWordModule)
  val srlw = rightShiftWordModule.io.srlw
  val revSrlw = rightShiftWordModule.io.revSrlw
  val sraw = rightShiftWordModule.io.sraw
  rightShiftWordModule.io.shamt := shamt
  rightShiftWordModule.io.revShamt := revShamt
  rightShiftWordModule.io.srlSrc := src1
  rightShiftWordModule.io.sraSrc := src1

  val rolw = revSrlw | sllw
  val rorw = srlw | revSllw

  // add
  val wordMaskAddSource = Cat(Fill(32, func(0)), Fill(32, 1.U)) & src1
  val shaddSource = VecInit(Seq(
    Cat(wordMaskAddSource(62, 0), 0.U(1.W)),
    Cat(wordMaskAddSource(61, 0), 0.U(2.W)),
    Cat(wordMaskAddSource(60, 0), 0.U(3.W)),
    Cat(wordMaskAddSource(59, 0), 0.U(4.W))
  ))
  val sraddSource = VecInit(Seq(
    ZeroExt(src1(63, 29), XLEN),
    ZeroExt(src1(63, 30), XLEN),
    ZeroExt(src1(63, 31), XLEN),
    ZeroExt(src1(63, 32), XLEN)
  ))
  // TODO: use decoder or other libraries to optimize timing
  // Now we assume shadd has the worst timing.
  addModule.io.src(0) := Mux(func(3), shaddSource(func(2, 1)),
    Mux(func(2), sraddSource(func(1, 0)),
    Mux(func(1), ZeroExt(src1(0), XLEN), wordMaskAddSource))
  )
  addModule.io.src(1) := src2
  val add = addModule.io.add

  // sub
  val sub  = subModule.io.sub
  subModule.io.src(0) := src1
  subModule.io.src(1) := src2
  val sltu    = !sub(XLEN)
  val slt     = src1(XLEN - 1) ^ src2(XLEN - 1) ^ sltu
  val maxMin  = Mux(slt ^ func(0), src2, src1)
  val maxMinU = Mux(sltu ^ func(0), src2, src1)
  val compareRes = Mux(func(2), Mux(func(1), maxMin, maxMinU), Mux(func(1), slt, Mux(func(0), sltu, sub)))

  // logic
  val logicSrc2 = Mux(!func(5) && func(0), ~src2, src2)
  val and     = src1 & logicSrc2
  val or      = src1 | logicSrc2
  val xor     = src1 ^ logicSrc2
  val orcb    = Cat((0 until 8).map(i => Fill(8, src1(i * 8 + 7, i * 8).orR)).reverse)
  val orh48   = Cat(src1(63, 8), 0.U(8.W)) | src2

  val sextb = SignExt(src1(7, 0), XLEN)
  val packh = Cat(src2(7,0), src1(7,0))
  val sexth = SignExt(src1(15, 0), XLEN)
  val packw = SignExt(Cat(src2(15, 0), src1(15, 0)), XLEN)

  val revb = Cat((0 until 8).map(i => Reverse(src1(8 * i + 7, 8 * i))).reverse)
  val pack = Cat(src2(31, 0), src1(31, 0))
  val rev8 = Cat((0 until 8).map(i => src1(8 * i + 7, 8 * i)))

  // branch
  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xor.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)


  // Result Select
  val shiftResSel = Module(new ShiftResultSelect)
  shiftResSel.io.func := func(3, 0)
  shiftResSel.io.sll  := sll
  shiftResSel.io.srl  := srl
  shiftResSel.io.sra  := sra
  shiftResSel.io.rol  := rol
  shiftResSel.io.ror  := ror
  shiftResSel.io.bclr := bclr
  shiftResSel.io.binv := binv
  shiftResSel.io.bset := bset
  shiftResSel.io.bext := bext
  val shiftRes = shiftResSel.io.shiftRes

  val miscResSel = Module(new MiscResultSelect)
  miscResSel.io.func    := func(5, 0)
  miscResSel.io.and     := and
  miscResSel.io.or      := or
  miscResSel.io.xor     := xor
  miscResSel.io.orcb    := orcb
  miscResSel.io.orh48   := orh48
  miscResSel.io.sextb   := sextb
  miscResSel.io.packh   := packh
  miscResSel.io.sexth   := sexth
  miscResSel.io.packw   := packw
  miscResSel.io.revb    := revb
  miscResSel.io.rev8    := rev8
  miscResSel.io.pack    := pack
  miscResSel.io.src     := src1
  val miscRes = miscResSel.io.miscRes

  val wordResSel = Module(new WordResultSelect)
  wordResSel.io.func := func
  wordResSel.io.addw := addw
  wordResSel.io.subw := subw
  wordResSel.io.sllw := sllw
  wordResSel.io.srlw := srlw
  wordResSel.io.sraw := sraw
  wordResSel.io.rolw := rolw
  wordResSel.io.rorw := rorw
  val wordRes = wordResSel.io.wordRes

  val aluResSel = Module(new AluResSel)
  // aluResSel.io.func := func(7, 4)
  aluResSel.io.func := func(6, 4)
  aluResSel.io.addRes := add
  aluResSel.io.compareRes := compareRes
  aluResSel.io.shiftRes := shiftRes
  aluResSel.io.miscRes := miscRes
  aluResSel.io.wordRes := wordRes
 // aluResSel.io.capRes := capRes//mlabaf//capstone
  val aluRes = aluResSel.io.aluRes

  io.result := aluRes
  io.taken := taken
  io.mispredict := (io.pred_taken ^ taken) && io.isBranch

}


class Alu(implicit p: Parameters) extends FUWithRedirect {

  val uop = io.in.bits.uop

  val isBranch = ALUOpType.isBranch(io.in.bits.uop.ctrl.fuOpType)
  val dataModule = Module(new AluDataModule)
  dataModule.io.src  := io.in.bits.src.take(2)
  dataModule.io.func := io.in.bits.uop.ctrl.fuOpType
  dataModule.io.pred_taken := uop.cf.pred_taken
  dataModule.io.isBranch   := isBranch
  printf("dataModule.io.src: %b,%b\n", dataModule.io.src(0), dataModule.io.src(1))


  val CapEModule = Module(new CapExeModule)
  //CapEModule.io.src := io.in.bits.src.take(2)
  //CapEModule.src := io.in.bits.CapSrc
  CapEModule.io.func      := io.in.bits.uop.ctrl.fuOpType
  CapEModule.io.operand_a := io.in.bits.operand_a 
  CapEModule.io.operand_b := io.in.bits.operand_b
  CapEModule.io.imm       := io.in.bits.imm
  CapEModule.io.cap_a     := io.in.bits.cap_a 
  CapEModule.io.cap_b     := io.in.bits.cap_b 
  CapEModule.io.cap_c     := io.in.bits.cap_c 
  CapEModule.io.tag_a     := io.in.bits.tag_a
  CapEModule.io.tag_b     := io.in.bits.tag_b
  CapEModule.io.tag_c     := io.in.bits.tag_c
  CapEModule.io.valid     := io.in.bits.valid
  CapEModule.io.rd        := io.in.bits.rd
  CapEModule.io.rs1       := io.in.bits.rs1
  CapEModule.io.rs2       := io.in.bits.rs2
  CapEModule.io.trans_id  := io.in.bits.trans_id
  ////////////////////////////capability control signal ////////////////////
  CapEModule.CapExe_ins.flush_i                 := io.in.bits.flush_i//controller
  CapEModule.CapExe_ins.pc_i                    := io.in.bits.pc_i //issue_stage// PC of instruction
  CapEModule.CapExe_ins.capstone_valid_i        := io.in.bits.capstone_valid_i//issue_stage
  CapEModule.CapExe_ins.cih_i                   := io.in.bits.cih_i //csr      

  io.out.bits.ex_o                := CapEModule.CapExe_ins.ex_o                 
  io.out.bits.cms_result_o        := CapEModule.CapExe_ins.cms_result_o 
  io.out.bits.capstone_valid_o    := CapEModule.CapExe_ins.capstone_valid_o      
  io.out.bits.capstone_ready_o    := CapEModule.CapExe_ins.capstone_ready_o                           
  io.out.bits.capstone_trans_id_o := CapEModule.CapExe_ins.capstone_trans_id_o   
  io.out.bits.dom_switch_valid_o  := CapEModule.CapExe_ins.dom_switch_valid_o                      
  io.out.bits.dom_switch_req_o    := CapEModule.CapExe_ins.dom_switch_req_o                 
  
  // scoreboard
  io.out.bits.node_mut_valid_o    := CapEModule.CapExe_ins.node_mut_valid_o         
  io.out.bits.node_mut_o.node_id  := CapEModule.CapExe_ins.node_mut_o.node_id            
  io.out.bits.node_mut_o.node_op  := CapEModule.CapExe_ins.node_mut_o.node_op            
  io.out.bits.node_mut_o.alloc    := CapEModule.CapExe_ins.node_mut_o.alloc            
  io.out.bits.node_mut_o.new_node_id := CapEModule.CapExe_ins.node_mut_o.new_node_id            

 /////////////////////////////////mock_node_unit//////////////////////// 
  val mock_node_unit_ins = Module(new mock_node_unit)

  mock_node_unit_ins.io.flush_i := io.in.bits.flush_i
 //temporarily//from load and store unit
   mock_node_unit_ins.io.query_valid_i(0) := 0.U
   mock_node_unit_ins.io.query_i(0).synchronous    := 0.U
   mock_node_unit_ins.io.query_i(0).trans_id       := 0.U
   mock_node_unit_ins.io.query_i(0).node_id        := 0.U
   mock_node_unit_ins.io.query_valid_i(1) := 0.U
   mock_node_unit_ins.io.query_i(1).synchronous    := 0.U
   mock_node_unit_ins.io.query_i(1).trans_id       := 0.U
   mock_node_unit_ins.io.query_i(1).node_id        := 0.U     
  // query interface (asynchronous)
   mock_node_unit_ins.io.query_valid_i(2) := CapEModule.CapExe_ins.node_query_valid_o
   mock_node_unit_ins.io.query_i(2)       <> CapEModule.CapExe_ins.node_query_o//node_query_t
   // allocation interface
   mock_node_unit_ins.io.alloc_valid_i := CapEModule.CapExe_ins.node_alloc_valid_o 
   // mutation interface
   mock_node_unit_ins.io.mut_valid_i   := CapEModule.CapExe_ins.node_mut_valid_o//1.U//commit_stage
   mock_node_unit_ins.io.mut_i.node_id       := CapEModule.CapExe_ins.node_mut_o.node_id //0.U//commit_stage
   mock_node_unit_ins.io.mut_i.node_op       := CapEModule.CapExe_ins.node_mut_o.node_op//0.U//commit_stage
   mock_node_unit_ins.io.mut_i.alloc         := CapEModule.CapExe_ins.node_mut_o.alloc//0.U//commit_stage
   mock_node_unit_ins.io.mut_i.new_node_id   := CapEModule.CapExe_ins.node_mut_o.new_node_id //0.U//commit_stage
   printf("mock_node_unit_ins.io.mut_i.node_id: %b \n:",mock_node_unit_ins.io.mut_i.node_id  )
   printf("mock_node_unit_ins.io.mut_i.node_op: %b \n:",mock_node_unit_ins.io.mut_i.node_op  )
   printf("mock_node_unit_ins.io.mut_i..mut_valid_i: %b \n:",mock_node_unit_ins.io.mut_valid_i  )


   // stalling (from scoreboard) 
   mock_node_unit_ins.io.query_stall_i := 0.U//scoreboard


   CapEModule.CapExe_ins.node_query_ready_i      := mock_node_unit_ins.io.query_ready_o(2)
   CapEModule.CapExe_ins.node_query_resp_i       := mock_node_unit_ins.io.query_resp_o(2) 
   CapEModule.CapExe_ins.node_query_resp_valid_i := mock_node_unit_ins.io.query_valid_o(2) 
   CapEModule.CapExe_ins.node_alloc_node_id_i    := mock_node_unit_ins.io.alloc_node_id_o
   CapEModule.CapExe_ins.node_alloc_ready_i      := mock_node_unit_ins.io.alloc_ready_o
   CapEModule.CapExe_ins.node_alloc_resp_valid_i := mock_node_unit_ins.io.alloc_valid_o

 //////////////////////////////////////////////ALU_Capability_selector//////////////////////////////////////


  val ResSel        = Module(new AluCapResSel)
  ResSel.io.func   := io.in.bits.uop.ctrl.fuOpType
  ResSel.io.capRes := CapEModule.capRes
  ResSel.io.AluRes := dataModule.io.result

  redirectOutValid := io.out.valid && isBranch
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.robIdx := uop.robIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.isMisPred := dataModule.io.mispredict
  redirectOut.cfiUpdate.taken := dataModule.io.taken
  redirectOut.cfiUpdate.predTaken := uop.cf.pred_taken

  io.in.ready  := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop <> io.in.bits.uop
 //  io.out.bits.data := dataModule.io.result
  io.out.bits.data := ResSel.io.ExRes
  io.out.bits.tag  := ResSel.io.ExResTag
  io.out.bits.valid:= ResSel.io.ExResValid

 ////////////////////////////////////////////for test// cpmpression and uncompression function///////////////////////////


 when(dataModule.io.func===128.U)
 {
   printf("instruction code in alu is: crevoke")
 }
 .elsewhen(dataModule.io.func===129.U)
  { 
   printf("instruction code in alu is: shrink")
  }
 .elsewhen(dataModule.io.func===130.U)
   {
     printf("instruction code in alu is: tighten")
   }
 .elsewhen(dataModule.io.func===131.U)
   {
     printf("instruction code in alu is: delin")
   }
 .elsewhen(dataModule.io.func===132.U)
  { 
   printf("instruction code in alu is: lcc")
  }
 .elsewhen(dataModule.io.func===133.U)
   {
     printf("instruction code in alu is: scc")
   }
 .elsewhen(dataModule.io.func===134.U)
   {
     printf("instruction code in alu is: split")
   }
 .elsewhen(dataModule.io.func===135.U)
  { 
   printf("instruction code in alu is: seal")
  }
 .elsewhen(dataModule.io.func===136.U)
   {
     printf("instruction code in alu is: mrev")
   }
 .elsewhen(dataModule.io.func===137.U)
   {
     printf("instruction code in alu is: cinit")
   }
 .elsewhen(dataModule.io.func===138.U)
  { 
   printf("instruction code in alu is: mov")
  }
 .elsewhen(dataModule.io.func===139.U)
   {
     printf("instruction code in alu is: drop")
   }
 .elsewhen(dataModule.io.func===140.U)
   {
     printf("instruction code in alu is: ccincoffset")
   }
 .elsewhen(dataModule.io.func===141.U)
  { 
   printf("instruction code in alu is: call")
  }
 .elsewhen(dataModule.io.func===142.U)
   {
     printf("instruction code in alu is: return")
   }
 .elsewhen(dataModule.io.func===143.U)
   {
     printf("instruction code in alu is: ccjalr")
   }
 .elsewhen(dataModule.io.func===144.U)
  { 
   printf("instruction code in alu is: bnz")
  }
 .elsewhen(dataModule.io.func===145.U)
   {
     printf("instruction code in alu is: cccsrrw")
   }
 .elsewhen(dataModule.io.func===146.U)
   {
     printf("instruction code in alu is: ccapenter")
   }    
 .elsewhen(dataModule.io.func===147.U)
  { 
   printf("instruction code in alu is: ccincoffsetim")
  }
 .elsewhen(dataModule.io.func===148.U)
   {
     printf("instruction code in alu is: cshrinkto")
   }
 
 
   // switch(dataModule.io.func)
   // {
   //   is (64.U){ printf("instruction code in alu is: and")}
   //   is (64.U){ printf("instruction code in alu is: add")}
   //   is (128.U){ printf("instruction code in alu is: crevoke")}
   //   is (129.U){ printf("instruction code in alu is: cshrink")}
   //   is (130.U){ printf("instruction code in alu is: ctighten")}
   //   is (131.U){ printf("instruction code in alu is: cdelin")}
   //   is (132.U){ printf("instruction code in alu is: clcc")}
   //   is (133.U){ printf("instruction code in alu is: cscc")}
   //   is (134.U){ printf("instruction code in alu is: csplit")}
   //   is (135.U){ printf("instruction code in alu is: cseal")}
   //   is (136.U){ printf("instruction code in alu is: cmrev")}
   //   is (137.U){ printf("instruction code in alu is: cinit")}
   //   is (138.U){ printf("instruction code in alu is: cmovc")}
   //   is (139.U){ printf("instruction code in alu is: cdrop")}        
   //   is (140.U){ printf("instruction code in alu is: ccincoffset")}    
   //   is (141.U){ printf("instruction code in alu is: ccall")}    
   //   is (142.U){ printf("instruction code in alu is: creturn")}    
   //   is (143.U){ printf("instruction code in alu is: ccjalr")}    
   //   is (144.U){ printf("instruction code in alu is: ccbnz")}    
   //   is (145.U){ printf("instruction code in alu is: cccsrrw")}    
   //   is (146.U){ printf("instruction code in alu is: ccapenter")}
   //   is (147.U){ printf("instruction code in alu is: ccincoffsetim")}    
   //   is (148.U){ printf("instruction code in alu is: cshrinkto")}    
   //   is (149.U){ printf("instruction code in alu is: ccreate")}      
   //   is (150.U){ printf("instruction code in alu is: ctype")}      
   //   is (151.U){ printf("instruction code in alu is: cnode")}    
   //   is (152.U){ printf("instruction code in alu is: cperm")}    
   //   is (153.U){ printf("instruction code in alu is: cbound")}    
   //   is (154.U){ printf("instruction code in alu is: cprint")}    
   //   is (155.U){ printf("instruction code in alu is: cregprint")}    
   //   is (156.U){ printf("instruction code in alu is: getrand")}    
   //   is (157.U){ printf("instruction code in alu is: tagset")}    
   //   is (158.U){ printf("instruction code in alu is: tagget")}    
   //  // is (159.U){ printf("instruction code in alu is: sdd")}    
       
   // }
  //  XSDebug("instruction in alu class1=%b\n",dataModule.io.func)
  //  XSDebug("instruction in alu class2=%b\n",io.in.bits.uop.ctrl.fuOpType)
 
   //for test
   CapEModule.io.operand_a := 100230190.U//previous base 100230256.U //shrink
   CapEModule.io.operand_b := 100230302.U//previous top 100230328//shrink
   CapEModule.io.imm       :=2.U 
   CapEModule.io.cap_b     := Cat(0.U(93.W), 0.U(3.W) ,1023.U(31.W))
   //  CapEModule.io.cap_a     := Cat(31.U(93.W), 2.U(3.W) ,1023.U(31.W))//revoke
   //  CapEModule.io.cap_a     := Cat(31.U(93.W), 4.U(3.W) ,1023.U(31.W))//clcc
    CapEModule.io.cap_a     := Cat(31.U(90.W), 1.U(3.W), 1.U(3.W) ,2103.U(31.W))//scc/tighten/split/mov/coffset
   //  CapEModule.io.cap_a     := Cat(31.U(90.W), 1.U(3.W), 0.U(3.W) ,2103.U(31.W))//scc/tighten/split
    // CapEModule.io.cap_c     := Cat(31.U(93.W), 0.U(3.W) ,12.U(31.W))//mrev
   //  CapEModule.io.cap_c     := Cat(cap_compress_cap.cap_cc.bounds.cursor, cap_compress_cap.cap_cc.bounds.bE,cap_compress_cap.cap_cc.bounds.b,cap_compress_cap.cap_cc.bounds.tE,cap_compress_cap.cap_cc.bounds.t,cap_compress_cap.cap_cc.bounds.iE,0.U(3.W), cap_type_t.CAP_TYPE_LINEAR ,0.U(31.W))//for delin
 
   // CapEModule.io.tag_a     := 0.U//delin//revoke/seal
   CapEModule.io.tag_a     := 1.U//scc/tighten/split/drop/cinit/mrev
   CapEModule.io.tag_b     := 0.U
   CapEModule.io.tag_c     := 1.U//shrink
 
   CapEModule.io.rs1    := 1198.U//mov
   CapEModule.io.rd     := 1345.U//mov
 
    val rs1_cc = Reg(new cap_cc_t)
    val rs1_c  = Reg(new cap_fat_t) 
    val cap_uncompress_cap = Module(new cap_uncompress)
    val cap_compress_cap   = Module(new cap_compress)
    val rs2_c  = Reg(new cap_fat_t) 
    rs1_c.bounds.cursor:=100230256.U//100231256.U//807661056.U//"h00df000000121400".U//1400.U//
    rs1_c.bounds.base  :=100230132.U//100230132.U//807660032.U//"h00df000000121000".U//1000.U//
    rs1_c.bounds.top   :=100230328.U//100246328.U//100234228.U//807664128.U//"h00df000000122000".U//2000.U//
            
    rs1_c.reg_id:= 3.U
    rs1_c.async:= 1.U
    rs1_c.padding:=0.U
    rs1_c.meta.ty:= cap_type_t.CAP_TYPE_REVOKE
    rs1_c.meta.perm:= cap_perm_t.CAP_PERM_RWX
 
    rs1_c.renode_id:=12.U
 
    cap_compress_cap.cap_fat.bounds.cursor:=rs1_c.bounds.cursor
    cap_compress_cap.cap_fat.bounds.base  :=rs1_c.bounds.base
    cap_compress_cap.cap_fat.bounds.top   :=rs1_c.bounds.top
    cap_compress_cap.cap_fat.renode_id   :=rs1_c.renode_id
    cap_compress_cap.cap_fat.reg_id:=rs1_c.reg_id
    cap_compress_cap.cap_fat.async:=rs1_c.async
    cap_compress_cap.cap_fat.padding:=rs1_c.padding
    cap_compress_cap.cap_fat.meta.ty:=rs1_c.meta.ty
    cap_compress_cap.cap_fat.meta.perm:=rs1_c.meta.perm
    cap_compress_cap.cap_fat.renode_id:=rs1_c.renode_id
 
    rs1_cc.bounds.iE     := cap_compress_cap.cap_cc.bounds.iE
    rs1_cc.bounds.t      := cap_compress_cap.cap_cc.bounds.t
    rs1_cc.bounds.tE     := cap_compress_cap.cap_cc.bounds.tE
    rs1_cc.bounds.b      := cap_compress_cap.cap_cc.bounds.b
    rs1_cc.bounds.bE     := cap_compress_cap.cap_cc.bounds.bE
    rs1_cc.bounds.cursor := cap_compress_cap.cap_cc.bounds.cursor
    rs1_cc.meta.ty       := cap_compress_cap.cap_cc.meta.ty
    rs1_cc.meta.perm     := cap_compress_cap.cap_cc.meta.perm
    rs1_cc.renode_id     := cap_compress_cap.cap_cc.renode_id
 
    //for shrink
    when (rs1_c.bounds.base===cap_uncompress_cap.cap_fat.bounds.base)
    {
    CapEModule.io.cap_c:= Cat(cap_compress_cap.cap_cc.bounds.cursor, cap_compress_cap.cap_cc.bounds.bE,cap_compress_cap.cap_cc.bounds.b,cap_compress_cap.cap_cc.bounds.tE,cap_compress_cap.cap_cc.bounds.t,cap_compress_cap.cap_cc.bounds.iE,0.U(3.W), 0.U(3.W) ,34789.U(31.W))//for shrink
    XSDebug("CapEModule.io.cap_c=  %b\n",CapEModule.io.cap_c)
 
    XSDebug("origin uncompress data.top=%b\n",rs1_c.bounds.top)
    XSDebug("last uncompress data.top=%b\n",cap_uncompress_cap.cap_fat.bounds.top)
 
    XSDebug("origin uncompress data.cursor=%b\n", rs1_c.bounds.cursor)
    XSDebug("last uncompress data.cursor=%b\n",cap_uncompress_cap.cap_fat.bounds.cursor)
 
    XSDebug("origin uncompress data.base=%b\n",rs1_c.bounds.base) 
    XSDebug("last uncompress data.base=%b\n",cap_uncompress_cap.cap_fat.bounds.base)
    
    }
    .otherwise
    {
     XSDebug("origin uncompress data not equal\n") 
     CapEModule.io.cap_c:=0.U
     }
    XSDebug("origin2 uncompress data.top=%b\n",rs1_c.bounds.top)
    XSDebug("last2 uncompress data.top=%b\n",cap_uncompress_cap.cap_fat.bounds.top)
 
    cap_uncompress_cap.cap_cc.bounds.iE    :=rs1_cc.bounds.iE
    cap_uncompress_cap.cap_cc.bounds.t     :=rs1_cc.bounds.t
    cap_uncompress_cap.cap_cc.bounds.tE    :=rs1_cc.bounds.tE
    cap_uncompress_cap.cap_cc.bounds.b     :=rs1_cc.bounds.b
    cap_uncompress_cap.cap_cc.bounds.bE    :=rs1_cc.bounds.bE   
    cap_uncompress_cap.cap_cc.bounds.cursor:=rs1_cc.bounds.cursor
    cap_uncompress_cap.cap_cc.meta.ty      :=rs1_cc.meta.ty
    cap_uncompress_cap.cap_cc.meta.perm    :=rs1_cc.meta.perm
    cap_uncompress_cap.cap_cc.renode_id    :=rs1_cc.renode_id
    
    rs2_c.bounds.base    := cap_uncompress_cap.cap_fat.bounds.base
    rs2_c.bounds.top     := cap_uncompress_cap.cap_fat.bounds.top
    rs2_c.bounds.cursor  := cap_uncompress_cap.cap_fat.bounds.cursor   
    rs2_c.renode_id      := cap_uncompress_cap.cap_fat.renode_id
    rs2_c.reg_id         := cap_uncompress_cap.cap_fat.reg_id
    rs2_c.async          := cap_uncompress_cap.cap_fat.async
    rs2_c.padding        := cap_uncompress_cap.cap_fat.padding
    rs2_c.meta.perm      := cap_uncompress_cap.cap_fat.meta.perm
    rs2_c.meta.ty        := cap_uncompress_cap.cap_fat.meta.ty
 
    
 ///end test
   
}
