
package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{LookupTree, LookupTreeDefault, ParallelMux, SignExt, ZeroExt}
import utils._
import xiangshan._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import circt.stage.ChiselStage

// Capstone node unit (mock) with internal array instead of external node unit
// Requests are generated in the execution stage and queued in
// the node unit until they are resolved later.
// Scoreboard needs to record the resolution state.
// Commit stage stalls until all node queries have been resolved.
// For write-type operations, just send to node unit and forget about it.
// mlabaf

// data structure


class revnode_t extends Bundle{
  val prev       = cap_revnode_id_t  
  val next       = cap_revnode_id_t
  val depth      = UInt (32.W) 
  val valid      = UInt (1.W)  
  val linear     = UInt (1.W)  
  val refcount   = UInt (32.W) 
  }

// def revnode_id_is_null ( ) = { &revnode_id }
class revnode_id_is_null  extends Module 
   {  //check if there is any one bit
    val io = IO(new Bundle{
     val revnode_id = Input (cap_revnode_id_t)
     val out = Output(UInt(1.W)) 
    })
    // io.out:= &k
     when (io.revnode_id===0.U(31.W))
     {  
      io.out:=0.U
     }
    .otherwise
    {
       io.out:=1.U
    }
   }
class alloc_new  extends Module //mlabaf//bug in origin code with multiple return
   { 
    val io = IO(new Bundle{
    val allocated_n_q  = Input(UInt(32.W))
    val allocated_n_n  = Output(UInt(32.W))
    val free_list_q    = Input (cap_revnode_id_t)
    val free_list_n    = Output(cap_revnode_id_t)
    val out            = Output(cap_revnode_id_t)
    val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
   })
   
   val revnode_id_is_null_n=Module(new(revnode_id_is_null))
   revnode_id_is_null_n.io.revnode_id:=io.free_list_q
   val k=revnode_id_is_null_n.io.out
   val k2=io.free_list_q
   
    io.allocated_n_n:= 0.U
    io.out:= 0.U

    // when (io.allocated_n_q < chiselTypeOf(REVNODE_BUF_N)) 
    when (io.allocated_n_q < REVNODE_BUF_N_c) 
    {
      io.allocated_n_n := io.allocated_n_q + 1.U
      io.out := io.allocated_n_q
    }

    // get from free list
    when (k===0.U) 
    {
      io.out:=CAP_REVNODE_ID_NULL
    }
    io.free_list_n := io.node_pool_q(k2).next
    // io.out := io.free_list_q
   }

class alloc_new_uncommitted  extends Module //mlabaf//bug in origin code with multiple return
{ 
    val io = IO(new Bundle{
    val uncommitted_free_list_q    = Input(cap_revnode_id_t)
    val uncommitted_free_list_n    = Output(cap_revnode_id_t)
    val uncommitted_allocated_n_q  = Input(UInt(32.W))
    val uncommitted_allocated_n_n  = Output (cap_revnode_id_t)
    val out            = Output(cap_revnode_id_t)
    val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
   })
  io.uncommitted_allocated_n_n := 0.U
  io.uncommitted_free_list_n   := 0.U
  io.out:= 0.U
  when (io.uncommitted_allocated_n_q < REVNODE_BUF_N_c)
   {
      io.uncommitted_allocated_n_n := io.uncommitted_allocated_n_q + 1.U
      io.out:=io.uncommitted_allocated_n_q
   }

    // get from free list
  when  (io.uncommitted_free_list_q =/= 0.U)
   {
     io.out:=CAP_REVNODE_ID_NULL
   } 

    io.uncommitted_free_list_n := io.node_pool_q(io.uncommitted_free_list_q).next
    // io.out:= io.uncommitted_free_list_q
  
}

class do_create  extends Module 
{ 
    val io = IO(new Bundle{
    val revnode_id    = Input(cap_revnode_id_t)
    val out            = Output(cap_revnode_id_t)
    val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))
    val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
    val allocated_n_q  = Input(UInt(32.W))
    val free_list_q    = Input (cap_revnode_id_t)
    val REVNODE_INIT   = Input (new revnode_t)
   })
    val new_node  = Module(new alloc_new)
    new_node.io.allocated_n_q  := io.allocated_n_q
    new_node.io.free_list_q    := io.free_list_q 
    new_node.io.node_pool_q    := io.node_pool_q

    // io.node_pool_n(new_node.io.out) := REVNODE_INIT
    io.node_pool_n(new_node.io.out) := io.REVNODE_INIT
    // node_pool_n[new_node].valid = 1'b0; // just a sanity check
    // $display("Created new node = %d", new_node);
    io.out:=new_node.io.out
}
class do_delin extends Module 
{
    val io = IO(new Bundle{ 
     val revnode_id     = Input(cap_revnode_id_t)
     val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))    
     })

    io.node_pool_n(io.revnode_id).linear := 0.U
}

class do_init extends Module 
{ 
   val io = IO(new Bundle{
   val allocated_n_n  = Output(UInt(32.W))
   val free_list_n    = Output(cap_revnode_id_t)
  //  val node_pool_n   = Output(Vec(REVNODE_BUF_N,new revnode_t))
   val node_pool_n0    = Output(new revnode_t)
   val node_pool_n1    = Output(new revnode_t)
   val node_pool_n2    = Output(new revnode_t)
   val REVNODE_INIT    = Input (new revnode_t)

  })    
    // the first three node ids should be guaranteed to be 0, 1, and 2
    // assumed in CAPENTER
    io.allocated_n_n := 3.U
    io.free_list_n   := CAP_REVNODE_ID_NULL
    // io.node_pool_n   := 0.U

    io.node_pool_n0 := io.REVNODE_INIT
    io.node_pool_n1 := io.REVNODE_INIT
    io.node_pool_n2 := io.REVNODE_INIT

    // $display("Node unit initialisation done");
  }

class mock_node_unit extends Module 
 { 
     val io = IO(new Bundle{
      val flush_i = Input(UInt(1.W))
      // query interface (asynchronous)
      val query_valid_i = Input(Vec(3, UInt (1.W)))
      val query_ready_o = Output(Vec(3,UInt(1.W)))
      val query_i       = Input(Vec(3,new node_query_t))//node_query_t
      val query_valid_o = Output(Vec(3,UInt(1.W)))
      val query_resp_o  = Output(Vec(3,new node_query_resp_t))//
      // allocation interface
      val alloc_valid_i = Input(UInt(1.W))
      val alloc_ready_o = Output(UInt(1.W))
      val alloc_valid_o = Output(UInt(1.W)) 
      val alloc_node_id_o = Output(cap_revnode_id_t)//c>vmv,ap_revnode_id_t // allocated node ID
      // mutation interface
      val mut_valid_i = Input(UInt(1.W))
      val mut_ready_o = Output(UInt(1.W))
      val mut_i       = Input(new node_mut_t)//node_mut_t
      // stalling (from scoreboard) 
      val query_stall_i = Input(UInt(1.W))

    })

    //query

    //  val ALLOC_BUFFER_N = 4 //ToDo localparam CVA6ConfigNrScoreboardEntries = 4;
     val query_last_q          = RegInit(VecInit(Seq.fill(3)(0.U.asTypeOf(new node_query_t))))//Vec(3,(new node_query_t))
     val valid_query_last_q    = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))//Vec(3,UInt(1.W))
    // val REVNODE_BUF_N = 1024
     val node_pool_q, node_pool_n =RegInit(VecInit(Seq.fill(REVNODE_BUF_N)(0.U.asTypeOf(new revnode_t))))// Vec(REVNODE_BUF_N,new revnode_t)
     val uncommitted_allocated_n_q, uncommitted_allocated_n_n = RegInit(0.U(32.W))//Vec(32,UInt(1.W))
     val free_list_q, free_list_n = RegInit(0.U(32.W))//cap_revnode_id_t
     val REVNODE_INIT = Wire(new revnode_t)
    
    REVNODE_INIT.prev   := CAP_REVNODE_ID_NULL//cap_revnode_id_t//CAP_REVNODE_ID_NULL
    REVNODE_INIT.next   := CAP_REVNODE_ID_NULL//cap_revnode_id_t//CAP_REVNODE_ID_NULL
    REVNODE_INIT.depth  := 0.U
    REVNODE_INIT.valid  := 1.U
    REVNODE_INIT.linear := 1.U
    REVNODE_INIT.refcount:= 1.U

    //index 0  
     io.query_ready_o(0) := ~io.query_stall_i
     io.query_resp_o(0).trans_id := query_last_q(0).trans_id
     io.query_resp_o(0).synchronous := query_last_q(0).synchronous
     io.query_valid_o(0) := valid_query_last_q(0)

    // TODO: optimise
    // always_comb begin
      io.query_resp_o(0).r_valid := node_pool_q(query_last_q(0).node_id).valid
    //   for (int unsigned j = 0; j < ALLOC_BUFFER_N; j ++) begin
      when((uncommitted_allocated_n_q(0) === query_last_q(0).node_id)||(uncommitted_allocated_n_q(1) === query_last_q(0).node_id) ||(uncommitted_allocated_n_q(2) === query_last_q(0).node_id) ||(uncommitted_allocated_n_q(3) === query_last_q(0).node_id) ) 
      {
        io.query_resp_o(0).r_valid := 1.U
      }

    // from capstone_unit//from load and store unit
    //index 1
     io.query_ready_o(1) := ~io.query_stall_i
     io.query_resp_o(1).trans_id := query_last_q(1).trans_id
     io.query_resp_o(1).synchronous := query_last_q(1).synchronous
     io.query_valid_o(1) := valid_query_last_q(1)


      io.query_resp_o(1).r_valid := node_pool_q(query_last_q(1).node_id).valid
    //   for (int unsigned j = 0; j < ALLOC_BUFFER_N; j ++) begin
      when((uncommitted_allocated_n_q(0) === query_last_q(1).node_id)||(uncommitted_allocated_n_q(1) === query_last_q(1).node_id) ||(uncommitted_allocated_n_q(2) === query_last_q(1).node_id) ||(uncommitted_allocated_n_q(3) === query_last_q(1).node_id) ) 
      {
        io.query_resp_o(1).r_valid := 1.U
      }

    //index 2
     io.query_ready_o(2) := ~io.query_stall_i
     io.query_resp_o(2).trans_id := query_last_q(2).trans_id
     io.query_resp_o(2).synchronous := query_last_q(2).synchronous
     io.query_valid_o(2) := valid_query_last_q(2)

  
      io.query_resp_o(2).r_valid := node_pool_q(query_last_q(2).node_id).valid
    //   for (int unsigned j = 0; j < ALLOC_BUFFER_N; j ++) begin
      when((uncommitted_allocated_n_q(0) === query_last_q(2).node_id)||(uncommitted_allocated_n_q(1) === query_last_q(2).node_id) ||(uncommitted_allocated_n_q(2) === query_last_q(2).node_id) ||(uncommitted_allocated_n_q(3) === query_last_q(2).node_id) ) 
      {
        io.query_resp_o(2).r_valid := 1.U
      }

   // allocation


   val uncommitted_alloc_buffer_q, uncommitted_alloc_buffer_d,alloc_buffer_q, alloc_buffer_d= RegInit(VecInit(Seq.fill(ALLOC_BUFFER_N)("b1111111111111111111111111111111".U.asTypeOf(cap_revnode_id_t)))) //Vec(ALLOC_BUFFER_N, cap_revnode_id_t) //?
   val alloc_valid_last_q = RegInit(0.U(1.W))
   val alloc_node_id_q, alloc_node_id_n = RegInit(("b1111111111111111111111111111111".U.asTypeOf(cap_revnode_id_t)))//cap_revnode_id_t
   io.alloc_ready_o := 1.U
   io.alloc_valid_o := alloc_valid_last_q
   io.alloc_node_id_o := alloc_node_id_q
  
   // mutation
    io.mut_ready_o := 1.U
  
  
   // committed node allocation state
   val allocated_n_q, allocated_n_n = RegInit(0.U(32.W))
   // all inflight states
   val uncommitted_free_list_q, uncommitted_free_list_n = RegInit(("b1111111111111111111111111111111".U.asTypeOf(cap_revnode_id_t)))//cap_revnode_id_t

   // single-cycle non-synthesisable implementation

   // always_comb begin : commit_mut
    allocated_n_n := allocated_n_q
    free_list_n   := free_list_q
    node_pool_n   := node_pool_q

    alloc_buffer_d := alloc_buffer_q

    // coming from commit, so don't care about flush_i
    printf("node_mut_type_t.io.mut_valid_i %b \n", io.mut_valid_i)
    printf("node_mut_type_t.io.mut_i.node_op %b \n", io.mut_i.node_op)
    
    //var needed in below when
    var depth       = Wire(UInt(32.W))
    var cur         = RegInit(0.U(32.W))//(cap_revnode_id_t) 
    var retain_data = RegInit(0.U(1.W))
    val prev_node   = Wire (cap_revnode_id_t)   
    prev_node := 0.U
    depth     := 0.U
    
    when (io.mut_valid_i===1.U) 
    {
      // just a single cycle, process the committed operation

      // we have to use the stored node id because we can't assume the
      // actual mutation can be performed immediately
    when (io.mut_i.alloc===1.U) 
      {
        // $display("Committed allocation %d", mut_i.new_node_id);
        // for (int unsigned i = 0; i < ALLOC_BUFFER_N - 1; i ++) begin
        // alloc_buffer_d:= (alloc_buffer_q<<1.U)      
        alloc_buffer_d(1):= alloc_buffer_q(0)      
        alloc_buffer_d(2):= alloc_buffer_q(1)      
        alloc_buffer_d(3):= alloc_buffer_q(2)      
        alloc_buffer_d(0) := io.mut_i.new_node_id
      }
     switch (io.mut_i.node_op)
     {
      is ( node_mut_type_t.NODE_INC) 
      {
        printf("node_mut_type_t.NODE_INC\n")
      }
      is ( node_mut_type_t.NODE_DEC) 
      {
         printf("node_mut_type_t.NODE_DEC\n")
       
      }
      is ( node_mut_type_t.NODE_MREV) 
      {  // create a revocation node as the parent node
        // val do_mrev_f  = Module(new do_mrev)
        // do_mrev_f.io.revnode_id    :=  io.mut_i.node_id
        // do_mrev_f.io.node_pool_q   :=  node_pool_q
        // do_mrev_f.io.allocated_n_q := allocated_n_q
        // do_mrev_f.io.free_list_q   := free_list_q 
        // node_pool_n := do_mrev_f.io.node_pool_n 

        // val  prev_node = Wire (cap_revnode_id_t)
        val new_node  = Module(new alloc_new)
        new_node.io.allocated_n_q  := allocated_n_q
        new_node.io.free_list_q    := free_list_q 
        new_node.io.node_pool_q    := node_pool_q
    
        node_pool_n(new_node.io.out).depth    := node_pool_q(io.mut_i.node_id).depth
        node_pool_n(new_node.io.out).valid    := 1.U
        node_pool_n(new_node.io.out).linear   := 1.U
        node_pool_n(new_node.io.out).refcount := 1.U
    
        prev_node := node_pool_q(io.mut_i.node_id).prev
        node_pool_n(new_node.io.out).prev := prev_node
    
        val revnode_id_is_null_n=Module(new(revnode_id_is_null))
        revnode_id_is_null_n.io.revnode_id:=free_list_q
    
        when (revnode_id_is_null_n.io.out===0.U)
        {
          node_pool_n(prev_node).next := new_node.io.out  
        }
       
        node_pool_n(new_node.io.out).next := io.mut_i.node_id
        node_pool_n(io.mut_i.node_id).prev   := new_node.io.out
        printf("node_mut_type_t.NODE_MREV\n")

      }
      is ( node_mut_type_t.NODE_REVOKE) 
      {  // returns whether the original content of the memory region can be retained
        // //val do_revoke_f  = Module(new do_revoke)// TODO: uninitialised capability
        //// do_revoke_f.io.revnode_id := io.mut_i.node_id
        // //do_revoke_f.io.node_pool_q:= node_pool_q
        // //node_pool_n:= do_revoke_f.io.node_pool_n  
        // //val out            = Output(cap_revnode_id_t)
          // var depth       = Wire(UInt(32.W))
          // var cur         = Reg(cap_revnode_id_t) 
          // var retain_data = Reg(UInt(1.W))

          depth := node_pool_q(io.mut_i.node_id).depth
          cur   := node_pool_q(io.mut_i.node_id).next
          retain_data := 1.U

          val revnode_id_is_null_n = Module(new(revnode_id_is_null))
          revnode_id_is_null_n.io.revnode_id:=cur

          when ((revnode_id_is_null_n.io.out===0.U) && (node_pool_q(cur).depth > depth)) 
          {
           retain_data := retain_data | ~node_pool_q(cur).linear
           node_pool_n(cur).valid := 0.U
          }

          node_pool_n(io.mut_i.node_id).next := cur
          when (revnode_id_is_null_n.io.out===0.U) 
          {
            node_pool_n(cur).prev := io.mut_i.node_id
          }         
          printf("node_mut_type_t.NODE_REVOKE\n")
    
          }
          is ( node_mut_type_t.NODE_SPLIT) 
          {
          //  val split_f  = Module(new do_split)
          //  split_f.io.revnode_id    := io.mut_i.node_id
          //  split_f.io.node_pool_q   := node_pool_q
          //  split_f.io.allocated_n_q := allocated_n_q
          //  split_f.io.free_list_q   := free_list_q      
          //  node_pool_n := split_f.io.node_pool_n
    
    
        //  val  prev_node = Wire (cap_revnode_id_t)
         val new_node  = Module(new alloc_new)
         new_node.io.allocated_n_q  := allocated_n_q    
         new_node.io.free_list_q    := free_list_q 
         new_node.io.node_pool_q    := node_pool_q
     
         node_pool_n(new_node.io.out).depth    := node_pool_q(io.mut_i.node_id).depth
         node_pool_n(new_node.io.out).valid    := 1.U
         node_pool_n(new_node.io.out).linear   := 1.U
         node_pool_n(new_node.io.out).refcount := 1.U
     
         prev_node := node_pool_q(io.mut_i.node_id).prev
         node_pool_n(new_node.io.out).prev := prev_node
     
         val revnode_id_is_null_n=Module(new(revnode_id_is_null))
         revnode_id_is_null_n.io.revnode_id:=free_list_q
     
         when (revnode_id_is_null_n.io.out===0.U)
         {
         node_pool_n(prev_node).next := new_node.io.out  
         }
       
       node_pool_n(new_node.io.out).next := io.mut_i.node_id
       node_pool_n(io.mut_i.node_id).prev   := new_node.io.out
  
       new_node.io.free_list_q    := free_list_q 
       new_node.io.node_pool_q    := node_pool_q
    
       node_pool_n(new_node.io.out).depth    := node_pool_q(io.mut_i.node_id).depth
       node_pool_n(new_node.io.out).valid    := 1.U
       node_pool_n(new_node.io.out).linear   := 1.U
       node_pool_n(new_node.io.out).refcount := 1.U
    
       printf("node_mut_type_t.NODE_SPLIT: %b , %b , \n",new_node.io.out,io.mut_i.node_id )    
      }
      is ( node_mut_type_t.NODE_DELIN) 
      {
       //  val do_delin_f  = Module(new do_delin)
       //  do_delin_f.io.revnode_id := io.mut_i.node_id
       //  node_pool_n              := do_delin_f.io.node_pool_n
    
        node_pool_n( io.mut_i.node_id).linear := 0.U
        printf("node_mut_type_t.NODE_DELIN , node_id, node_pool_linear= %b , %b\n", io.mut_i.node_id,node_pool_n( io.mut_i.node_id).linear )
        printf("node_mut_type_t.NODE_DELIN\n")

      }
      is ( node_mut_type_t.NODE_INIT) 
      {// the first three node ids should be guaranteed to be 0, 1, and 2
       // assumed in CAPENTER       
       val do_init_f      = Module(new do_init)
       allocated_n_n     := do_init_f.io.allocated_n_n
       free_list_n       := do_init_f.io.free_list_n
       node_pool_n(0)    := do_init_f.io.node_pool_n0
       node_pool_n(1)    := do_init_f.io.node_pool_n1
       node_pool_n(2)    := do_init_f.io.node_pool_n2
       do_init_f.io.REVNODE_INIT   := REVNODE_INIT 
       printf("node_mut_type_t.NODE_INIT\n")

      }
      is ( node_mut_type_t.NODE_CREATE) 
      {
       //  val do_create_f    = Module(new do_create)
       //  do_create_f.io.revnode_id     := io.mut_i.node_id
       //  node_pool_n                   := do_create_f.io.node_pool_n
       //  do_create_f.io.node_pool_q    := node_pool_q
       //  do_create_f.io.allocated_n_q  := allocated_n_q 
       //  do_create_f.io.free_list_q    := free_list_q 
       //  do_create_f.io.REVNODE_INIT   := REVNODE_INIT   

       val new_node  = Module(new alloc_new)
       new_node.io.allocated_n_q  := allocated_n_q
       new_node.io.free_list_q    := free_list_q 
       new_node.io.node_pool_q    := node_pool_q
       node_pool_n(new_node.io.out) := REVNODE_INIT
       printf("node_mut_type_t.NODE_CREATE\n")

      }
     }
   }

    // always_comb begin : uncommitted_allocation
    uncommitted_allocated_n_n := uncommitted_allocated_n_q
    uncommitted_free_list_n := uncommitted_free_list_q

    uncommitted_alloc_buffer_d := uncommitted_alloc_buffer_q

    alloc_node_id_n := CAP_REVNODE_ID_NULL

    when ((io.alloc_valid_i===1.U) && (io.flush_i===0.U)) 
    {
     val alloc_new_uncommitted_ins      = Module(new alloc_new_uncommitted)
    //  val alloc_node_id_n      = Module(new alloc_new_uncommitted)
     alloc_new_uncommitted_ins.io.uncommitted_free_list_q    := uncommitted_free_list_q
     alloc_new_uncommitted_ins.io.uncommitted_allocated_n_q  := uncommitted_allocated_n_q
     alloc_new_uncommitted_ins.io.node_pool_q    := node_pool_q
     uncommitted_allocated_n_n  := alloc_new_uncommitted_ins.io.uncommitted_allocated_n_n
     uncommitted_free_list_n    := alloc_new_uncommitted_ins.io.uncommitted_free_list_n//?
     alloc_node_id_n            := alloc_new_uncommitted_ins.io.out
      // $display("Uncommitted allocation %d", alloc_node_id_n);
      // for (int unsigned i = 0; i < ALLOC_BUFFER_N - 1; i ++) begin
      uncommitted_alloc_buffer_d(1) := uncommitted_alloc_buffer_q(0)
      uncommitted_alloc_buffer_d(2) := uncommitted_alloc_buffer_q(1)
      uncommitted_alloc_buffer_d(3) := uncommitted_alloc_buffer_q(2)
      
      uncommitted_alloc_buffer_d(0) := alloc_node_id_n//alloc_node_id_n_ins.io.uncommitted_allocated_n_n//?
    }

    when (io.flush_i===1.U)
    {
      uncommitted_allocated_n_n  := allocated_n_n
      uncommitted_free_list_n    := free_list_n
      uncommitted_alloc_buffer_d := alloc_buffer_d
    }


    // always_ff @(posedge clk_i or negedge rst_ni) begin

    query_last_q := io.query_i
    // for (int unsigned i = 0; i < 3; i ++) begin
    valid_query_last_q(0) := io.query_valid_i(0) & ~io.flush_i
    valid_query_last_q(1) := io.query_valid_i(1) & ~io.flush_i
    valid_query_last_q(2) := io.query_valid_i(2) & ~io.flush_i
      
    alloc_valid_last_q := io.alloc_valid_i & ~io.flush_i
    uncommitted_alloc_buffer_q := uncommitted_alloc_buffer_d
    alloc_buffer_q  := alloc_buffer_d
    alloc_node_id_q := alloc_node_id_n//C
    node_pool_q     := node_pool_n
    allocated_n_q   := allocated_n_n
    free_list_q     := free_list_n
    uncommitted_allocated_n_q := uncommitted_allocated_n_n//C
    uncommitted_free_list_q   := uncommitted_free_list_n
    printf("uncommitted_allocated_n_q , uncommitted_free_list_q= %b, %b, %b , %b, %b\n", uncommitted_allocated_n_q, uncommitted_free_list_q, allocated_n_q, free_list_q, alloc_node_id_n)


 }

   

// class do_split extends Module 
//    { 
//     val io = IO(new Bundle{
//     val revnode_id    = Input(cap_revnode_id_t)
//     val out            = Output(cap_revnode_id_t)
//     val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))
//     val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))
//     val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
//     val allocated_n_q  = Input(UInt(32.W))
//     val free_list_q    = Input (cap_revnode_id_t)
//    })

//     val  prev_node = Wire (cap_revnode_id_t)
//     val new_node  = Module(new alloc_new)
//     new_node.io.allocated_n_q  := io.allocated_n_q
//     new_node.io.free_list_q    := io.free_list_q 
//     new_node.io.node_pool_q    := io.node_pool_q

//     io.node_pool_n(new_node.io.out).depth    := io.node_pool_q(io.revnode_id).depth
//     io.node_pool_n(new_node.io.out).valid    := 1.U
//     io.node_pool_n(new_node.io.out).linear   := 1.U
//     io.node_pool_n(new_node.io.out).refcount := 1.U

//     prev_node := io.node_pool_q(io.revnode_id).prev
//     io.node_pool_n(new_node.io.out).prev := prev_node

//     val revnode_id_is_null_n=Module(new(revnode_id_is_null))
//     revnode_id_is_null_n.io.revnode_id:=io.free_list_q

//     when (revnode_id_is_null_n.io.out===0.U)
//     {
//       io.node_pool_n(prev_node).next := new_node.io.out  
//     }
   
//     io.node_pool_n(new_node.io.out).next := io.revnode_id
//     io.node_pool_n(io.revnode_id).prev   := new_node.io.out

//     io.out := new_node.io.out
//   }

  // create a revocation node as the parent node
// class do_mrev  extends Module 
//   { 
//     val io = IO(new Bundle{
//     val revnode_id     = Input(cap_revnode_id_t)
//     val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))
//     val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
//     val out            = Output(cap_revnode_id_t)
//     val allocated_n_q = Input(UInt(32.W))
//     val free_list_q   = Input (cap_revnode_id_t)
//     })

//   //    val split  = Module(new do_split)
//   //    split.io.revnode_id    := io.revnode_id
//   //    split.io.node_pool_q   := io.node_pool_q
//   //    io.node_pool_n         := split.io.node_pool_n
//   //    split.io.allocated_n_q := io.allocated_n_q
//   //    split.io.free_list_q   := io.free_list_q

//   //   io.node_pool_n(io.revnode_id).depth := io.node_pool_q(io.revnode_id).depth + 1.U(32.W)
//   //  // return do_split(revnode_id);
//   //   io.out:= split.io.out

//     val  prev_node = Wire (cap_revnode_id_t)
//     val new_node  = Module(new alloc_new)
//     new_node.io.allocated_n_q  := io.allocated_n_q
//     new_node.io.free_list_q    := io.free_list_q 
//     new_node.io.node_pool_q    := io.node_pool_q

//     node_pool_n(new_node.io.out).depth    := node_pool_q(io.revnode_id).depth
//     node_pool_n(new_node.io.out).valid    := 1.U
//     node_pool_n(new_node.io.out).linear   := 1.U
//     node_pool_n(new_node.io.out).refcount := 1.U

//     prev_node := node_pool_q(io.revnode_id).prev
//     node_pool_n(new_node.io.out).prev := prev_node

//     val revnode_id_is_null_n=Module(new(revnode_id_is_null))
//     revnode_id_is_null_n.io.revnode_id:=free_list_q

//     when (revnode_id_is_null_n.io.out===0.U)
//     {
//       node_pool_n(prev_node).next := new_node.io.out  
//     }
   
//     node_pool_n(new_node.io.out).next := revnode_id
//     node_pool_n(io.revnode_id).prev   := new_node.io.out

//   }

  // returns whether the original content of the memory region can be retained
  // class do_revoke extends Module {
  //   val io = IO(new Bundle{ 
  //    val revnode_id     = Input(cap_revnode_id_t)
  //    val node_pool_n    = Output(Vec(REVNODE_BUF_N,new revnode_t))
  //    val node_pool_q    = Input(Vec(REVNODE_BUF_N,new revnode_t))
  //    val out            = Output(cap_revnode_id_t)
    
  //    })
  //    var depth       = Wire(UInt(32.W))
  //    var cur         = Wire(cap_revnode_id_t) 
  //    var retain_data = Wire(UInt(1.W))

  //    depth := io.node_pool_q(io.revnode_id).depth
  //    cur   := io.node_pool_q(io.revnode_id).next
  //    retain_data := 1.U

  //   val revnode_id_is_null_n = Module(new(revnode_id_is_null))
  //   revnode_id_is_null_n.io.revnode_id:=cur

  //    when ((revnode_id_is_null_n.io.out===0.U) && (io.node_pool_q(cur).depth > depth)) 
  //    {
  //     retain_data := retain_data | ~io.node_pool_q(cur).linear
  //     io.node_pool_n(cur).valid := 0.U
  //    }

  //    io.node_pool_n(io.revnode_id).next := cur
  //    when (revnode_id_is_null_n.io.out===0.U) 
  //    {
  //      io.node_pool_n(cur).prev := io.revnode_id
  //    }

  //    io.out:= retain_data
  //  }


