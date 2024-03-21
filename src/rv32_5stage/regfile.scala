//**************************************************************************
// RISCV Processor Register File
//--------------------------------------------------------------------------
//

package Sodor
{

import chisel3._
import chisel3.util._


import Constants._
import Common._

class RFileIo(implicit val conf: SodorConfiguration) extends Bundle()
{
   val rs1_addr = Input(UInt(5.W))
   val rs1_data = Output(UInt(conf.xprlen.W))
   val rs2_addr = Input(UInt(5.W))
   val rs2_data = Output(UInt(conf.xprlen.W))
   val dm_addr = Input(UInt(5.W))
   val dm_rdata = Output(UInt(conf.xprlen.W))
   val dm_wdata = Input(UInt(conf.xprlen.W))
   val dm_en = Input(Bool())

   val waddr    = Input(UInt(5.W))
   val wdata    = Input(UInt(conf.xprlen.W))
   val wen      = Input(Bool())
   val stall    = Output(Bool())  // Add stall signal here
   val rs2_valid = Input(Bool())  // whether rs2 is used or not
}
class RegisterFile(implicit val conf: SodorConfiguration) extends Module {
   val io = IO(new RFileIo())

   val regfile = Mem(32, UInt(conf.xprlen.W))

   val counter = RegInit(0.U(6.W))

   val rs1_ready = RegInit(false.B)
   val rs2_ready = RegInit(false.B)

   // Increment by 10 and wrap around from 0 to 31
   counter := (counter + 10.U ) % 32.U
   
   def isAddrInWindow(addr: UInt, size: UInt = 10.U): Bool = {
      val windowStart = counter
      val windowEnd = (counter + size) & 0x1F.U // Ensure wrap-around within 32 registers using mask

      // If the window doesn't wrap around
      val inWindow = (addr >= windowStart) && (addr < windowEnd)
      
      // If the window wraps around
      val wrapAround = (windowEnd < windowStart) && ((addr >= windowStart) || (addr < windowEnd))
      
      inWindow || wrapAround
   }

   // Write within the window condition
   when (io.wen && (io.waddr =/= 0.U)) {
       regfile(io.waddr) := io.wdata
   }

   // Data memory write operation
   when (io.dm_en && (io.dm_addr =/= 0.U)) {
       regfile(io.dm_addr) := io.dm_wdata
   }

   // Only allow reads for addresses within the window or address 0
   io.rs1_data := Mux((io.rs1_addr =/= 0.U) && isAddrInWindow(io.rs1_addr), regfile(io.rs1_addr), 0.U)
   io.rs2_data := Mux((io.rs2_addr =/= 0.U) && isAddrInWindow(io.rs2_addr), regfile(io.rs2_addr), 0.U)
   io.dm_rdata := Mux((io.dm_addr =/= 0.U) && isAddrInWindow(io.dm_addr), regfile(io.dm_addr), 0.U)
   
   when (!rs1_ready){
      rs1_ready := (io.rs1_addr === 0.U) || isAddrInWindow(io.rs1_addr)
   }

   when (!rs2_ready){
      // rs2 may not be used
      rs2_ready := (((io.rs2_addr === 0.U) || isAddrInWindow(io.rs2_addr)) && io.rs2_valid ) || (!io.rs2_valid)
   }

   // Generate the stall signal. Stall if either rs1_addr or rs2_addr is outside the window and non-zero
   io.stall := (!rs1_ready) || (!rs2_ready)

   when (!io.stall){
      rs1_ready := RegNext(false.B)
      rs2_ready := RegNext(false.B)
   }
   printf("rs1_ready: %d, rs2_ready: %d, counter: %d, is_addr2_in: %d, r_data1: %d, r_data2: %d\n",rs1_ready,rs2_ready, counter, isAddrInWindow(io.rs2_addr), io.rs1_data,io.rs2_data)
   //((io.rs1_addr =/= 0.U) && !isAddrInWindow(io.rs1_addr)) || ((io.rs2_addr =/= 0.U) && !isAddrInWindow(io.rs2_addr))
   for (i <- 0 until 32) {
     printf(p"Reg[$i]: 0x${Hexadecimal(regfile(i))} ")
   }
   printf(p"\n") // Print newline after all registers have been printed

}
}
