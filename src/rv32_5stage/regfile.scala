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
}
class RegisterFile(implicit val conf: SodorConfiguration) extends Module {
   val io = IO(new RFileIo())

   val regfile = Mem(32, UInt(conf.xprlen.W))

   val counter = RegInit(0.U(5.W))

   val rs1_ready = RegInit(false.B)
   val rs2_ready = RegInit(false.B)

   // Increment by 1 to create a counter that goes from 0 to 31
   counter := Mux(counter === 31.U, 0.U, counter + 1.U)
   
   // Read window check, including when the counter wraps around
   def isAddrInWindow(addr: UInt, size: UInt = 32.U): Bool = {
      val windowStart = counter % 32.U
      val windowEnd = (counter + size) % 32.U
      ((addr >= windowStart) && (addr < windowEnd)) 
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

   // Generate the stall signal. Stall if either rs1_addr or rs2_addr is outside the window and non-zero
   io.stall := false.B
   //((io.rs1_addr =/= 0.U) && !isAddrInWindow(io.rs1_addr)) || ((io.rs2_addr =/= 0.U) && !isAddrInWindow(io.rs2_addr))
}
}
