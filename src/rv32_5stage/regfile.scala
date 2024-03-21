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
}

class RegisterFile(implicit val conf: SodorConfiguration) extends Module
{
   val io = IO(new RFileIo())

   val regfile = Mem(32, UInt(conf.xprlen.W))

   when (io.wen && (io.waddr =/= 0.U))
   {
      regfile(io.waddr) := io.wdata
   }

   when (io.dm_en && (io.dm_addr =/= 0.U))
   {
      regfile(io.dm_addr) := io.dm_wdata
   }

   io.rs1_data := Mux((io.rs1_addr =/= 0.U), regfile(io.rs1_addr), 0.U)
   io.rs2_data := Mux((io.rs2_addr =/= 0.U), regfile(io.rs2_addr), 0.U)
   io.dm_rdata := Mux((io.dm_addr =/= 0.U), regfile(io.dm_addr), 0.U)

   //((io.rs1_addr =/= 0.U) && !isAddrInWindow(io.rs1_addr)) || ((io.rs2_addr =/= 0.U) && !isAddrInWindow(io.rs2_addr))
   for (i <- 0 until 32) {
     printf(p"Reg[$i]: 0x${Hexadecimal(regfile(i))} ")
   }
   printf(p"\n") // Print newline after all registers have been printed


}
}
