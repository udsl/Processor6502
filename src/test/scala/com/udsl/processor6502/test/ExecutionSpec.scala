package com.udsl.processor6502.test

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegister.*
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{Processor, StatusRegisterFlags}
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, fixedValuesInitialised, logger, testLocation}
import com.udsl.processor6502.test.ExecutionSpecData.{dataAdcInstructionTest, dataAndInstructionTest, dataAslInstructionTest, dataBccInstructionTest, dataBcsInstructionTest}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class ExecutionSpec extends AnyFlatSpec, should.Matchers, StrictLogging:


  "Given a valid ADC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAdcInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid AND instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAndInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid ASL instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataAslInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BCC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBccInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BCS instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBcsInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }


object ExecutionSpec extends StrictLogging:

  var fixedValuesInitialised = false
  val testLocation = 2000
  val absTestLocation = 2500
  val absTestLocation2 = 2600

  def asHexStr( v: Int): String =
    s"0x${v.toHexString.toUpperCase()}"

  def checkRes(resData: ResultData, memRes: ResultMemData, title: String, opcodeExecuted: OpcodeValue): Unit =
    logger.info(s"Checking results for $title")
    assert(Processor.ac.value == resData.ac, s"AC = ${Processor.ac.value} required ${resData.ac} - $resData")
    assert(Processor.ix.value == resData.ix, s"IX = ${Processor.ix.value} required ${resData.ix} - $resData")
    assert(Processor.iy.value == resData.iy, s"IY = ${Processor.iy.value} required ${resData.iy} - $resData")

    // Ensure we have the UNUSED_FLAG_MASK in the value
    val requiredMask: Int = UNUSED_FLAG_MASK | resData.sr

    assert(Processor.sr.value == requiredMask, s"SR = ${Processor.sr.value} required $requiredMask - $resData")

    if resData.pc > 0 then // include PC check
      val pc = Processor.pc.addr
      assert(pc == resData.pc, s"PC = ${asHexStr(pc)} expected ${asHexStr(resData.pc)}")

    if memRes.byte then
      // checking memory byte
      val checkByte = memoryAccess.getMemoryByte(memRes.loc)
      assert(checkByte == memRes.value,s"Memory byte at = ${memRes.loc} (${asHexStr(memRes.loc)}) is $checkByte (${asHexStr(checkByte)}) required ${memRes.value} " +
        s"(0x${memRes.value.toHexString.toUpperCase()})")
    else if memRes.loc > 0 then // testing location zer0 makes no sense
      // checking memory wrd
      val checkWrd = memoryAccess.getMemoryWrd(memRes.loc)
      assert(checkWrd == memRes.value,s"Memory word at = ${memRes.loc} (${asHexStr(memRes.loc)}) is $checkWrd (${asHexStr(checkWrd)}) required ${memRes.value} " +
        s"(0x${memRes.value.toHexString.toUpperCase()})")

  /**
   * Initialise memory and registers for test
   */
  def initFixedValuesForTest() : Unit =
    if !fixedValuesInitialised then
      AssembleLocation.setAssembleLoc(100) // zero page location
      // setting a zero page pointer to address 0x638
      AssembleLocation.setMemoryByte(0x38) // 100 (0x64) = 56
      AssembleLocation.setMemoryByte(6) // 101 (0x65) =6
      AssembleLocation.setMemoryByte(0x3F) // 102 (0x66) = 63
      AssembleLocation.setMemoryByte(0x80) // 103 (0x67) = 128
      AssembleLocation.setMemoryAddress(absTestLocation2) // 103 (ox67) pointer to absTestLocation2

      AssembleLocation.setAssembleLoc(0x638) // set current location to ins location
      for x <- List(1,2,3,4, 0x80) do
        AssembleLocation.setMemoryByte(x)

      AssembleLocation.setAssembleLoc(absTestLocation) // set current location to absTestLocation
      AssembleLocation.setMemoryByte(0x33) // absTestLocation = 0x33
      AssembleLocation.setMemoryByte(0xCC) // (absTestLocation  + 1)= 0xCC
      AssembleLocation.setMemoryByte(0x84) // (absTestLocation  + 2)= 0x84
      AssembleLocation.setMemoryByte(0x00) // (absTestLocation  + 3)= 0x00
      AssembleLocation.setMemoryAddress(absTestLocation2) // (absTestLocation  + 2) pointer to address absTestLocation2

      AssembleLocation.setAssembleLoc(absTestLocation2) // set current location to absTestLocation2
      AssembleLocation.setMemoryByte(0xF0) // absTestLocation2 = 0xF0
      AssembleLocation.setMemoryByte(0x3F) // (absTestLocation2 + 1) = 0xF0


      AssembleLocation.setAssembleLoc(NMI_VECTOR)  // start of vectors in memory
      AssembleLocation.setMemoryAddress(1000) // set NMI vector to address 1000
      AssembleLocation.setMemoryAddress(2000) // set RESET vector to address 2000
      AssembleLocation.setMemoryAddress(3000) // set INTERRUPT vector to address 3000


      AssembleLocation.setAssembleLoc(testLocation) // set current location to ins location
    fixedValuesInitialised = true

  def initValuesForTest(sourceData : InsSourceData) : Unit =
    AssembleLocation.setAssembleLoc(testLocation)
    AssembleLocation.setMemoryByte(sourceData.opcode)
    AssembleLocation.setMemoryByte(sourceData.data.loByte)
    if sourceData.data.hasHiByte then
      AssembleLocation.setMemoryByte(sourceData.data.hiByte)
    initRegisters(sourceData.data)

  def initRegisters(regData: InsData) : Unit =
    Processor.pc.addr = testLocation
    Processor.ac.value = regData.regValues.acc
    Processor.ix.value = regData.regValues.ix
    Processor.iy.value = regData.regValues.iy
    Processor.sr.reset()
    if regData.regValues.withCarry then
      Processor.sr.setFlag(StatusRegisterFlags.Carry)



