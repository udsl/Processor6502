package com.udsl.processor6502.test

import com.typesafe.scalalogging.StrictLogging
import com.udsl.processor6502.assembler.AssembleLocation.logger
import com.udsl.processor6502.assembler.{AssembleLocation, InstructionToken}
import com.udsl.processor6502.cpu.Memory.NMI_VECTOR
import com.udsl.processor6502.cpu.Processor.*
import com.udsl.processor6502.cpu.StatusRegister.*
import com.udsl.processor6502.cpu.StatusFlag.Unused
import com.udsl.processor6502.cpu.execution.*
import com.udsl.processor6502.cpu.{Processor, StatusFlag, StatusRegister}
import com.udsl.processor6502.test.ExecutionSpec.{absTestLocation, absTestLocation2, logger, testLocation}
import com.udsl.processor6502.test.ExecutionSpecData.{dataAdcInstructionTest, dataAndInstructionTest, dataAslInstructionTest, dataBccInstructionTest, dataBcsInstructionTest, dataBitInstructionTest, dataBmiInstructionTest, dataBneInstructionTest, dataBplInstructionTest, dataBrkInstructionTest, dataBvcInstructionTest, dataBvsInstructionTest, dataClcInstructionTest, dataCldInstructionTest, dataCliInstructionTest, dataClvInstructionTest, dataCmpInstructionTest, dataCpxInstructionTest, dataCpyInstructionTest, dataDecInstructionTest, dataDexInstructionTest, dataDeyInstructionTest, dataEorInstructionTest, dataIncInstructionTest, dataInxInstructionTest, dataInyInstructionTest}
import com.udsl.processor6502.test.InsData.{checkValue, logger}
import com.udsl.processor6502.test.Validation.{checkAcc, checkIx, checkIy, checkPc, checkSr}
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


  "Given a valid BIT instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBitInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BMI instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBmiInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BNE instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBneInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BPL instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBplInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BRK instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBrkInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BVC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBvcInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid BVS instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataBvsInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CLC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataClcInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CLD instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataCldInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CLI instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataCliInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CLV instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataClvInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CMP instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataCmpInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CPX instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataCpxInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid CPY instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataCpyInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid DEC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataDecInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid DEX instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataDexInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid DEY instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataDeyInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid EOR instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataEorInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid INC instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataIncInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid INX instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataInxInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }

  "Given a valid INY instruction token" should "should execute to the correct opcode and value" in {
    val executionUnit = ExecutionUnit.apply
    ExecutionSpec.initFixedValuesForTest()
    for ((title, insData, resData, memRes) <- dataInyInstructionTest) {
      logger.info(s"\nStarting test: $title")
      ExecutionSpec.initValuesForTest(insData)
      logger.info(s"Single stepping instruction 0x${insData.opcode.toHexString.toUpperCase} at ${Processor.pc.addr}")
      val opcodeExecuted: OpcodeValue = executionUnit.singleStep()
      ExecutionSpec.checkRes(resData, memRes, title, opcodeExecuted)
    }
  }


object ExecutionSpec extends StrictLogging:

  val testLocation = 2000 // 0x7D0
  val absTestLocation = 2500 // 0x9C4
  val absTestLocation2 = 2600 // 0xA28

  def asHexStr( v: Int): String =
    s"0x${v.toHexString.toUpperCase()}"

  def checkRes(resData: ResultData, memRes: ResultMemData, title: String, opcodeExecuted: OpcodeValue): Unit =
    logger.info(s"Checking results for $title")

    checkAcc(resData.ac)
    checkIx(resData.ix)
    checkIy(resData.iy)
    checkSr(resData.sr)
    checkPc(resData.pc)

    // call the stack pointer validation method
    resData.spValidation()

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
    AssembleLocation.setAssembleLoc(100) // zero page location
    // setting a zero page pointer to address 0x638
    AssembleLocation.setMemoryByte(0x38) // 100 (0x64) = 56
    AssembleLocation.setMemoryByte(6) // 101 (0x65) = 6
    AssembleLocation.setMemoryByte(0x3F) // 102 (0x66) = 63
    AssembleLocation.setMemoryByte(0x80) // 103 (0x67) = 128
    AssembleLocation.setMemoryByte(0xF0) // 104 (0x68) = 240
    AssembleLocation.setMemoryByte(0x40) // 105 (0x69) = 64
    AssembleLocation.setMemoryByte(0xC0) // 106 (0x6A) = 192
    AssembleLocation.setMemoryAddress(absTestLocation2) // 107 (0x6B) pointer to absTestLocation2
    AssembleLocation.setMemoryByte(0x00) // 109 (0x6D) = 0
    AssembleLocation.setMemoryByte(0x01) // 110 (0x6E) = 1

    AssembleLocation.setAssembleLoc(0x638) // set current location to ins location
    for x <- List(1,2,3,4, 0x80) do
      AssembleLocation.setMemoryByte(x)

    // absTestLocation = 2500 (0x9c4)
    AssembleLocation.setAssembleLoc(absTestLocation) // set current location to absTestLocation
    AssembleLocation.setMemoryByte(0x33) // absTestLocation (0x9C4) = 51
    AssembleLocation.setMemoryByte(0xCC) // absTestLocation + 1 (0x9C5) = 204
    AssembleLocation.setMemoryByte(0x84) // absTestLocation + 2 (0x9C6) = 132
    AssembleLocation.setMemoryByte(0x00) // absTestLocation + 3 (0x9C7) = 0
    AssembleLocation.setMemoryByte(0x80) // absTestLocation + 4 (0x9C8) = 128
    AssembleLocation.setMemoryByte(0xF0) // absTestLocation + 5 (0x9C9) = 240
    AssembleLocation.setMemoryByte(0x40) // absTestLocation + 6 (0x9CA) = 64
    AssembleLocation.setMemoryAddress(absTestLocation2) // (absTestLocation + 7) pointer to address absTestLocation2

    AssembleLocation.setAssembleLoc(absTestLocation2) // set current location to absTestLocation2
    AssembleLocation.setMemoryByte(0xF0) // absTestLocation2 (0xA28) = 0xF0
    AssembleLocation.setMemoryByte(0x3F) // absTestLocation2 + 1 0xA29) = 0x3F
    AssembleLocation.setMemoryByte(0x0F) // absTestLocation2 + 2 0xA2A) = 0x0F
    AssembleLocation.setMemoryByte(0x66) // absTestLocation2 + 3 0xA2B) = 0x66
    AssembleLocation.setMemoryByte(0xFF) // absTestLocation2 + 4 0xA2C) = 0xFF

    AssembleLocation.setAssembleLoc(NMI_VECTOR)  // start of vectors in memory
    AssembleLocation.setMemoryAddress(1000) // set NMI vector to address 1000
    AssembleLocation.setMemoryAddress(2000) // set RESET vector to address 2000
    AssembleLocation.setMemoryAddress(3000) // set INTERRUPT vector to address 3000

    AssembleLocation.setAssembleLoc(testLocation) // set current location to ins location


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
    //Set initial stack value
    Processor.sp.value = 0xFF
    if regData.regValues.withCarry then
      Processor.sr.setFlag(StatusFlag.Carry)
    if regData.regValues.withZero then
      Processor.sr.setFlag(StatusFlag.Zero)
    if regData.regValues.withNegative then
      Processor.sr.setFlag(StatusFlag.Negative)
    if regData.regValues.withOverflow then
      Processor.sr.setFlag(StatusFlag.Overflow)
    if regData.regValues.withDecimal then
      Processor.sr.setFlag(StatusFlag.Decimal)
    if regData.regValues.withInterupt then
      Processor.sr.setFlag(StatusFlag.Interrupt)
