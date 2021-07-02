import com.udsl.processor6502.cpu.execution.{Immediate, Instruction, Invalid}

val aaa = 3


if (0 to 3 contains aaa) println("hello")


 match {
  case i if 0 to 3 contains aaa => Immediate
  case _ => Invalid
}

println((aaa))