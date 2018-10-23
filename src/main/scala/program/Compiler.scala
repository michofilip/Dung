package program

import program.Instruction._
import program.Statement._
import value.Value

import scala.language.implicitConversions

object Compiler {
    def mapLabels(instructions: IndexedSeq[Instruction]): Map[Int, Int] = {
        def m(inst: IndexedSeq[Instruction], line: Int, labels: Map[Int, Int]): Map[Int, Int] = {
            inst match {
                case in +: rest =>
                    in match {
                        case LB(labelId) => m(rest, line + 1, labels + (labelId -> line))
                        case _ => m(rest, line + 1, labels)
                    }
                case _ => labels
            }
        }
        
        m(instructions, 0, Map.empty)
    }
    
    def compile(statement: Statement): IndexedSeq[Instruction] = {
        implicit def i2seq(instruction: Instruction): IndexedSeq[Instruction] = IndexedSeq(instruction)
        
        def compVariant(switchTest: Value[_], variant: Variant, defId: Int, nextLabelId: Int): (IndexedSeq[Instruction], Int) = {
            val (varInnerInst, nextLabelId1) = comp(Seq(variant.variantStatement), IndexedSeq.empty, nextLabelId + 1)
            val varInst =
                IF(switchTest === variant.variantTest) ++
                        GT(nextLabelId) ++
                        varInnerInst ++
                        GT(defId) ++
                        LB(nextLabelId)
            (varInst, nextLabelId1)
        }
        
        def compVariants(switchTest: Value[_], variants: Seq[Variant], defId: Int, inst: IndexedSeq[Instruction], nextLabelId: Int): (IndexedSeq[Instruction], Int) = {
            variants match {
                case variant +: rest =>
                    val (varInst, nextLabelId1) = compVariant(switchTest, variant, defId, nextLabelId)
                    compVariants(switchTest, rest, defId, inst ++ varInst, nextLabelId1)
                case Nil => (inst, nextLabelId)
            }
        }
        
        def comp(sts: Seq[Statement], inst: IndexedSeq[Instruction], nextLabelId: Int): (IndexedSeq[Instruction], Int) = {
            sts match {
                case st +: rest =>
                    st match {
                        case Do(events) =>
                            comp(rest, inst ++ DO(events), nextLabelId)
                        case Block(statements) =>
                            val (blockInst, nextLabelId1) = comp(statements, IndexedSeq.empty, nextLabelId)
                            comp(rest, inst ++ blockInst, nextLabelId1)
                        case When(condition, thenStatement, elseStatement) =>
                            val labelId1 = nextLabelId
                            val labelId2 = nextLabelId + 1
                            val (thenInstructions, nextLabelId1) = comp(Seq(thenStatement), IndexedSeq.empty, nextLabelId + 2)
                            val (elseInstructions, nextLabelId2) = comp(Seq(elseStatement), IndexedSeq.empty, nextLabelId1)
                            val whenInst =
                                IF(condition) ++
                                        GT(labelId1) ++
                                        thenInstructions ++
                                        GT(labelId2) ++
                                        LB(labelId1) ++
                                        elseInstructions ++
                                        LB(labelId2)
                            comp(rest, inst ++ whenInst, nextLabelId2)
                        case Loop(condition, loopedStatement) =>
                            val labelId1 = nextLabelId
                            val labelId2 = nextLabelId + 1
                            val (loopedInst, nextLabelId1) = comp(Seq(loopedStatement), IndexedSeq.empty, nextLabelId + 2)
                            val loopInst =
                                LB(labelId1) ++
                                        IF(condition) ++
                                        GT(labelId2) ++
                                        loopedInst ++
                                        GT(labelId1) ++
                                        LB(labelId2)
                            comp(rest, inst ++ loopInst, nextLabelId1)
                        case Switch(switchTest, variants, defaultStatement) =>
                            val defId = nextLabelId
                            val (varInnerInst, nextLabelId1) = compVariants(switchTest, variants, defId, IndexedSeq.empty, nextLabelId + 1)
                            val (defInst, nextLabelId2) = comp(Seq(defaultStatement), IndexedSeq.empty, nextLabelId1)
                            val varInst =
                                varInnerInst ++
                                        defInst ++
                                        LB(defId)
                            comp(rest, inst ++ varInst, nextLabelId2)
                    }
                case Nil => (inst, nextLabelId)
            }
        }
        
        val (instructions, _) = comp(Seq(statement), IndexedSeq.empty, 0)
        instructions ++ EX
    }
    
}
