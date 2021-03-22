package pet.scala.s99.logic

import pet.scala.s99.PxxSpec

class P47Spec extends PxxSpec {
  import S99Logic._ 
  
  "'and' operator" should "implement logical AND" in {
    false and false shouldBe false
    false and true  shouldBe false
    true  and false shouldBe false
    true  and true  shouldBe true
  }
  
  // "'OR' operator" should "implement logical OR" in {
  //   OR(false, false) shouldBe false
  //   OR(false, true)  shouldBe true
  //   OR(true,  false) shouldBe true
  //   OR(true,  true)  shouldBe true
  // }
  
  // "'NOT' operator" should "implement logical NOT" in {
  //   NOT(false) shouldBe true
  //   NOT(true)  shouldBe false
  // }
  
  // "'NAND' operator" should "implement logical NAND" in {
  //   NAND(false, false) shouldBe true
  //   NAND(false, true)  shouldBe true
  //   NAND(true,  false) shouldBe true
  //   NAND(true,  true)  shouldBe false
  // }
  
  // "'NOR' operator" should "implement logical NOR" in {
  //   NOR(false, false) shouldBe true
  //   NOR(false, true)  shouldBe false
  //   NOR(true,  false) shouldBe false
  //   NOR(true,  true)  shouldBe false
  // }
  
  // "'XOR' operator" should "implement logical XOR" in {
  //   XOR(false, false) shouldBe false
  //   XOR(false, true)  shouldBe true
  //   XOR(true,  false) shouldBe true
  //   XOR(true,  true)  shouldBe false
  // }
  
  // "'IMPL' operator" should "implement logical IMPL" in {
  //   IMPL(false, false) shouldBe true
  //   IMPL(false, true)  shouldBe true
  //   IMPL(true,  false) shouldBe false
  //   IMPL(true,  true)  shouldBe true
  // }
  
  // "'EQU' operator" should "implement logical EQU" in {
  //   EQU(false, false) shouldBe true
  //   EQU(false, true)  shouldBe false
  //   EQU(true,  false) shouldBe false
  //   EQU(true,  true)  shouldBe true
  // }
}