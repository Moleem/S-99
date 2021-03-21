package pet.scala.s99.logic

import pet.scala.s99.PxxSpec

class P46Spec extends PxxSpec {
  import S99Logic._ 
  
  "'AND' method" should "implement logical AND" in {
    AND(false, false) shouldBe false
    AND(false, true)  shouldBe false
    AND(true,  false) shouldBe false
    AND(true,  true)  shouldBe true
  }
  
  "'OR' method" should "implement logical OR" in {
    OR(false, false) shouldBe false
    OR(false, true)  shouldBe true
    OR(true,  false) shouldBe true
    OR(true,  true)  shouldBe true
  }
  
  "'NOT' method" should "implement logical NOT" in {
    NOT(false) shouldBe true
    NOT(true)  shouldBe false
  }
  
  "'NAND' method" should "implement logical NAND" in {
    NAND(false, false) shouldBe true
    NAND(false, true)  shouldBe true
    NAND(true,  false) shouldBe true
    NAND(true,  true)  shouldBe false
  }
  
  "'NOR' method" should "implement logical NOR" in {
    NOR(false, false) shouldBe true
    NOR(false, true)  shouldBe false
    NOR(true,  false) shouldBe false
    NOR(true,  true)  shouldBe false
  }
  
  "'XOR' method" should "implement logical XOR" in {
    XOR(false, false) shouldBe false
    XOR(false, true)  shouldBe true
    XOR(true,  false) shouldBe true
    XOR(true,  true)  shouldBe false
  }
  
  "'IMPL' method" should "implement logical IMPL" in {
    IMPL(false, false) shouldBe true
    IMPL(false, true)  shouldBe true
    IMPL(true,  false) shouldBe false
    IMPL(true,  true)  shouldBe true
  }
  
  "'EQU' method" should "implement logical EQU" in {
    EQU(false, false) shouldBe true
    EQU(false, true)  shouldBe false
    EQU(true,  false) shouldBe false
    EQU(true,  true)  shouldBe true
  }
  
  "'table' method" should "return human readable representation of 2 input expressions" in {
    table((a: Boolean, b: Boolean) => OR(NOT(a), b)) shouldBe 
    List(
      List("A",     "B",     "Result"),
      List("false", "false", "true"),
      List("false", "true",  "true"),
      List("true",  "false", "false"),
      List("true",  "true",  "true")
    )
  }
}