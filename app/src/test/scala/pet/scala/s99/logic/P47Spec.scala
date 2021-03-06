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
  
  "'or' operator" should "implement logical OR" in {
    false or false shouldBe false
    false or true  shouldBe true
    true  or false shouldBe true
    true  or true  shouldBe true
  }
  
  "'nand' operator" should "implement logical NAND" in {
    false nand false shouldBe true
    false nand true  shouldBe true
    true  nand false shouldBe true
    true  nand true  shouldBe false
  }
  
  "'nor' operator" should "implement logical NOR" in {
    false nor false shouldBe true
    false nor true  shouldBe false
    true  nor false shouldBe false
    true  nor true  shouldBe false
  }
  
  "'XOR' operator" should "implement logical XOR" in {
    false xor false shouldBe false
    false xor true  shouldBe true
    true  xor false shouldBe true
    true  xor true  shouldBe false
  }
  
  "'impl' operator" should "implement logical IMPL" in {
    false impl false shouldBe true
    false impl true  shouldBe true
    true  impl false shouldBe false
    true  impl true  shouldBe true
  }
  
  "'equ' operator" should "implement logical EQU" in {
    false equ false shouldBe true
    false equ true  shouldBe false
    true  equ false shouldBe false
    true  equ true  shouldBe true
  }
}