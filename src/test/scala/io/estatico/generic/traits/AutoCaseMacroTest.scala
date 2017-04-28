package io.estatico.generic.traits

import org.scalatest.{FlatSpec, Matchers}

@AutoCase trait Foo {
  def bar: String
  def baz: Int
}

@AutoCase trait Spam extends Foo {
  def foo: Foo
}

class AutoCaseMacroTest extends FlatSpec with Matchers {

  val applyFoo: Foo = Foo("a", 2)

  val newFoo = new Foo {
    val bar = "a"
    val baz = 2
  }

  val applySpam: Spam = Spam("a", 2, applyFoo)

  val newSpam = new Spam {
    val bar = "a"
    val baz = 2
    val foo = newFoo
  }

  "@AutoCase" should "make traits equate like case classes" in {
    applyFoo shouldEqual newFoo
  }

  it should "make traits have a toString like case classes" in {
    val expected = """Foo(a,2)"""
    applyFoo.toString shouldEqual expected
    newFoo.toString shouldEqual expected
  }

  it should "make extended traits equate like case classes" in {
    for {
      spam <- List(applySpam, newSpam)
      foo <- List(applyFoo, newFoo)
    } spam shouldNot equal(foo)
  }

  it should "make extended traits have a toString like case classes" in {
    val expected = """Spam(a,2,Foo(a,2))"""
    newSpam.toString shouldEqual expected
  }
}
