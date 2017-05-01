package io.estatico.generic.traits

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Gen

class AutoCaseMacroTest extends FlatSpec with Matchers with PropertyChecks {

  import AutoCaseMacroTest._

  "@AutoCase" should "generate equals" in {
    newFoo shouldEqual applyFoo
    newSpam shouldEqual applySpam
    // Even though these spams and foos inherit from Foo and have the same
    // field values, they should NOT be equal since spam is a different type
    // and has additional fields, even if they are compared as Foos.
    for {
      spam <- List[Foo](applySpam, newSpam)
      foo <- List[Foo](applyFoo, newFoo)
    } spam shouldNot equal(foo)
  }

  it should "generate productPrefix" in {
    applyFoo.productPrefix shouldEqual "Foo"
    newFoo.productPrefix shouldEqual "Foo"
    applySpam.productPrefix shouldEqual "Spam"
    newSpam.productPrefix shouldEqual "Spam"
  }

  it should "generate productArity" in {
    applyFoo.productArity shouldEqual 2
    newFoo.productArity shouldEqual 2
    applySpam.productArity shouldEqual 3
    newSpam.productArity shouldEqual 3
  }

  it should "generate productIterator" in {
    forAll(Gen.oneOf(applyFoo, newFoo)) { foo: Foo =>
      foo.productIterator.toList shouldEqual List(foo.bar, foo.baz)
    }
    forAll(Gen.oneOf(applySpam, applySpam)) { spam: Spam =>
      spam.productIterator.toList shouldEqual List(spam.bar, spam.baz, spam.foo)
    }
  }

  it should "generate productElement" in {
    forAll(Gen.oneOf(applyFoo, newFoo)) { foo: Foo =>
      List(0, 1).map(foo.productElement) shouldEqual List(foo.bar, foo.baz)
    }
    forAll(Gen.oneOf(applySpam, applySpam)) { spam: Spam =>
      List(0, 1, 2).map(spam.productElement) shouldEqual List(spam.bar, spam.baz, spam.foo)
    }
  }

  it should "generate toString" in {
    val fooString = """Foo(a,2)"""
    applyFoo.toString shouldEqual fooString
    newFoo.toString shouldEqual fooString
    val spamString = """Spam(a,2,Foo(a,2))"""
    newSpam.toString shouldEqual spamString
    applySpam.toString shouldEqual spamString
  }

  it should "generate apply" in {
    Foo("a", 2) shouldEqual Foo("a", 2)
    Spam("a", 2, Foo("b", 3)) shouldEqual Spam("a", 2, Foo("b", 3))
  }

  it should "generate unapply" in {
    Foo("a", 2) match {
      case Foo(x, y) => (x, y) shouldEqual ("a", 2)
    }
    Spam("a", 2, Foo("b", 3)) match {
      case Spam(w, x, Foo(y, z)) => (w, x, y, z) shouldEqual ("a", 2, "b", 3)
    }
  }
}

object AutoCaseMacroTest {

  @AutoCase trait Foo {
    def bar: String
    def baz: Int
  }

  @AutoCase trait Spam extends Foo {
    def foo: Foo
  }

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
}
