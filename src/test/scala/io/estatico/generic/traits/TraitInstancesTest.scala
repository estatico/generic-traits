package io.estatico.generic.traits

import org.scalatest.{FlatSpec, Matchers}
import shapeless._

class TraitInstancesTest extends FlatSpec with Matchers {

  import TraitInstancesTest._

  "materializeGenericTrait" should "create a valid Generic instance" in {
    val g = Generic[Foo]
    g.to(foo) shouldEqual fooHList
    val from = Generic[Foo].from(fooHList)
    (from.bar, from.baz) shouldEqual (foo.bar, foo.baz)
  }

  it should "create a valid DefaultSymbolicLabelling instance" in {
    val sl = DefaultSymbolicLabelling[Foo]
    sl() shouldEqual fooSymbols
  }

  it should "automatically get a LabelledGeneric instance" in {
    val g = LabelledGeneric[Foo]
    val repr = g.to(foo)
    repr shouldEqual fooHList
    val from = g.from(repr)
    (from.bar, from.baz) shouldEqual (foo.bar, foo.baz)
  }

//  "materializeGenericTrait" should "handle trait inheritance" in {
//    trait Foo {
//      def bar: String
//      def baz: Int
//    }
//
//
//    val g = Generic[Quux]
//
//    val hlist = quux.bar :: quux.baz :: quux.spam :: HNil
//    g.to(quux) shouldEqual hlist
//
//    val from = Generic[Quux].from(hlist)
//    (from.bar, from.baz, from.spam) shouldEqual (quux.bar, quux.baz, quux.spam)
//  }
}

object TraitInstancesTest {

  trait Foo {
    def bar: String
    def baz: Int
  }

  val foo = new Foo {
    val bar = "a"
    val baz = 2
  }

  val fooHList = foo.bar :: foo.baz :: HNil

  val fooSymbols = 'bar :: 'baz :: HNil

  trait Quux extends Foo {
    def spam: Float
  }

  val quux = new Quux {
    val bar = "b"
    val baz = 3
    val spam = 1.2f
  }

}
