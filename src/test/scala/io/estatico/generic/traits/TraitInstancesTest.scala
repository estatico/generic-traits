package io.estatico.generic.traits

import org.scalatest.{FlatSpec, Matchers}
import shapeless._

class TraitInstancesTest extends FlatSpec with Matchers {

  "materializeGenericTrait" should "create a valid Generic instance" in {
    trait Foo {
      def bar: String
      def baz: Int
    }

    val foo = new Foo {
      val bar = "a"
      val baz = 2
    }

    val g = Generic[Foo]

    val hlist = foo.bar :: foo.baz :: HNil
    g.to(foo) shouldEqual hlist

    val from = Generic[Foo].from(hlist)
    (from.bar, from.baz) shouldEqual (foo.bar, foo.baz)
  }

//  "materializeGenericTrait" should "handle trait inheritance" in {
//    trait Foo {
//      def bar: String
//      def baz: Int
//    }
//    trait Quux extends Foo {
//      def spam: Foo
//      def eggs: Float
//    }
//
//    val quux = new Quux {
//      val bar = "b"
//      val baz = 3
//      val spam = new Foo {
//        val bar = "c"
//        val baz = 4
//      }
//      val eggs = 1.2f
//    }
//
//    val g = Generic[Quux]
//
//    val hlist = quux.bar :: quux.baz :: (quux.spam.bar :: quux.spam.baz :: HNil) :: quux.eggs :: HNil
//    g.to(quux) shouldEqual hlist
//
//    val from = Generic[Quux].from(hlist)
//    from.bar shouldEqual quux.bar
//    (from.bar, from.baz, (from.spam.bar, from.spam.baz), from.eggs) shouldEqual (quux.bar)
//  }
}
