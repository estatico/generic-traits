package io.estatico.generic.traits

import org.scalatest.{FlatSpec, Matchers}
import shapeless._

class TraitInstancesTest extends FlatSpec with Matchers {

  import TraitInstancesTest._

  "TraitInstances" should "create a valid Generic instance" in {
    val g = Generic[Foo]
    g.to(foo) shouldEqual fooHList
    val from = g.from(fooHList)
    (from.bar, from.baz) shouldEqual (foo.bar, foo.baz)
  }

  it should "create Generic for single inheritance" in {
    val g = Generic[Quux]
    g.to(quux) shouldEqual quuxHList
    val from = g.from(quuxHList)
    (from.bar, from.baz, from.spam) shouldEqual (quux.bar, quux.baz, quux.spam)
  }

  it should "create Generic for multiple inheritance" in {
    val g = Generic[OneTwoThree]
    g.to(ott) shouldEqual ottHList
    val from = g.from(ottHList)
    (from.one, from.two, from.three) shouldEqual (ott.one, ott.two, ott.three)
  }

  it should "create Generic for trait with type params" in {
    val g = Generic[Params[String, Int]]
    g.to(params) shouldEqual paramsHList
    val from = g.from(paramsHList)
    (from.a, from.b) shouldEqual (params.a, params.b)
  }

  it should "create a valid DefaultSymbolicLabelling instance" in {
    DefaultSymbolicLabelling[Foo].apply() shouldEqual fooSymbols
  }

  it should "create DefaultSymbolicLabelling for single inheritance" in {
    DefaultSymbolicLabelling[Quux].apply() shouldEqual quuxSymbols
  }

  it should "create DefaultSymbolicLabelling for multiple inheritance" in {
    DefaultSymbolicLabelling[OneTwoThree].apply() shouldEqual ottSymbols
  }

  it should "create DefaultSymbolicLabelling for trait with type params" in {
    DefaultSymbolicLabelling[Params[_, _]].apply() shouldEqual paramsSymbols
  }

  it should "get LabelledGeneric" in {
    val g = LabelledGeneric[Foo]
    val repr = g.to(foo)
    repr shouldEqual fooHList
    val from = g.from(repr)
    (from.bar, from.baz) shouldEqual(foo.bar, foo.baz)
  }
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

  val quuxHList = quux.bar :: quux.baz :: quux.spam :: HNil

  val quuxSymbols = 'bar :: 'baz :: 'spam :: HNil

  trait One {
    def one: Int
  }

  trait Two {
    def two: Int
  }

  trait Three {
    def three: Int
  }

  trait OneTwoThree extends One with Two with Three

  val ott = new OneTwoThree {
    val one = 1
    val two = 2
    val three = 3
  }

  val ottHList = ott.one :: ott.two :: ott.three :: HNil

  val ottSymbols = 'one :: 'two :: 'three :: HNil

  trait Params[A, B] {
    def a: A
    def b: B
  }

  val params = new Params[String, Int] {
    val a = "c"
    val b = 42
  }

  val paramsHList = params.a :: params.b :: HNil

  val paramsSymbols = 'a :: 'b :: HNil
}
