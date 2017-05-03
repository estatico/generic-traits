# Generic Traits

[![Build Status](https://travis-ci.org/estatico/generic-traits.svg?branch=master)](https://travis-ci.org/estatico/generic-traits)
[![Gitter](https://img.shields.io/badge/gitter-join%20chat-green.svg)](https://gitter.im/estatico/generic-traits)
[![Maven Central](https://img.shields.io/maven-central/v/io.estatico/generic-traits_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/io.estatico/confide-core_2.12)

Automatic generic derivation for traits

## Usage

For traits which are not coproducts (e.g. not part of a sealed trait/case class family),
this library allows you to derive [shapeless](https://github.com/milessabin/shapeless)
`Generic` and `LabelledGeneric` instances from accessor fields. These instances allow
us to operate on our traits generically; for instance, we can automatically derive type class
instances for them.

Simply importing `io.estatico.generic.traits._` will bring in the implicit macro used to
derive instances. See the example usage below.

```scala
scala> :paste
// Entering paste mode (ctrl-D to finish)

import io.estatico.generic.traits._

trait Foo {
  def bar: String
  def baz: Int
}

// Exiting paste mode, now interpreting.

scala> shapeless.Generic[Foo]
res0: shapeless.Generic[Foo]{type Repr = shapeless.::[String,shapeless.::[Int,shapeless.HNil]]} = $anon$1@38245121

scala> val foo = new Foo { val bar = "a" ; val baz = 1 }
foo: Foo{val bar: String; val baz: Int} = $anon$1@1f0ed694

scala> val repr = shapeless.Generic[Foo].to(foo)
repr: shapeless.::[String,shapeless.::[Int,shapeless.HNil]] = a :: 1 :: HNil

scala> val foo = shapeless.Generic[Foo].from(repr)
foo: Foo = $anon$1$$anon$2@10423297

scala> (foo.bar, foo.baz)
res1: String = (a,1)

scala> shapeless.LabelledGeneric[Foo]
res2: shapeless.LabelledGeneric[Foo]{type Repr = shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("bar")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged
[String("baz")],Int],shapeless.HNil]]} = shapeless.LabelledGeneric$$anon$1@1d3983be

scala> val repr = shapeless.LabelledGeneric[Foo].to(foo)
repr: shapeless.::[String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("bar")],String],shapeless.::[Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("baz")],Int],shapeless.HNil]] = a :
: 1 :: HNil

scala> val foo = shapeless.LabelledGeneric[Foo].from(repr)
foo: Foo = $anon$2$$anon$3@2a03c5d

scala> (res1.bar, res1.baz)
res3: (String, Int) = (a,1)
```
