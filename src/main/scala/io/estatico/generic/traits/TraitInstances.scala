package io.estatico.generic.traits

import io.estatico.generic.traits.macros.{GenericTraitMacros => M}
import shapeless.{DefaultSymbolicLabelling, Generic}

trait TraitInstances {

  implicit def materializeGenericTrait[T, R]: Generic.Aux[T, R] =
    macro M.materializeGenericTrait[T, R]

  implicit def materializeDefaultSymbolicLabellingTrait[T]: DefaultSymbolicLabelling[T] =
    macro M.materializeDefaultSymbolicLabelling[T]
}
