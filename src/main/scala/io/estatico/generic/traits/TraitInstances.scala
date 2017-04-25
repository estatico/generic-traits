package io.estatico.generic.traits

import io.estatico.generic.traits.macros.{GenericTraitMacros => M}
import shapeless.Generic

trait TraitInstances {
  implicit def materializeGenericTrait[T, R]: Generic.Aux[T, R] = macro M.materializeGenericTrait[T, R]
}

