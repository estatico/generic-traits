package io.estatico.generic.traits.macros

import shapeless.HList

/** Meta-info for handling case traits. */
trait CaseTrait {
  protected def toHList: HList
  protected def getFullClassName: String
}
