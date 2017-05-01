package io.estatico.generic.traits

import io.estatico.generic.traits.macros.{AutoCaseMacros => M}

import scala.annotation.StaticAnnotation

class AutoCase extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro M.autoCase
}
