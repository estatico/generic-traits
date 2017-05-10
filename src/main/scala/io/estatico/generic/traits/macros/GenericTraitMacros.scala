package io.estatico.generic.traits.macros

import shapeless.{CaseClassMacros, SingletonTypeUtils}

import scala.collection.mutable
import scala.reflect.macros.whitebox

@macrocompat.bundle
private[traits] final class GenericTraitMacros(val c: whitebox.Context)
  extends SingletonTypeUtils with CaseClassMacros {

  import c.universe._

  def materializeGenericTrait[T : WeakTypeTag, R : WeakTypeTag]: Tree = {
    val typ = weakTypeOf[T]
    if (!typ.typeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, "Cannot materialize non-abstract class")
    }
    val fields = collectFields(typ)
    val ReprType = makeHListType(fields.map(_._2))
    val toBody = makeHListVal(fields.map(_._1))

    q"""
      new $GenericClass[$typ] {

        override type Repr = $ReprType

        override def to(t: $typ): Repr = $toBody

        override def from(r: Repr): $typ = new $typ {
          ..${makeOverrides(fields)}
        }
      }: $GenericObj.Aux[$typ, $ReprType]
    """
  }

  def materializeDefaultSymbolicLabelling[T : WeakTypeTag]: Tree = {
    val typ = weakTypeOf[T]
    if (!typ.typeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, "Cannot materialize non-abstract class")
    }
    val fields = collectFields(typ)
    val labels: List[String] = fields.map(f => f._1.decodedName.toString)
    val labelTypes = labels.map(SingletonSymbolType(_))
    val labelValues = labels.map(mkSingletonSymbol)

    val labelsType = mkHListTpe(labelTypes)
    val labelsValue = labelValues.foldRight(q"$HNilObj")(
      (x, acc) => q"$HConsObj($x, $acc)"
    )

    q"""
      new $DefaultSymbolicLabellingClass[$typ] {
        type Out = $labelsType
        def apply(): $labelsType = $labelsValue
      }: $DefaultSymbolicLabellingObj.Aux[$typ, $labelsType]
    """
  }

  private def collectFields(typ: Type): List[(TermName, Type)] = {
    // Retain order of fields as they are collected.
    val fields = new mutable.LinkedHashMap[TermName, Type]
    typ.baseClasses.reverse.foreach { cls =>
      cls.asType.toType.members.sorted.foreach { s =>
        val isEligible = (
          s.isAbstract
          && s.isMethod
          && s.asMethod.paramLists == Nil
          && s.asMethod.typeParams == Nil
        )
        if (isEligible) {
          fields += (s.name.toTermName -> s.typeSignatureIn(typ).resultType)
        } else {
          fields -= s.name.toTermName
        }
      }
    }
    fields.toList
  }

  private def makeOverrides(fields: List[(TermName, Type)]): List[Tree] = {
    fields.zipWithIndex.map { case ((n, t), i) =>
      DefDef(Modifiers(), n, Nil, Nil, tq"$t", q"r($i)")
    }
  }

  private def makeHListType(types: List[Type]): AppliedTypeTree = types match {
    case Nil => AppliedTypeTree(tq"$HNilClass", Nil)
    case t :: ts => AppliedTypeTree(tq"$HConsClass", List(tq"$t", makeHListType(ts)))
  }

  private def makeHListVal(names: List[TermName]): Tree = names match {
    case Nil => q"$HNilObj"
    case n :: ns => q"$HConsObj(t.$n, ${makeHListVal(ns)})"
  }

  private val DefaultSymbolicLabellingClass = typeSym[shapeless.DefaultSymbolicLabelling[_]]
  private val DefaultSymbolicLabellingObj = DefaultSymbolicLabellingClass.companion
  private val GenericClass = typeSym[shapeless.Generic[_]]
  private val GenericObj = GenericClass.companion
  private val HConsClass = typeSym[shapeless.::[_, _]]
  private val HConsObj = HConsClass.companion
  private val HNilClass = typeSym[shapeless.HNil]
  private val HNilObj = HNilClass.companion

  private def typeSym[A : TypeTag] = typeOf[A].typeSymbol.asType
}
