package io.estatico.generic.traits.macros

import scala.reflect.macros.whitebox

@macrocompat.bundle
private[traits] final class GenericTraitMacros(val c: whitebox.Context) {

  import c.universe._

  def materializeGenericTrait[T : WeakTypeTag, R: WeakTypeTag]: Tree = {
    val typ = weakTypeOf[T]
    if (!typ.typeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, "Cannot materialize non-abstract class")
    }
    val fields = collectFields(typ)
    val typeName = typ.typeSymbol.name.toTypeName
    val ReprType = makeHListType(fields.map(_._2))
    val toBody = makeHListVal(fields.map(_._1))

    q"""
      {
        new $GenericClass[$typeName] {

          override type Repr = $ReprType

          override def to(t: $typeName): Repr = $toBody

          override def from(r: Repr): $typeName = new $typeName {
            ..${makeOverrides(fields)}
          }
        }
      }: $GenericObj.Aux[$typeName, $ReprType]
    """
  }

  private def collectFields(typ: Type): List[(TermName, Type)] = {
    typ.members.sorted.collect { case m if m.isAbstract && m.typeSignature.paramLists == Nil =>
      (m.name.toTermName, m.typeSignature.resultType)
    }.toList
  }

  private def makeOverrides(fields: List[(TermName, Type)]): List[Tree] = {
    fields.zipWithIndex.map { case ((n, t), i) =>
      ValDef(Modifiers(Flag.OVERRIDE), n, tq"$t", q"r($i)")
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

  private def makeCaseClass(typ: Type): (TypeName, Tree) = {
    val name = TypeName(
      typ.typeSymbol.name.decodedName.toString + "Impl"
    )
    val caseFields = collectFields(typ).map { case (n, t) =>
      ValDef(Modifiers(Flag.CASEACCESSOR | Flag.PARAMACCESSOR), n, tq"$t", EmptyTree)
    }
    (name, q"case class $name(..$caseFields) extends $typ")
  }

  private val GenericClass = typeOf[shapeless.Generic[_]].typeSymbol.asType
  private val GenericObj = GenericClass.companion
  private val HConsClass = typeOf[shapeless.::[_, _]].typeSymbol.asType
  private val HConsObj = HConsClass.companion
  private val HNilClass = typeOf[shapeless.HNil].typeSymbol.asType
  private val HNilObj = HNilClass.companion
}
