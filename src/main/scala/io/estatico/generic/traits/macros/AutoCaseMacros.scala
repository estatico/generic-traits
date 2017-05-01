package io.estatico.generic.traits.macros

import scala.reflect.macros.blackbox

@macrocompat.bundle
private[traits] final class AutoCaseMacros(val c: blackbox.Context) {

  import c.universe._

  def autoCase(annottees: Tree*): Tree = annottees match {
    case List(clsDef: ClassDef) if isTraitDef(clsDef) =>
      q"""
        ${patchClass(clsDef)}
        object ${clsDef.name.toTermName} {
          ${createApplyMethod(clsDef)}
          ${createUnapplyMethod(clsDef)}
        }
      """

    case List(
      clsDef: ClassDef,
      q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
    ) if isTraitDef(clsDef) =>
      println(showRaw(clsDef))
      ???

    case _ => c.abort(c.enclosingPosition, s"Only traits are supported.")
  }

  private def isTraitDef(clsDef: ClassDef) = clsDef.mods.hasFlag(Flag.TRAIT)

  private def createApplyMethod(clsDef: ClassDef) = {
    q"""
      def apply[T, R](t: T)(
        implicit
        tg: $GenericObj.Aux[T, R],
        fg: $GenericObj.Aux[${clsDef.name}, R]
      ): ${clsDef.name} = fg.from(tg.to(t))
    """
  }

  private def createUnapplyMethod(clsDef: ClassDef) = {
    q"def unapply(x: ${clsDef.name}) = Some($GenericObj[${clsDef.name}].to(x).tupled)"
  }

  private def patchClass(clsDef: ClassDef): ClassDef = {
    List(
      extendCaseTrait _,
      extendProduct _,
      maybeOverrideToString _,
      maybeOverrideCanEqual _,
      maybeOverrideEquals _
    ).foldLeft(clsDef)((c, f) => f(c))
  }

  private def extendCaseTrait(clsDef: ClassDef): ClassDef = {
    val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
    val toHListMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("toHList"), Nil, Nil, tq"$HListClass",
      q"$GenericObj[${clsDef.name}].to(this)"
    )
    val getFullClassNameMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("getFullClassName"), Nil, Nil, tq"String",
      q"${c.internal.enclosingOwner.fullName + '.' + clsDef.name.decodedName.toString}"
    )
    val newParents = tq"$CaseTraitClass" :: removeAnyRef(parents)
    val newBody = (
      toHListMethod
      :: getFullClassNameMethod
      :: body
    )
    ClassDef(mods, name, tparams, Template(newParents, self, newBody))
  }

  private def extendProduct(clsDef: ClassDef): ClassDef = {
    val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
    val productPrefixMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productPrefix"), Nil, Nil, tq"String",
      q"${clsDef.name.decodedName.toString}"
    )
    val productArityMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productArity"), Nil, Nil, tq"Int",
      q"$GenericObj[$name].to(this).to[List].length"
    )
    val productIteratorMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productIterator"), Nil, Nil,
      tq"Iterator[Any]",
      q"$GenericObj[$name].to(this).to[Iterator]"
    )
    val productElementMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productElement"), Nil,
      List(List(ValDef(Modifiers(Flag.PARAM), TermName("n"), tq"Int", EmptyTree))),
      tq"Any",
      q"$GenericObj[$name].to(this).to[Stream].apply(n)"
    )
    val newParents = tq"$ProductClass" :: removeAnyRef(parents)
    val newBody = (
      productPrefixMethod
      :: productArityMethod
      :: productIteratorMethod
      :: productElementMethod
      :: body
    )
    ClassDef(mods, name, tparams, Template(newParents, self, newBody))
  }

  private def removeAnyRef(parents: List[Tree]): List[Tree] = {
    parents.filter {
      case tq"scala.AnyRef" => false
      case _ => true
    }
  }

  private def maybeOverrideToString(clsDef: ClassDef): ClassDef = {
    if (!shouldOverrideToString(clsDef)) clsDef else {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
      val toStringMethod = DefDef(
        Modifiers(Flag.OVERRIDE), TermName("toString"), Nil, Nil, tq"String",
        q""" productPrefix + '(' + productIterator.mkString(",") + ')' """
      )
      val newBody = toStringMethod :: body
      ClassDef(mods, name, tparams, Template(parents, self, newBody))
    }
  }

  private def shouldOverrideToString(clsDef: ClassDef): Boolean = {
    clsDef.impl.body.collectFirst {
      case DefDef(_, TermName("toString"), Nil, Nil, _, tree) if tree != EmptyTree => false
    }.getOrElse(true)
  }

  private def maybeOverrideEquals(clsDef: ClassDef): ClassDef = {
    if (!shouldOverrideEquals(clsDef)) clsDef else {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
      val equalsMethod = DefDef(
        Modifiers(Flag.OVERRIDE), TermName("equals"), Nil,
        List(List(ValDef(Modifiers(Flag.PARAM), TermName("o"), tq"Any", EmptyTree))),
        tq"Boolean",
        q"""
          o match {
            case x: ${clsDef.name} =>
              canEqual(x) && toHList == x.toHList
            case _ => false
        }
        """
      )
      val newBody = equalsMethod :: body
      ClassDef(mods, name, tparams, Template(parents, self, newBody))
    }
  }

  private def shouldOverrideEquals(clsDef: ClassDef): Boolean = {
    clsDef.impl.body.collectFirst {
      case DefDef(_, TermName("equals"), Nil, List(List(_)), _, tree) if tree != EmptyTree => false
    }.getOrElse(true)
  }

  private def maybeOverrideCanEqual(clsDef: ClassDef): ClassDef = {
    if (!shouldOverrideCanEqual(clsDef)) clsDef else {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
      val equalsMethod = DefDef(
        Modifiers(Flag.OVERRIDE), TermName("canEqual"), Nil,
        List(List(ValDef(Modifiers(Flag.PARAM), TermName("o"), tq"Any", EmptyTree))),
        tq"Boolean",
        q"""
          o match {
            case x: ${clsDef.name} => getFullClassName == x.getFullClassName
            case _ => false
        }
        """
      )
      val newBody = equalsMethod :: body
      ClassDef(mods, name, tparams, Template(parents, self, newBody))
    }
  }

  private def shouldOverrideCanEqual(clsDef: ClassDef): Boolean = {
    clsDef.impl.body.collectFirst {
      case DefDef(_, TermName("canEqual"), Nil, List(List(_)), _, tree) if tree != EmptyTree => false
    }.getOrElse(true)
  }

  private val CaseTraitClass = typeSym[CaseTrait]
  private val ProductClass = typeSym[Product]
  private val GenericClass = typeSym[shapeless.Generic[_]]
  private val GenericObj = GenericClass.companion
  private val HListClass = typeSym[shapeless.HList]

  private def typeSym[A : TypeTag] = typeOf[A].typeSymbol.asType
}
