package io.estatico.generic.traits.macros

import scala.reflect.macros.{TypecheckException, blackbox}

@macrocompat.bundle
private[traits] final class AutoCaseMacros(val c: blackbox.Context) {

  import c.universe._

  def autoCase(annottees: Tree*): Tree = annottees match {
    case List(clsDef: ClassDef) if isTraitDef(clsDef) =>
      val caseFields = collectCaseFieldsForClassDef(clsDef)
      q"""
        ${patchClass(clsDef, caseFields)}
        object ${clsDef.name.toTermName} {
          ${createApplyMethod(clsDef, caseFields)}
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

  type CaseFields = List[(TermName, Tree)]

  /** Returns name and type tree for each field eligible to be a case field. **/
  private def collectCaseFieldsForClassDef(clsDef: ClassDef): CaseFields = {
    val parentFields = removeAnyRef(clsDef.impl.parents).flatMap {
      tree => collectCaseFieldsForType(treeToType(tree))
    }
    val classFields = clsDef.impl.body.collect {
      case DefDef(_, name, Nil, Nil, tpt, EmptyTree) => (name, tpt)
    }
    parentFields ++ classFields
  }

  private def collectCaseFieldsForType(typ: Type): CaseFields = {
    typ.members.sorted.flatMap {
      case sym if sym.isMethod && sym.isAbstract =>
        val m = sym.asMethod
        if (m.paramLists != Nil || m.typeParams != Nil) None else {
          Some((m.name, tq"${m.typeSignature}"))
        }
      case _ => None
    }
  }

  private def treeToType(tree: Tree): Type = {
    try {
      c.typecheck(tq"$tree", mode=c.TYPEmode).tpe
    } catch {
      case e: TypecheckException =>
        c.abort(c.enclosingPosition, s"""
        |Unable to expand @AutoCase macro due to a type check error.
        | Possibly you are attempting to extend a trait which is defined
        | in the same class the inheriting trait is defined.
        | Type check error was: ${e.getMessage}
        """.stripMargin.trim
        )
    }
  }

  private def createApplyMethod(clsDef: ClassDef, caseFields: CaseFields) = {
    val params = caseFields.map { case (name, typ) =>
      ValDef(Modifiers(Flag.PARAM), name, typ, EmptyTree)
    }
    val withImplNames = caseFields.map { case (name, typ) =>
      (name, TermName(name.decodedName.toString + "Impl"), typ)
    }
    val fieldImpls = withImplNames.map { case (name, nameImpl, typ) =>
      ValDef(Modifiers(), nameImpl, typ, Ident(name))
    }
    val fieldOverrides = withImplNames.map { case (name, nameImpl, typ) =>
      ValDef(Modifiers(Flag.OVERRIDE), name, typ, Ident(nameImpl))
    }
    val body = q"{ ..$fieldImpls ; new ${clsDef.name} { ..$fieldOverrides } }"
    DefDef(
      Modifiers(),
      TermName("apply"),
      Nil,
      List(params),
      tq"${clsDef.name}",
      body
    )
  }

  private def patchClass(clsDef: ClassDef, caseFields: CaseFields): ClassDef = {
    List(
      extendProduct(_: ClassDef, caseFields),
      maybeOverrideToString _,
      maybeOverrideCanEqual _,
      maybeOverrideEquals _
    ).foldLeft(clsDef)((c, f) => f(c))
  }

  private def extendProduct(clsDef: ClassDef, caseFields: CaseFields): ClassDef = {
    val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
    val newParents = tq"$ProductClass" :: removeAnyRef(parents)
    val productPrefixMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productPrefix"), Nil, Nil, tq"String",
      q"${clsDef.name.decodedName.toString}"
    )
    val productArityMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productArity"), Nil, Nil, tq"Int",
      q"${caseFields.length}"
    )
    val productIteratorMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productIterator"), Nil, Nil,
      tq"Iterator[Any]",
      q"Iterator(..${caseFields.map(_._1)})"
    )
    val productElementConds = caseFields.zipWithIndex.map { case ((field, _), i) =>
      q"if (n == $i) return $field"
    }
    val productElementMethod = DefDef(
      Modifiers(Flag.OVERRIDE), TermName("productElement"), Nil,
      List(List(ValDef(Modifiers(Flag.PARAM), TermName("n"), tq"Int", EmptyTree))),
      tq"Any",
      q"""
        ..$productElementConds
        throw new IndexOutOfBoundsException(n.toString)
      """
    )
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
              productArity == x.productArity &&
                productIterator.zip(x.productIterator).forall {
                  case (a, b) => a == b
                }
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
    if (!shouldOverrideEquals(clsDef)) clsDef else {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = clsDef
      val equalsMethod = DefDef(
        Modifiers(Flag.OVERRIDE), TermName("canEqual"), Nil,
        List(List(ValDef(Modifiers(Flag.PARAM), TermName("o"), tq"Any", EmptyTree))),
        tq"Boolean",
        q"""
          o match {
            case x: ${clsDef.name} => productArity == x.productArity
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

  private val ProductClass = typeOf[Product].typeSymbol.asType
}
