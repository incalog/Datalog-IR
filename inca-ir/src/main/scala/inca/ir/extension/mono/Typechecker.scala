package inca.ir.extension.mono

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}

case class AddMonoInfo(monoTy: Type, termTy: Type, keysTy: Seq[Type])

trait Typechecker extends BaseIRTypechecker {
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case NewMono(mono, keys, args) =>
      val tys = args.map(inferTerm(_, Mode.Bound).ty)
      if (mono.constructorParamTypes.size != args.size)
        error(s"Expected ${mono.constructorParamTypes.size} arguments, but got $args", term)
      args.zip(mono.constructorParamTypes).foreach((a, ty) => checkTerm(a, ty, Mode.Bound))
      mono.monoType(keys).bound
    case NewMonoFor(mono, keys, args, uniqueFor) =>
      val tys = args.map(inferTerm(_, Mode.Bound).ty)
      uniqueFor.foreach(inferTerm(_, Mode.Bound))

      if (mono.constructorParamTypes.size != args.size)
        error(s"Expected ${mono.constructorParamTypes.size} arguments, but got $args", term)
      args.zip(mono.constructorParamTypes).foreach((a, ty) => checkTerm(a, ty, Mode.Bound))
      mono.monoType(keys).bound
    case ReadMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(input, output, keys) => output.bound
        case ty =>
          error(s"Expected mono type but got $ty", m)
          TAny.bound
    case _ => super.inferTermExtend(term, mode)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case WriteMono(m, input, keys) =>
      inferTerm(m, mode).ty match
        case TMono(inType, outType, keyTypes) =>
          checkTerm(input, inType, Mode.Bound)
          if (keys.size != keyTypes.size)
            error(s"Expected ${keyTypes.size} keys, but got ${keys.size}", atom)
          keys.zip(keyTypes) map { (k, ty) => checkTerm(k, ty, Mode.Bound) }
        case ty => error(s"Expected mono type but got $ty", m)
    case _ => super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TMono(ity, oty, ktys) =>
      checkType(ity)
      checkType(oty)
      ktys.foreach(checkType)
    case _ => super.checkType(ty)
}
