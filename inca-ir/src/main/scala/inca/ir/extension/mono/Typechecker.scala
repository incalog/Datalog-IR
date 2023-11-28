package inca.ir.extension.mono

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}

case class AddMonoInfo(monoTy: Type, termTy: Type, keysTy: Seq[Type])

trait Typechecker extends BaseIRTypechecker{

  
  private var cachedAddMonoCtx: Map[WriteMono, AddMonoInfo] = Map()

  private var cachedResultMonoCtx: Map[ReadMono, TMono] = Map()


  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case NewMono(mono, keys, args) =>
      val tys = args.map(inferTerm(_, Mode.Bound).ty)
      if (mono.args.size != args.size)
        error(s"Expected ${mono.args.size} arguments, but got $args", term)
      args.zip(mono.args).foreach((a,ty) => checkTerm(a, ty, Mode.Bound))
      mono.monoType(keys).bound
    case ReadMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(input, output, keys) =>
          cachedResultMonoCtx += ReadMono(m) -> TMono(input, output, keys)
          output.bound
        case ty =>
          error(s"Expected mono type but got $ty", m)
          TAny.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case WriteMono(m, input, keys) =>
      inferTerm(m, mode).ty match
        case TMono(inType, outType, keyTypes) =>
          checkTerm(input, inType, Mode.Bound)
          if (keys.size != keyTypes.size)
            error(s"Expected ${keyTypes.size} keys, but got ${keys.size}", atom)
          keys.zip(keyTypes) map {(k, ty) => checkTerm(k, ty, Mode.Bound)}
          recordAddMono(WriteMono(m, input, keys))
        case ty => error(s"Expected mono type but got $ty", m)
    case _ => super.checkAtom(atom, mode)

  private def recordAddMono(atom: WriteMono): Unit =
    cachedAddMonoCtx += atom ->
      AddMonoInfo(
        inferTerm(atom.m, Mode.Bound).ty,
        inferTerm(atom.input, Mode.Bound).ty,
        atom.keys.map(k => inferTerm(k, Mode.Bound).ty)
      )

  def getAddMonoInfo: Map[WriteMono, AddMonoInfo] = cachedAddMonoCtx

  def getResultMonoInfo: Map[ReadMono, TMono] = cachedResultMonoCtx


}
