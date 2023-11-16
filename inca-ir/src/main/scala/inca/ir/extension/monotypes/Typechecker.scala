package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}

case class AddMonoInfo(monoTy: Type, termTy: Type, keysTy: Seq[Type])

trait Typechecker extends BaseIRTypechecker{

  
  private var cachedAddMonoCtx: Map[AddMono, AddMonoInfo] = Map()

  private var cachedResultMonoCtx: Map[ResultMono, TMono] = Map()

  private var cachedMkMonoCtx: Map[MkMono, Seq[Type]] = Map()

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MkMono(mono, args, keyTypes) =>
      val (in, out) = mono.monotypecheck(args) match
        case Left(msg) =>
          error(msg, term)
          (TAny, TAny)
        case Right(tm) => tm
      val argsType: Seq[Type] = args.map(a => inferTerm(a, mode).ty)
      cachedMkMonoCtx += MkMono(mono, args, keyTypes) -> argsType
      TMono(in, out, keyTypes).bound
    case ResultMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(input, output, keys) =>
          cachedResultMonoCtx += ResultMono(m) -> TMono(input, output, keys)
          output.bound
        case t =>
          error(s"Expected type of $m: TMono, actual type of $m: $t")
          TAny.bound
    case MonoAggregate(rel, args, op) => TAny.bound
    case _ => super.inferTermExtend(term, mode)

  private def recordAddMono(atom: AddMono): Unit =
    cachedAddMonoCtx += atom ->
      AddMonoInfo(
        inferTerm(atom.m, Mode.Bound).ty,
        inferTerm(atom.input, Mode.Bound).ty,
        atom.keys.map(k => inferTerm(k, Mode.Bound).ty)
      )



  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case AddMono(m, input, keys) =>
      inferTerm(m, mode).ty match
        case TMono(inType, outType, keyTypes) =>
          checkTerm(input, inType, Mode.Bound)
          recordAddMono(AddMono(m, input, keys))
          keys.zip(keyTypes) map {(k, ty) => checkTerm(k, ty, Mode.Bound)}
        case ty => error(s"Expected $m to have Mono type, but was $ty", m)
    case _ => super.checkAtom(atom, mode)
    
  def getAddMonoInfo: Map[AddMono, AddMonoInfo] = cachedAddMonoCtx

  def getResultMonoInfo: Map[ResultMono, TMono] = cachedResultMonoCtx

  def getMkMonoInfo: Map[MkMono, Seq[Type]] = cachedMkMonoCtx

}
