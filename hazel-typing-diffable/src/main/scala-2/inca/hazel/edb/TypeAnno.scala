package inca.hazel.edb

import truediff.Diffable
import truediff.macros.diffable

@diffable sealed trait TypeAnno extends Diffable

@diffable case class TAUnknown() extends TypeAnno

@diffable case class TANum() extends TypeAnno

@diffable case class TABool() extends TypeAnno

@diffable case class TAArrow(dom: TypeAnno, codom: TypeAnno) extends TypeAnno

@diffable case class TAProd(fst: TypeAnno, snd: TypeAnno) extends TypeAnno
