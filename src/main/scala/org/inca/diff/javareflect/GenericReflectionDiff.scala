package org.inca.diff.javareflect

import java.lang.reflect.Field
import java.security.MessageDigest

import org.inca.diff.WithCachedCryptoHash

import GenericReflectionCryptoHashOracle.digest

object GenericReflectionDiff {

  // marker trait for types that should be structurally diffed
  trait StructuralDiff

  implicit def withClassOps[A](cls: Class[A]): ClassOps[A] = new ClassOps(cls)
  class ClassOps[A](val cls: Class[A]) extends AnyVal {
    def allFields: Seq[Field] = {
      ClassOps.allFieldsCache.get(cls) match {
        case Some(fields) => fields
        case None =>
          var fields = cls.getDeclaredFields.toSeq.filter(!_.getName.contains('$'))
          val sup = cls.getSuperclass
          if (sup != null)
            fields ++= sup.allFields
          for (ifc <- cls.getInterfaces)
            fields ++= ifc.allFields
          ClassOps.allFieldsCache += cls -> fields
          fields
      }
    }
    def allFieldVals(n: Any): Seq[AnyRef] = allFields.map{ fld =>
      fld.setAccessible(true)
      val value = fld.get(n)
      value
    }
  }
  object ClassOps {
    private var allFieldsCache: Map[Class[_], Seq[Field]] = Map()
  }
  implicit def withFieldOps(fld: Field): FieldOps = new FieldOps(fld)
  class FieldOps(val fld: Field) extends AnyVal {

  }


  trait Tree extends WithCachedCryptoHash
  case class Val(v: Any) extends Tree {
    override val $hash: Array[Byte] = {
      digest.update(v.getClass.getCanonicalName.getBytes())
      v match {
        case v: Boolean => digest.update(if(v) 1:Byte else 0:Byte); digest.digest()
        case v: Byte => digest.update(v); digest.digest()
        case v: Short  => digest.digest(BigInt(v).toByteArray)
        case v: Char  => digest.digest(BigInt(v).toByteArray)
        case v: Int => digest.digest(BigInt(v).toByteArray)
        case v: Long => digest.digest(BigInt(v).toByteArray)
        case v: Float => digest.digest(BigInt(java.lang.Float.floatToRawIntBits(v)).toByteArray)
        case v: Double => digest.digest(BigInt(java.lang.Double.doubleToRawLongBits(v)).toByteArray)
        case v: String => digest.digest(v.getBytes)
        case v: Symbol => digest.digest(v.name.getBytes)
        case _ => throw new IllegalArgumentException(s"Cannot compute hash of $v")
      }
    }
  }
  case class Node(cls: Class[_], subs: Seq[Tree]) extends Tree {
    override val $hash: Array[Byte] = {
      digest.update(cls.getCanonicalName.getBytes)
      subs.foreach(t => digest.update(t.$hash))
      digest.digest()
    }
  }

  def decorate(n: Any): Tree =
    if (n == null)
      Val(n)
    else if (n.isInstanceOf[Seq[_]])
      Node(classOf[Seq[_]], n.asInstanceOf[Seq[_]].map(decorate))
    else if (n.isInstanceOf[StructuralDiff])
      Node(n.getClass, n.getClass.allFieldVals(n).map(decorate(_)))
    else
      Val(n)


  class MetaVar(val i: Int) extends Plug {
    override val freevars: Set[MetaVar] = Set(this)

    override def toString: String = i.toString
    override def equals(obj: Any): Boolean = obj.isInstanceOf[MetaVar] && i==obj.asInstanceOf[MetaVar].i
    override def hashCode(): Int = i
  }

  trait Plug {
    val freevars: Set[MetaVar]
  }

  trait TreeC[A] {
    val freevars: Set[MetaVar]
    def isClosed: Boolean = freevars.isEmpty
  }
  case class Hole[A <: Plug](a: A) extends TreeC[A] {
    override val freevars: Set[MetaVar] = a.freevars
  }
  case class ValC[A <: Plug](v: Any) extends TreeC[A] {
    override val freevars: Set[MetaVar] = Set()
  }
  case class NodeC[A <: Plug](cls: Class[_], subs: Seq[TreeC[A]]) extends TreeC[A] {
    override val freevars: Set[MetaVar] = subs.foldLeft(Set[MetaVar]())(_ union _.freevars)
  }

  def asCtx[A <: Plug](t: Tree): TreeC[A] = t match {
    case Val(v) => ValC(v)
    case Node(cls, subs) => NodeC(cls, subs.map(asCtx(_)))
  }

  def retainHoles[A <: Plug](tc: TreeC[A], vs: Set[A], t: Tree): TreeC[A] = tc match {
    case Hole(a) => if (vs.contains(a)) tc else asCtx(t)
    case ValC(_) => tc
    case NodeC(cls, subs) => {
      val newsubs = (subs zip t.asInstanceOf[Node].subs).map ( tt => retainHoles(tt._1, vs, tt._2))
      NodeC(cls, newsubs)
    }
  }

  case class Change[A <: Plug](delCtx: TreeC[A], insCtx: TreeC[A]) extends Plug {
    override val freevars: Set[MetaVar] = insCtx.freevars diff delCtx.freevars
    def isClosed: Boolean = freevars.isEmpty
  }

  def changeTree(src: Tree, dest: Tree, oracle: GenericReflectionOracle): Change[MetaVar] = {
    val change = Change(extract(oracle, src), extract(oracle, dest))
    postprocess(src, dest, change)
  }

  def extract(oracle: GenericReflectionOracle, t: Tree): TreeC[MetaVar] = oracle.predict(t) match {
    case Some(i) => Hole(i)
    case None => t match {
      case Val(v) => ValC(v)
      case Node(cls, subs) => NodeC(cls, subs.map(extract(oracle, _)))
    }
  }

  def postprocess(src: Tree, dest: Tree, c: Change[MetaVar]): Change[MetaVar] = {
    val okvars = c.delCtx.freevars intersect c.insCtx.freevars
    val postDel = retainHoles(c.delCtx, okvars, src)
    val postIns = retainHoles(c.insCtx, okvars, dest)
    Change(postDel, postIns)
  }


  type Patch = TreeC[Change[MetaVar]]
  def isEmptyPatch(p: Patch): Boolean = p match {
    case Hole(Change(Hole(i), Hole(j))) => i ==j
    case _ => false
  }

  type Prefix[A <: Plug] = Either[TreeC[Change[A]], Change[A]]

  def mkPrefix[A <: Plug](change: Change[A]): Prefix[A] =
    if (change.isClosed)
      Left(Hole(change))
    else
      Right(change)

  def greatestCommonClosedPrefix[A <: Plug](t1: TreeC[A], t2: TreeC[A]): Prefix[A] = (t1, t2) match {
    case (ValC(v1), ValC(v2)) if v1 == v2 => Left(ValC(v1))
    case (NodeC(cls1, subs1), NodeC(cls2, subs2)) if cls1 == cls2 =>
      val newsubs = (subs1 zip subs2).map { tt =>
        greatestCommonClosedPrefix(tt._1, tt._2) match {
          case Right(_) => return mkPrefix(Change(t1, t2))
          case Left(newsub) => newsub
        }
      }
      Left(NodeC(cls1, newsubs))
    case _ => mkPrefix(Change(t1, t2))
  }

  def diffTree(a1: Any, a2: Any)(implicit mkOracle: MkGenericReflectionOracle): Patch = {
    val t1 = decorate(a1)
    val t2 = decorate(a2)
    val oracle = mkOracle(t1, t2)
    val change = changeTree(t1, t2, oracle)
    greatestCommonClosedPrefix(change.delCtx, change.insCtx).left.getOrElse(sys.error(s"Unclosable change $change"))
  }
}

