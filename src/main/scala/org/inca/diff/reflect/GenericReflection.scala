package org.inca.diff.reflect

import java.lang.reflect.Field
import java.security.MessageDigest

object GenericReflection {

  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  // marker trait for types should be structurally diffed
  trait StructuralDiff

  implicit def withClassOps[A](cls: Class[A]): ClassOps[A] = new ClassOps(cls)
  class ClassOps[A](val cls: Class[A]) extends AnyVal {
    def extendsStructuralDiff: Boolean = classOf[StructuralDiff].isAssignableFrom(cls)
    def allFields: Seq[Field] = {
      ClassOps.allFieldsCache.get(cls) match {
        case Some(fields) => fields
        case None =>
          var fields = cls.getDeclaredFields.toSeq.filter(!_.getName.startsWith("$"))
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


  type Node = Any

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

  def asCtx[A <: Plug](node: Node): TreeC[A] = {
    val cls = node.getClass
    if (cls.extendsStructuralDiff) {
      val subs = cls.allFieldVals(node).map(asCtx[A](_))
      NodeC(cls, subs)
    }
    else
      ValC(node)
  }

  def retainHoles[A <: Plug](tc: TreeC[A], vs: Set[A], t: Node): TreeC[A] = tc match {
    case Hole(a) => if (vs.contains(a)) tc else asCtx(t)
    case ValC(_) => tc
    case NodeC(cls, subs) => {
      val newsubs = (subs zip t.getClass.allFields).map { tt =>
        val subTc = tt._1
        val subT = tt._2.get(t)
        retainHoles(subTc, vs, subT)
      }
      NodeC(cls, newsubs)
    }
  }

  case class Change[A <: Plug](delCtx: TreeC[A], insCtx: TreeC[A]) extends Plug {
    override val freevars: Set[MetaVar] = insCtx.freevars diff delCtx.freevars
    def isClosed: Boolean = freevars.isEmpty
  }

  def applyChange(c: Change[MetaVar], t: Node): Option[Node] =
    del(c.delCtx, t) flatMap (ins(c.insCtx, _))

  def del(ctx: TreeC[MetaVar], tree: Node): Option[Map[MetaVar, Node]] =
    go(ctx, tree, Map())

  def go(ctx: TreeC[MetaVar], node: Node, m: Map[MetaVar, Node]): Option[Map[MetaVar, Node]] = ctx match {
    case ValC(v) => if (v == node) Some(m) else None
    case NodeC(cls, subs) =>
      if (cls != node.getClass)
        None
      else {
        var res = m
        (subs zip cls.allFieldVals(node)).foreach { tt =>
          go(tt._1, tt._2, res) match {
            case Some(m_) => res = m_
            case None => return None
          }
        }
        Some(res)
      }
    case Hole(i) => m.get(i) match {
      case None => Some(m + (i -> node))
      case Some(node_) => if(node == node_) Some(m) else None
    }
  }


  def ins(ctx: TreeC[MetaVar], m: Map[MetaVar, Node]): Option[Node] = ctx match {
    case ValC(v) => Some(v)
    case NodeC(cls, subs) =>
      val node = cls.newInstance()
      (subs zip cls.allFields).foreach { tt =>
        ins(tt._1, m) match {
          case Some(subnode) => tt._2.set(node, subnode)
          case None => return None
        }
      }
      Some(node)
    case Hole(i) => m.get(i)
  }


  def changeTree(src: Node, dest: Node, oracle: GenericReflectionOracle): Change[MetaVar] = {
    val change = Change(extract(oracle, src), extract(oracle, dest))
    postprocess(src, dest, change)
  }

  def extract(oracle: GenericReflectionOracle, node: Node): TreeC[MetaVar] = oracle.predict(node) match {
    case Some(i) => Hole(i)
    case None =>
      if (node == null)
        return ValC(null)
      val cls = node.getClass
      if (cls.extendsStructuralDiff) {
        val subs = cls.allFieldVals(node).map(extract(oracle, _))
        NodeC(cls, subs)
      }
      else
        ValC(node)
  }

  def postprocess(src: Node, dest: Node, c: Change[MetaVar]): Change[MetaVar] = {
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
    case (ValC(v1), ValC(v2)) if v1 == v2 =>
      println("foo")
      Left(ValC(v1))
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

  def diffTree(t1: Node, t2: Node)(implicit mkOracle: MkGenericReflectionOracle): Patch = {
    val oracle = mkOracle(t1, t2)
    val change = changeTree(t1, t2, oracle)
    greatestCommonClosedPrefix(change.delCtx, change.insCtx).left.getOrElse(sys.error(s"Unclosable change $change"))
  }

  def applyPatch(p: Patch, node: Node): Option[Node] = p match {
    case Hole(change) => applyChange(change, node)
    case ValC(v) if v == node => Some(node)
    case NodeC(cls, subs) if cls == node.getClass =>
      val newnode = cls.newInstance()
      (subs zip cls.allFields).foreach { tt =>
        val fld = tt._2
        val subnode = fld.get(node)
        applyPatch(tt._1, subnode) match {
          case Some(newSubnode) => fld.set(newnode, subnode)
          case None => return None
        }
      }
      Some(newnode)
  }
}
