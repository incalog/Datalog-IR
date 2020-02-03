package org.inca.diff.example

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.HasCryptoHash

object SimpleExp {
  sealed trait Exp extends HasCryptoHash{
    def uri: URI = URI(toString)
    def foreach(f: Exp => Unit): Unit
  }
  case class Num(n: Int) extends Exp {
    override def foreach(f: Exp => Unit): Unit = f(this)
    override lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      hashNonDiffable(n, digest)
      digest.digest()
    }
  }
  case class Neg(e: Exp) extends Exp {
    override def foreach(f: Exp => Unit): Unit = {f(this); e.foreach(f)}
    override lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      digest.update(e.$hash)
      digest.digest()
    }
  }
  case class Add(e1: Exp, e2: Exp) extends Exp {
    override def foreach(f: Exp => Unit): Unit = {f(this); e1.foreach(f); e2.foreach(f)}
    override lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      digest.update(e1.$hash)
      digest.update(e2.$hash)
      digest.digest()
    }
  }
  case class Mul(e1: Exp, e2: Exp) extends Exp {
    override def foreach(f: Exp => Unit): Unit = {f(this); e1.foreach(f); e2.foreach(f)}
    override lazy val $hash: Array[Byte] = {
      val digest = mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      digest.update(e1.$hash)
      digest.update(e2.$hash)
      digest.digest()
    }
  }
  case class MetavarHole(mv: Metavar) extends Exp {
    override def foreach(f: Exp => Unit): Unit = throw new IllegalStateException(s"Input trees may not contain hole $this")
    override def $hash: Array[Byte] = throw new IllegalStateException(s"Input trees may not contain hole $this")
    override def toString: String = mv.toString
  }

  class Metavar(val i: Int, val originalTree: Exp) {
    private var _availableCopies: Seq[Exp] = Seq()
    def addCopy(e: Exp): Unit = {
      _availableCopies +:= e
    }
    def takeCopy: Exp = {
      val copy = _availableCopies.head
      _availableCopies = _availableCopies.tail
      copy
    }
    def availableCopies: Int = _availableCopies.size
    var requiredCopies: Int = 0

    override def equals(obj: Any): Boolean = obj match {
      case other: Metavar => i == other.i
      case _ => false
    }
    override def hashCode(): Int = 31*i
    override def toString: String = "#" + i
  }

  trait MoveOracle {
    def predict(e: Exp): Option[Metavar]
  }
  def mkOracle(src: Exp, dest: Exp): MoveOracle = {
    val srcTrie = new PatriciaTrie[Metavar]()
    var freshCount = 0
    src.foreach { e =>
      val key = e.$hashString
      srcTrie.put(key, new Metavar(freshCount, e))
      freshCount += 1
    }

    val intersectTrie = new PatriciaTrie[Metavar]()
    dest.foreach { e =>
      val key = e.$hashString
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
    }

    e => Option(intersectTrie.get(e.$hashString))
  }

  def diff(src: Exp, dest: Exp): Seq[ChangeCmd] = {
    val oracle = mkOracle(src, dest)
    val srcC = bindMetavars(src)(oracle, findCopies = true)
    val destC = bindMetavars(dest)(oracle, findCopies = false)

    computeChanges(URI("<root>"), RootLink, srcC, destC)
  }

  def bindMetavars(exp: Exp)(implicit oracle: MoveOracle, findCopies: Boolean): Exp = oracle.predict(exp) match {
    case Some(mv) if findCopies || mv.availableCopies > mv.requiredCopies =>
      if (findCopies)
        mv.addCopy(exp)
      else
        mv.requiredCopies += 1
      MetavarHole(mv)
    case _ => exp match {
      case Num(n) => Num(n)
      case Neg(e) => Neg(bindMetavars(e))
      case Add(e1, e2) => Add(bindMetavars(e1), bindMetavars(e2))
      case Mul(e1, e2) => Mul(bindMetavars(e1), bindMetavars(e2))
      case MetavarHole(_) => throw new IllegalStateException(s"Input trees may not contain hole $this")
    }
  }

  sealed trait Link
  case object RootLink extends Link
  case class NamedLink(name: String) extends Link {
    override def toString: String = name
  }

  trait NodeRef
  case class URI(id: String) extends NodeRef
  case class Literal[T](value: T) extends NodeRef
  case class Var(name: String) extends NodeRef {
    override def toString: String = name
  }

  type Parent = URI

  trait ChangeCmd
  case class LoadNode(v: Var, node: Class[_], kids: Iterable[(Link, NodeRef)]) extends ChangeCmd {
    override def toString: String = s"$v = LoadNode(${node.getSimpleName}, ${kids.map(kv => kv._1 + "=" + kv._2).mkString(", ")})"
  }
  case class UnloadNode(ref: NodeRef) extends ChangeCmd
  case class AttachNode(parent: NodeRef, l: Link, newchild: NodeRef) extends ChangeCmd
  case class DetachNode(ref: NodeRef) extends ChangeCmd

  var freshCount = 0
  def freshVar(): Var = {
    val name = s"x_$freshCount"
    freshCount += 1
    Var(name)
  }

  def computeChanges(parent: Parent, link: Link, src: Exp, dest: Exp): Seq[ChangeCmd] = (src,dest) match {
    case (Num(n1), Num(n2)) if n1 == n2 => Seq()
    case (Neg(e), Neg(f)) =>
      computeChanges(src.uri, NamedLink("e"), e, f)
    case (Add(e1, e2), Add(f1, f2)) =>
      computeChanges(src.uri, NamedLink("e1"), e1, f1) ++
        computeChanges(src.uri, NamedLink("e2"), e2, f2)
    case (Mul(e1, e2), Add(f1, f2)) =>
      computeChanges(src.uri, NamedLink("e1"), e1, f1) ++
        computeChanges(src.uri, NamedLink("e2"), e2, f2)
    case (MetavarHole(mv1), MetavarHole(mv2)) if mv1.i == mv2.i => Seq()
    case (_, _) =>
      val ch1 = unload(src)
      val (v, ch2) = load(dest)
      ch1 ++ ch2 :+ AttachNode(parent, link, v)
  }

  def unload(src: Exp): Seq[ChangeCmd] = src match {
    case Num(n) => Seq(UnloadNode(src.uri))
    case Neg(e) => unload(e) :+ UnloadNode(src.uri)
    case Add(e1, e2) => unload(e1) ++ unload(e2) :+ UnloadNode(src.uri)
    case Mul(e1, e2) => unload(e1) ++ unload(e2) :+ UnloadNode(src.uri)
    case MetavarHole(mv) =>
      if (mv.availableCopies > mv.requiredCopies) {
        val copy = mv.takeCopy
        unload(copy)
      } else {
        Seq()
      }
  }

  def load(dest: Exp): (NodeRef,Seq[ChangeCmd]) = dest match {
    case Num(n) =>
      val v = freshVar()
      val node = LoadNode(v, classOf[Num], Seq(NamedLink("n") -> Literal(n)))
      (v, Seq(node))
    case Neg(e) =>
      val (ve, ch) = load(e)
      val v = freshVar()
      val node = LoadNode(v, classOf[Neg], Seq(NamedLink("e") -> ve))
      (v, ch :+ node)
    case Add(e1, e2) =>
      val (ve1, ch1) = load(e1)
      val (ve2, ch2) = load(e2)
      val v = freshVar()
      val node = LoadNode(v, classOf[Add], Seq(NamedLink("e1") -> ve1, NamedLink("e2") -> ve2))
      (v, ch1 ++ ch2 :+ node)
    case Mul(e1, e2) =>
      val (ve1, ch1) = load(e1)
      val (ve2, ch2) = load(e2)
      val v = freshVar()
      val node = LoadNode(v, classOf[Mul], Seq(NamedLink("e1") -> ve1, NamedLink("e2") -> ve2))
      (v, ch1 ++ ch2 :+ node)
    case MetavarHole(mv) =>
      if (mv.availableCopies > 0) {
        val copy = mv.takeCopy.uri
        (copy, Seq(DetachNode(copy)))
      } else {
        load(mv.originalTree)
      }
  }
}