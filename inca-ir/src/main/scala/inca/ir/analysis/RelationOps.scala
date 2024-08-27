package inca.ir.analysis

trait RelationOps[V, RV, B, Var]:
  lazy val empty: RV = makeRelation(Vector(), Seq(Vector()))
  def makeRelation(cols: Vector[Var], vals: Seq[Vector[V]]): RV
  def getCols(rel: RV): Vector[Var]
  def scan[A](rel: RV)(f: Vector[V] => A): Seq[A]
  def unionFold[A](itr: Iterable[A])(f: A => RV): RV
  def projection(rel: RV, cols: Vector[Var]): RV
  def subset(rel1: RV, rel2: RV): B
  def union(rel1: RV, rel2: RV): RV
  def antiJoin(rel1: RV, rel2: RV): RV
  def natJoin(rel1: RV, rel2: RV): RV