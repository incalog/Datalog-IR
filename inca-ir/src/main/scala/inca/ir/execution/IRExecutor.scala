package inca.ir.execution

import inca.ir.CompiledModule

trait ExecutorEngine:
  def read(rel: String): Relation
  def readAll(): Seq[Relation]
  def insert(edb: String, tuple: Seq[Any]): Unit =
    insertAll(edb, Iterable.single(tuple))
  def insertAll(edb: String, tuples: Iterable[Seq[Any]]): Unit

trait IRExecutor:
  type Engine <: ExecutorEngine
  
  def instantiate(m: CompiledModule): Engine
  