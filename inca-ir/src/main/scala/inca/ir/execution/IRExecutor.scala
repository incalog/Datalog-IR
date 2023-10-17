package inca.ir.execution

import inca.ir.CompiledModule

trait ExecutorEngine:
  def read(rel: Relation): Relation
  def readAll(): Seq[Relation]
  def insert(edb: Relation): Unit

trait IRExecutor:
  type Engine <: ExecutorEngine
  
  def instantiate(m: CompiledModule): Engine
  