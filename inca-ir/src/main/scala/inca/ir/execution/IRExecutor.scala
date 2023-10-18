package inca.ir.execution

import inca.ir.CompiledModule

trait ExecutorEngine:
  /* The input to this method is best understood using an example.
   * Consider the inoput for a standard path program:
   *     IR-Module: path(x,y) :- ...
   * The input you could provide to this functions might be:
   *      Relation2("path", Seq("x", "y"), Seq(Seq(null, 2), Seq(1, 3)))
   * This will match all tuples, for which either y = 2 or (x = 1 and y = 3).
   * Some important things to note:
   *     1. `null` behaves like a wildcard
   *     2. the parameter names in the input must match the parameters names of the relation definition in the IR-Module
   */
  def read(rel: Relation): Relation
  /*
   * Read all output relations from the idb.
   */
  def readAll(): Seq[Relation]
  /*
   * Insert a relation into the edb.
   * You might add multiple tuples at once by specifying multiple `matches` for a Relation.
   * Note, the parameter names are not relevant and in fact will be ignored.
   */
  def insert(edb: Relation): Unit

trait IRExecutor:
  type Engine <: ExecutorEngine
  
  def instantiate(m: CompiledModule): Engine
  