package inca.ir.execution

import inca.ir.CompiledUnit

trait ExecutorEngine:
  /**
   * The input to this method is best understood using an example.
   * Consider the input for a standard path program:
   *     IR-Module: path(x,y) :- ...
   * The input you could provide to this functions might be:
   *      Relation2("path", Seq("x", "y"), Seq(Seq(null, 2), Seq(1, 3)))
   * This will match all tuples, for which either y = 2 or (x = 1 and y = 3).
   * Some important things to note:
   *     1. `null` behaves like a wildcard
   *     2. the parameter names in the input must match the parameters names of the relation definition in the IR-Module
   */
  def read(rel: Relation): Relation

  /** Execution time in ns */
  def measure(rel: Relation): Long

  /**
   * Read all output relations from the idb.
   */
  def readAll(): Seq[Relation]

  /**
   * Insert a relation into the edb.
   * Note, the parameter names are not relevant and will be ignored.
   */
  def insert(edb: Relation): Unit
  /**
   * Remove (part of) a relation from the edb.
   * Note, the parameter names are not relevant and will be ignored.
   */
  def remove(edb: Relation): Unit

  /** Registers an update listener. */
  def addUpdateListener(up: RelationUpdateListener): Unit
  /** Unregisters an update listener. */
  def removeUpdateListener(up: RelationUpdateListener): Unit
  /** Executes `f` while `up` is registered. */
  def withUpdateListener[A](up: RelationUpdateListener)(f: => A): A =
    addUpdateListener(up)
    try f
    finally removeUpdateListener(up)

trait IRExecutor:
  type Engine <: ExecutorEngine
  
  def instantiate(m: CompiledUnit): Engine
  