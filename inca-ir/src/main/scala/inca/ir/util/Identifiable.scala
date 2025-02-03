package inca.ir.util

var nextId: Long = 0

trait Identifiable:
  val id: Long = nextId
  nextId += 1
