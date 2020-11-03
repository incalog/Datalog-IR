package inca.frontend.typechecker

trait Resolvable[T] {
  var target: Option[T] = None
}
