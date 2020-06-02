package org.inca.meta

abstract class CaseClassEqualityFix {
  override def equals(obj: Any): Boolean = this.hashCode() == obj.hashCode()

  override def hashCode(): Int = System.identityHashCode(this)
}