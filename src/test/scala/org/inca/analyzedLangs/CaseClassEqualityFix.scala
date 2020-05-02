package org.inca.analyzedLangs

abstract class CaseClassEqualityFix {
  override def equals(obj: Any): Boolean = this.hashCode() == obj.hashCode()

  override def hashCode(): Int = System.identityHashCode(this)
}