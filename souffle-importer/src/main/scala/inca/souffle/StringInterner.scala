package inca.souffle

import org.eclipse.collections.api.map.primitive.{MutableIntObjectMap, MutableObjectIntMap}
import org.eclipse.collections.impl.map.mutable.primitive.{IntObjectHashMap, ObjectIntHashMap}

object StringInterner {
  val values: MutableObjectIntMap[String] = ObjectIntHashMap.newMap()
  val backwardValues: MutableIntObjectMap[String] = IntObjectHashMap.newMap()

  def intern(str: String): Int = {
    if (values.containsKey(str)) values.get(str)
    else {
      val index = values.size + 1
      values.put(str, index)
      backwardValues.put(index, str)
      index
    }
  }

  def get(key: Int): String = {
    if (backwardValues.containsKey(key)) backwardValues.get(key)
    else throw new RuntimeException(s"Key $key not found")
  }
}
