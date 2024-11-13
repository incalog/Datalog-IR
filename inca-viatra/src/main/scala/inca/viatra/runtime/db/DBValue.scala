package inca.viatra.runtime.db

//import inca.runtime.data.MockURI

import truechange.URI

object DBValue {
  def prettyPrint(v: Any, db: DatabaseInspector): String = {
    v match {
      /*case uri: MockURI =>
        val v = MockURI.convertToValue(uri)
        val str = v.deepPrettyPrint(db)
        str*/
      case uri: URI => db.prettyPrint(uri)
      case v => v.toString
    }
  }
}
