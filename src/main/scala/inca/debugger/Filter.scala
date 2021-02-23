package inca.debugger

import truechange.{Type, URI}

object Filter {

  // Filter by URI
  def filter(matches: Seq[Match], col: String, uris: Seq[URI]): Seq[Match] = {
    matches.filter(mat => {
      val colVal = mat.inputs.find(c => c.name == col) match {
        case Some(c) => c.value match {
          case ColURI(uri, _, _) => uri
          case ColScalaType(scalaVal) => scalaVal
        }
        case None => throw new IllegalArgumentException(s"Column $col not found in relation")
      }
      uris.contains(colVal)
    })
  }

  def filter(matches: Seq[Match], col: String, uri: URI): Seq[Match] =
    filter(matches: Seq[Match], col, Seq(uri))


  // Filter by Type
  def filter(matches: Seq[Match], col: String, tys: Seq[Type])(implicit i: DummyImplicit): Seq[Match] = {
    matches.filter(mat => {
      val colTys = mat.inputs.find(c => c.name == col) match {
        case Some(c) => c.value match {
          case ColURI(_, tys, _) => tys
          case ColScalaType(scalaVal) => Seq(scalaVal)
        }
        case None => throw new IllegalArgumentException(s"Column $col not found in relation")
      }

      tys.exists(colTys.contains)
    })
  }

  def filter(matches: Seq[Match], col: String, ty: Type): Seq[Match] =
    filter(matches: Seq[Match], col, Seq(ty))
}
