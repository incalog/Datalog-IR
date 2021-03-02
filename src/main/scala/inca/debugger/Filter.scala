package inca.debugger

import truechange.{Type, URI}

object Filter {

  def filterByUri(matches: Seq[Match], col: String, uris: Seq[URI]): Seq[Match] = {
    matches.filter(mat => {
      val colVal = mat.inputs.find(c => c.name == col) match {
        case Some(c) => c.value match {
          case URIValue(uri, _) => uri
          case ScalaValue(scalaVal) => scalaVal
        }
        case None => throw new IllegalArgumentException(s"Column $col not found in relation")
      }
      uris.contains(colVal)
    })
  }

  def filterByUri(matches: Seq[Match], col: String, uri: URI): Seq[Match] =
    filterByUri(matches: Seq[Match], col, Seq(uri))

  def filterByUri(matches: Seq[Match], cols: Seq[String], uris: Seq[Seq[URI]]): Seq[Match] = {
    matches.filter(mat => {
      val colVals = mat.inputs
        .filter(c => cols.contains(c.name))
        .map(c => c.value match {
          case URIValue(uri, _) => uri
          case ScalaValue(scalaVal) => scalaVal
        })

      uris.contains(colVals)
    })
  }


  def filterByType(matches: Seq[Match], col: String, tys: Seq[Type]): Seq[Match] = {
    matches.filter(mat => {
      val colTys = mat.inputs.find(c => c.name == col) match {
        case Some(c) => c.value match {
          case URIValue(_, types) => types
          case ScalaValue(scalaVal) => Seq(scalaVal)
        }
        case None => throw new IllegalArgumentException(s"Column $col not found in relation")
      }

      tys.exists(colTys.contains)
    })
  }

  def filterByType(matches: Seq[Match], col: String, ty: Type): Seq[Match] =
    filterByType(matches: Seq[Match], col, Seq(ty))

  def filterByType(matches: Seq[Match], cols: Seq[String], tys: Seq[Seq[Type]]): Seq[Match] = {
    matches.filter(mat => {
      val colTys = mat.inputs
        .filter(c => cols.contains(c.name))
        .map(c => c.value match {
          case URIValue(_, types) => types
          case ScalaValue(scalaVal) => Seq(scalaVal)
        })

      tys.exists(tyLst => tyLst.zip(colTys).forall {
        case (ty, cTys) => cTys.contains(ty)
      })
    })
  }
}
