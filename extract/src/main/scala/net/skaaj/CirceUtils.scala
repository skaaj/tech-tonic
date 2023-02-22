package net.skaaj

import io.circe._
import io.circe.optics.JsonTraversalPath
import monocle.Traversal

object CirceUtils {
  implicit class CirceOps(val self: Json) {
    private def _fetch(path: String): Either[String, JsonTraversalPath] = {
      PathParser.eval(path) match {
        case Left(err) => Left(err.longMsg)
        case Right(atoms) =>
          Right(atoms.foldLeft(JsonTraversalPath(Traversal.id)) {
            case (acc, FieldAccess(key)) => acc.selectDynamic(key)
            case (acc, ArrayAccess(key)) => acc.selectDynamic(key).each
          })
      }
    }

    def fetchAll[T](path: String)(implicit decode: Decoder[T], encode: Encoder[T]): Either[Unit, List[T]] = {
      _fetch(path) match {
        case Left(_) => Left(())
        case Right(traversal) => Right(traversal.as[T].getAll(self))
      }
    }

    def fetch[T](path: String)(implicit decode: Decoder[T], encode: Encoder[T]): Either[Unit, Option[T]] = {
      fetchAll(path).map(_.headOption)
    }

    def fetchAllRequired[T](path: String)(implicit decode: Decoder[T], encode: Encoder[T]): Either[String, List[T]] = {
      _fetch(path) match {
        case Right(traversal) =>
          val items = traversal.as[T].getAll(self)
          if (items.isEmpty) Left(s"Could not find items in '$path'")
          else Right(items)
        case Left(err) => Left(err)
      }
    }

    def fetchRequired[T](path: String)(implicit decode: Decoder[T], encode: Encoder[T]): Either[String, T] = {
      fetchAllRequired(path).flatMap {
        case List(head) => Right(head)
        case _ => Left(s"Found multiple items at '$path'")
      }
    }
  }
}
