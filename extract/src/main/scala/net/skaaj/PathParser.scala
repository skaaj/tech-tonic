package net.skaaj

import fastparse.NoWhitespace._
import fastparse._

object PathParser {
  def eval(path: String): Either[Parsed.Failure, Seq[PathAtom]] =
    parse(path, PathParser(_)) match {
      case Parsed.Success(value, _) => Right(value)
      case failure: Parsed.Failure => Left(failure)
    }

  def apply[_: P]: P[Seq[PathAtom]] = P(_root ~ _next.rep)

  private def _root[_: P]: P[Unit] = P(IgnoreCase("$"))
  private def _arrayStar[_: P]: P[Unit] = P("[*]")
  private def _field[_: P]: P[String] = P("." ~ CharIn("a-z").rep(1).!)
  private def _next[_: P]: P[PathAtom] = P(_field ~ _arrayStar.!.?).map {
    case (field, Some(_)) => ArrayAccess(field)
    case (field, _) => FieldAccess(field)
  }
}

sealed trait PathAtom
final case class FieldAccess(key: String) extends PathAtom
final case class ArrayAccess(key: String) extends PathAtom