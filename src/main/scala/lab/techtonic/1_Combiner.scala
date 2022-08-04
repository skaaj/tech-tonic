package lab.techtonic

// definition
trait Combiner[A] {
  def empty: A
  def combine(x: A, y: A): A
}

def combineAll[A](values: Seq[A])(using combiner: Combiner[A]): A =
  values.foldLeft(combiner.empty)(combiner.combine)

// specific
given intAdditionCombiner: Combiner[Int] with {
  override def empty: Int = 0
  override def combine(x: Int, y: Int): Int = x + y
}

given optionCombine[A](using combiner: Combiner[A]): Combiner[Option[A]] with {
  override def empty = Some(combiner.empty)
  override def combine(x: Option[A], y: Option[A]): Option[A] = for {
    vx <- x
    vy <- y
  } yield combiner.combine(vx, vy)
}

@main def combinerMain() =
  val foo: Option[Int] = combineAll(Seq(Some(1), None, Some(2)))
  println(foo)
