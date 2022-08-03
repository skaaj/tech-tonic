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

@main def main() =
  println(combineAll(Seq(2, 3, 5, 32)))
