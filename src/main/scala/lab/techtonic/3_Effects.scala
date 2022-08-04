package lab.techtonic

import zio._

/*
 * Effect properties :
 * - KIND of computation
 * - VALUE produced
 * - if side effects then construction separated from execution
 */

case class MyIO[A](unsafe: () => A) {
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafe()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafe()).unsafe())
}

// 1
val currentTime: MyIO[Long] = MyIO(() => java.lang.System.currentTimeMillis())

// 2
def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = for {
  start <- currentTime
  c <- computation
  end <- currentTime
} yield (end - start, c)

def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
  zio.flatMap(_ => runForever(zio))

def fiboZIO(n: Int): UIO[BigInt] = {
  if (n <= 1) ZIO.succeed(n)
  else {
    for {
      n_1 <- ZIO.suspendSucceed(fiboZIO(n - 1))
      n_2 <- fiboZIO(n - 2)
    } yield n_1 + n_2
  }
}


object Program extends ZIOAppDefault {
  override def run = fiboZIO(20_000).map(x => println(x))
}

//@main def effectsMain() =
//  println(currentTime.unsafe())
//  val aProgram = measure(MyIO { () =>
//    println("do some work")
//    Thread.sleep(1000)
//    println("work done")
//    42
//  })
//
//  println(aProgram.unsafe())