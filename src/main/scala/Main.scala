import scala.io.StdIn.readLine

object Main {

  final case class IO[A](unsafeRun: () => A) { self =>
    def map[B](fn: A => B): IO[B] = IO(() => fn(self.unsafeRun()))
    def flatMap[B](fn: A => IO[B]): IO[B] = IO(() => fn(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  def readLineEffect: IO[String] = IO(() => readLine())

  def printLnEffect(s: String): IO[Unit] = IO(() => println(s))


  def main(args: Array[String]): Unit = readLineEffect.flatMap((s: String) => printLnEffect(s)).unsafeRun()

}
