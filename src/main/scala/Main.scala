
import scala.util.Try
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

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper + 1)) // doesn't work w/ flatmap?

  def checkContinue(name: String): IO[Boolean] = {
    printLnEffect(("Do you want to continue, " + name + "?"))
      .flatMap(_ => readLineEffect)
      .flatMap(input => input match {
        case "y" => IO.point(true)
        case "n" => IO.point(false)
        case _ => checkContinue(name)
      })
  }

  def gameLoop(name: String): IO[Unit] = {
    nextInt(5).flatMap(num =>
      printLnEffect("Dear " + name + ", please guess a number from 1 to 5:")
        .flatMap(_ => readLineEffect)
        .flatMap(input => parseInt(input).fold(
          printLnEffect("You did not enter a number")
        )(guess =>
          if (guess == num) printLnEffect("You guessed right, " + name + "!")
          else printLnEffect("You guessed wrong, " + name + "! The number was: " + num)
        )
        )
        .flatMap(_ => checkContinue(name))
        .flatMap(shouldContinue => shouldContinue match {
          case true => gameLoop(name)
          case false => IO.point(())
        })
    )

  }


  def main(args: Array[String]): Unit = {
    printLnEffect("What is your name?")
      .flatMap(_ => readLineEffect)
      .flatMap((name: String) =>
        printLnEffect("Hello, " + name)
        .flatMap(_ => gameLoop(name))
      ).unsafeRun()
  }

}
