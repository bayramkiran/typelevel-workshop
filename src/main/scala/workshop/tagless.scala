package workshop

import java.io.{File, FileNotFoundException}

import workshop.monoids.{IO, Monad}
import workshop.monoids.Monad.ops._

import scala.io.{Source, StdIn}

object tagless {

  trait Console[F[_]] {
    def printLine(s: String): F[Unit]
    def readLine: F[String]
  }

  object Console {
    def apply[F[_]](implicit ev: Console[F]): Console[F] = ev
  }

  // Rewrite the `monoids.consoleProgram` from earlier using tagless final style
  def consoleProgram[F[_]: Monad: Console]: F[Unit] =
    for {
      _ <- Console[F].printLine("Enter your name")
      name <- Console[F].readLine
      _ <- Console[F].printLine(s"Hello $name")
    } yield ()


  //create a Console interpreter for IO
  implicit def consoleIO: Console[IO] = new Console[IO] {
    def printLine(s: String): IO[Unit] = monoids.printLine(s)

    def readLine: IO[String] = monoids.readLine
  }

  type Id[A] = A

  //create a Console interpreter for Id that does nothing for printing and returns a static string for reading
  implicit def consoleId: Console[Id] = new Console[Id] {
    def printLine(s: String): Id[Unit] = ()

    def readLine: Id[String] = "bayram"
  }

  implicit def monadId: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    def unit: Id[Unit] = ()

    override def pure[A](a: A): Id[A] = a
  }


  // Create a Tagless algebra for accesing the file system
  trait FileSystem[F[_]] {
    def readFile(file: File): F[Either[String, String]]
  }

  object FileSystem {
    def apply[F[_]](implicit ev: FileSystem[F]): FileSystem[F] = ev
  }

  implicit def fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    override def readFile(file: File): IO[Either[String, String]] =
      IO(() =>
        try {
          Right(Source.fromFile(file).getLines().mkString)
        } catch {
          case t: FileNotFoundException => Left("file not found")
        }
      )
  }

  // Rewrite the `monoids.fileProgram` from earlier using tagless final style
  def fileProgram[F[_]: Monad: FileSystem: Console]: F[Unit] =
    for {
      _           <- Console[F].printLine("Enter file name")
      fileName    <- Console[F].readLine
      fileContentOrError <- FileSystem[F].readFile(new File(fileName))
      _           <- Console[F].printLine(fileContentOrError.merge)
    } yield ()
}
