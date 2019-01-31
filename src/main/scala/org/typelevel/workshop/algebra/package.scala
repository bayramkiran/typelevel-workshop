package org.typelevel.workshop

import cats.effect.IO
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

package object algebra {


  implicit def consoleLogger: Logging[IO] = new Logging[IO] {
    override def log(s: String): IO[Unit] =
      IO(println(s"LOG MESSAGE: $s"))
  }

//  implicit def slf4jLogger: Logging[IO] = new Logging[IO] {
//    override def log(s: String): IO[Unit] =
//      Slf4jLogger.fromName("slf4jLogger").
//  }
}
