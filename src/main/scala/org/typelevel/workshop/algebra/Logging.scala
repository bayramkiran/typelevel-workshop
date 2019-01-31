package org.typelevel.workshop.algebra

trait Logging[F[_]] {
  def log(s: String): F[Unit]
}

object Logging {
  def apply[F[_]: Logging]: Logging[F] = implicitly
}
