package org.typelevel.workshop.http

import cats.effect._
import cats.implicits._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.HttpService
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.typelevel.workshop.algebra.ProjectRepository
import org.typelevel.workshop.algebra._

class ProjectService[F[_]: Sync: ProjectRepository: Logging] extends Http4sDsl[F] {

  def service: HttpService[F] = HttpService[F] {

    case GET -> Root / "all" =>
      ProjectRepository[F].findAll().flatMap(l => Ok(l))

    case GET -> Root / name =>
      ProjectRepository[F].findByName(name).flatMap {
        case Some(project) => {
          Logging[F].log(s"Fetched project successfully: $name") *>
            Ok(project)
        }
        case None => {
          val errMsg = s"No project found: $name"
          Logging[F].log(errMsg) *>
          NotFound(errMsg.asJson)
        }
      }

    case req @ POST -> Root / "update" =>
      ???

    case req @ DELETE -> Root / name =>
      ProjectRepository[F].deleteProject(name).flatMap(_ => NoContent())



  }
}
