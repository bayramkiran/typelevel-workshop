package org.typelevel.workshop.algebra

import org.typelevel.workshop.model.Project

trait ProjectRepository[F[_]] {
  def findByName(name: String): F[Option[Project]]

  def findAll(): F[List[Project]]

  def updateProject(id: Int, name: String, description: String): F[Unit]

  def deleteProject(name: String): F[Unit]
}

object ProjectRepository {
  def apply[F[_]: ProjectRepository]: ProjectRepository[F] = implicitly
}
