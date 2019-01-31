package org.typelevel.workshop.interpreters

import org.typelevel.workshop.algebra.ProjectRepository
import org.typelevel.workshop.model.Project
import cats.data.State

object ProjectRepositoryState {
  implicit def projectRepoInterpreter: ProjectRepository[State[List[Project], ?]] =
    new ProjectRepository[State[List[Project], ?]] {
      def findByName(name: String): State[List[Project], Option[Project]] =
        State.get[List[Project]].map(_.find(_.name == name))

      def deleteProject(name: String): State[List[Project], Unit] =
        State.modify[List[Project]](_.filter(_.name == name))

      def findAll(): State[List[Project], List[Project]] =
        State.get[List[Project]]

      def updateProject(id: Int, name: String, description: String): State[List[Project], Unit] =
        State.modify[List[Project]] {list: List[Project] =>
          val projectOpt = list.find(_.id == id)

          projectOpt.map { project =>
            val updatedProject: Project = project.copy(name = name, description = description)
            updatedProject :: list.filter(_.id != project.id)
          }.getOrElse(list)

        }
    }
}
