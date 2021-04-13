package tcof

import tcof.InitStages.InitStages
import tcof.Utils._
import org.chocosolver.solver.Model

import scala.collection.mutable

trait Component extends WithName with Notifiable {
  override def toString: String =
    s"""Component "$name""""

  // method stubs used in aspects:
  val hasFailure = true
  def getValidator : Component = this

}
