package tcof

import scala.collection.mutable
import scala.reflect.runtime.universe._

object PrivacyLevel extends Enumeration {
  val ANY = Value("any")
  val SENSITIVE = Value("sensitive")
  type PrivacyLevel = Value
}

abstract class Action {
  var _subjectType: Type = null
  var _objectType: Type = null

  def addTypes(subjectType: Type, objectType: Type): Unit = {
    _subjectType = subjectType
    _objectType = objectType
  }
}
case class AllowAction(subj: Component, action: String, obj: Component) extends Action
case class DenyAction(subj: Component, action: String, obj: Component, privacyLevel: PrivacyLevel.PrivacyLevel) extends Action
case class NotifyAction(subj: Component, notification: Notification) extends Action

trait WithActionsInEnsemble {
  this: Ensemble =>

  def getType[T: TypeTag](obj: T) = {
    typeTag[T].tpe
  }

  private[tcof] val _actions = mutable.ListBuffer.empty[() => Iterable[Action]]

  private[tcof] def _collectActions(): Iterable[Action] = {
    val groupActions = _ensembleGroups.values.flatMap(group => group.selectedMembers.flatMap(member => member._collectActions()))

    val filteredActions =  _config.aspectProcessor.process(_actions.flatMap(_()))

    groupActions ++ filteredActions
  }

  def allow[S <: Component :TypeTag,O <: Component :TypeTag](subject: S, action: String, objct: O): Unit = allow(List(subject), action, List(objct))
  def allow[S <: Component :TypeTag,O <: Component :TypeTag](subjects: => Iterable[S], action: String, objct: O): Unit = allow(subjects, action, List(objct))
  def allow[S <: Component :TypeTag,O <: Component :TypeTag](subject: S, action: String, objects: => Iterable[O]): Unit = allow(List(subject), action, objects)

  def allow[S <: Component :TypeTag,O <: Component :TypeTag](subjects: => Iterable[S], action: String, objects: => Iterable[O]): Unit = {

    _actions += (() => {
      for {
        objct <- objects
        subject <- subjects
      } yield {
        val act = AllowAction(subject, action, objct)
        act.addTypes(getType(subject), getType(objct))
        act
      }
    })
  }

  def deny[S <: Component :TypeTag,O <: Component :TypeTag](subject: S, action: String, objct: O, privacyLevel: PrivacyLevel.PrivacyLevel): Unit = deny(List(subject), action, List(objct), privacyLevel)
  def deny[S <: Component :TypeTag,O <: Component :TypeTag](subjects: => Iterable[S], action: String, objct: O, privacyLevel: PrivacyLevel.PrivacyLevel): Unit = deny(subjects, action, List(objct), privacyLevel)
  def deny[S <: Component :TypeTag,O <: Component :TypeTag](subject: S, action: String, objects: => Iterable[O], privacyLevel: PrivacyLevel.PrivacyLevel): Unit = deny(List(subject), action, objects, privacyLevel)

  def deny[S <: Component :TypeTag,O <: Component :TypeTag](subjects: => Iterable[S], action: String, objects: => Iterable[O], privacyLevel: PrivacyLevel.PrivacyLevel): Unit = {
    _actions += (() => {
      for {
        objct <- objects
        subject <- subjects
      } yield {
        val act = DenyAction(subject, action, objct, privacyLevel)
        act.addTypes(getType(subject), getType(objct))
        act
      }
    })
  }

  def notify(subject: Component, notification: Notification): Unit = notify(List(subject), notification)

  def notify(subjects: => Iterable[Component], notification: Notification): Unit = {
    _actions += (() => {
      subjects.foreach(_.notify(notification))
      subjects.map(NotifyAction(_, notification))
    })
  }
}
