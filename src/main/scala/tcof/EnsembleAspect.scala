package tcof

import scala.collection.mutable.ListBuffer

object ActionSelection extends Enumeration {
  val ALL = Value("all")
  val ANY = Value("any")
  type ActionSelection = Value
}

object ObjectSelection extends Enumeration {
  val ANY = Value("any")
  type ObjectSelection = Value
}

sealed class AspectActionType
case class AspectInsertActionType(aspectAction : AspectAction) extends AspectActionType
case class AspectDeleteActionType(aspectAction : AspectAction) extends AspectActionType

sealed class AspectAction
case class AspectAllowAllForSubjectAndObject(subj: Component, obj: Component) extends AspectAction
case class AspectAllowForSubject(subj: Component) extends AspectAction
case class AspectDenyAllForSubject(subj: Component) extends AspectAction
case class AspectDenyForSubjectAndObject(subj: Component, obj: Component) extends AspectAction
case class AspectAllowActionForSubjectAndObject(subj: Component, action: String, obj: Component) extends AspectAction

import scala.reflect.runtime.universe._

class EnsembleAspect {

  val availableActions = Set("enter", "use", "read.personalData.phoneNo", "read.distanceToWorkPlace", "read.personalData")

  type SubjectType
  type ObjectType

  var _subjectType: Type = null
  var _objectType: Type = null

  def addTypes(subjectType: Type, objectType: Type): Unit = {
    _subjectType = subjectType
    _objectType = objectType
  }

  var _spec: (SubjectType, ObjectType) => Unit = null
  var _condition: Boolean = false
  var _aspectActionType: AspectActionType = null
  var _ensembleActions: ListBuffer[Action] = null

  def spec(fn: (SubjectType, ObjectType) => Unit) = {
    _spec = fn
  }

  def pointcut(condition: Boolean): Unit = {
    _condition = condition
  }

  def insert_rules(aspectAction: AspectAction): Unit = {
    _aspectActionType = AspectInsertActionType(aspectAction)
  }

  def delete_rules(aspectAction: AspectAction): Unit = {
    _aspectActionType = AspectDeleteActionType(aspectAction)
  }

  def allow(subject: Component, action: ActionSelection.ActionSelection, objct: Component): AspectAction =
    _allow(subject, action, objct)

  def allow(subject: Component, action: ActionSelection.ActionSelection, objct: ObjectSelection.ObjectSelection): AspectAction =
    _allow(subject, action, objct)

  def allow(subject: Component, action: String, objct: Component): AspectAction =
    _allow(subject, action, objct)

  def _allow(subject: Component, action: Any, objct: Any): AspectAction =
    (action, objct) match {
      case (ActionSelection.ALL, objct: Component) => AspectAllowAllForSubjectAndObject(subject, objct)
      case (ActionSelection.ANY, ObjectSelection.ANY) => AspectAllowForSubject(subject)
      case (action: String, objct: Component) => AspectAllowActionForSubjectAndObject(subject, action, objct)
      case _ => null
    }

  def deny(subject: Component, action: ActionSelection.ActionSelection, objct: Component): AspectAction =
    _deny(subject, action, objct)

  def deny(subject: Component, action: ActionSelection.ActionSelection, objct: ObjectSelection.ObjectSelection): AspectAction =
    _deny(subject, action, objct)

  def _deny(subject: Component, action: ActionSelection.ActionSelection, objct: Any): AspectAction =
    (action, objct) match {
      case (ActionSelection.ANY, objct: Component) => AspectDenyForSubjectAndObject(subject, objct)
      case (ActionSelection.ALL, ObjectSelection.ANY) => AspectDenyAllForSubject(subject)
      case _ => null
    }

  def existsAllowRule(subject: Component, action: ActionSelection.ActionSelection, objct: Component): Boolean =
    _existsAllowRule(subject, action, objct)

  def existsAllowRule(subject: Component, action: ActionSelection.ActionSelection, objct: ObjectSelection.ObjectSelection): Boolean =
    _existsAllowRule(subject, action, objct)

  def _existsAllowRule(subject: Component, action: ActionSelection.ActionSelection, objct: Any): Boolean =
    (action, objct) match {
      case (ActionSelection.ANY, _: Component) => _ensembleActions.exists(_.isInstanceOf[AllowAction])
      case (ActionSelection.ANY, ObjectSelection.ANY) => _ensembleActions.exists(_.isInstanceOf[AllowAction])
      case _ => false
    }

  def existsDenyRule(subject: Component, action: ActionSelection.ActionSelection, objct: Component): Boolean =
    _existsAllowRule(subject, action, objct)

  def existsDenyRule(subject: Component, action: ActionSelection.ActionSelection, objct: ObjectSelection.ObjectSelection): Boolean =
    _existsAllowRule(subject, action, objct)

  def _existsDenyRule(subject: Component, action: ActionSelection.ActionSelection, objct: Any): Boolean =
    (action, objct) match {
      case (ActionSelection.ANY, _: Component) => _ensembleActions.exists(_.isInstanceOf[DenyAction])
      case (ActionSelection.ANY, ObjectSelection.ANY) => _ensembleActions.exists(_.isInstanceOf[DenyAction])
      case _ => false
    }

  def createRulesToAdd(aspectAction: AspectAction, ensembleActionObject: Component): Set[Action] =
    aspectAction match {
      case AspectAllowAllForSubjectAndObject(subject, objct) =>
        for {action <- availableActions} yield
          AllowAction(subject, action, objct)
      case AspectDenyAllForSubject(subject) =>
        for {action <- availableActions} yield
          DenyAction(subject, action, ensembleActionObject, PrivacyLevel.ANY)
      case AspectAllowActionForSubjectAndObject(subject, action, objct) =>
        Set(AllowAction(subject, action, objct))
      case _ => Set[Action]()
    }

  def createRulesToDelete(aspectAction: AspectAction, ensembleAction: String, ensembleActionObject: Component, privacyLevel: PrivacyLevel.PrivacyLevel = null): Set[Action] =
    aspectAction match {
      case AspectAllowForSubject(subject) => Set(AllowAction(subject, ensembleAction, ensembleActionObject))
      case AspectDenyForSubjectAndObject(subject, objct) => Set(DenyAction(subject, ensembleAction, objct, privacyLevel))
      case _ => Set[Action]()
    }

  def applyAspectToAction(subj: Component, ensembleAction: String, obj: Component, privacyLevel: PrivacyLevel.PrivacyLevel = null): (Set[Action],Set[Action]) = {
    _spec(subj.asInstanceOf[SubjectType],obj.asInstanceOf[ObjectType])

    var rulesToAdd = Set[Action]()
    var rulesToDelete = Set[Action]()

    if (_condition) {
      _aspectActionType match {
        case AspectInsertActionType(aspectAction) =>
          rulesToAdd = createRulesToAdd(aspectAction, obj)
        case AspectDeleteActionType(aspectAction) =>
          rulesToDelete = createRulesToDelete(aspectAction, ensembleAction, obj, privacyLevel)
      }
    }

    (rulesToAdd, rulesToDelete)
  }

  def applyTo(ensembleActions: ListBuffer[Action]): (scala.collection.mutable.Set[Action],scala.collection.mutable.Set[Action]) = {
    val allowDenyActions = ensembleActions.filter(!_.isInstanceOf[NotifyAction])
    val filteredEnsembleActions = allowDenyActions.filter(a => a._subjectType <:< _subjectType && a._objectType <:< _objectType)
    _ensembleActions = filteredEnsembleActions

    val rulesToAdd = scala.collection.mutable.Set[Action]()
    val rulesToDelete = scala.collection.mutable.Set[Action]()

    for (action <- filteredEnsembleActions) {
      action match {
        case AllowAction(subj, action, obj)  => {
          val (newRulesToAdd, newRulesToDelete) = applyAspectToAction(subj, action, obj)
          rulesToAdd ++= newRulesToAdd
          rulesToDelete ++= newRulesToDelete
        }
        case DenyAction(subj, action, obj, privacyLevel)  => {
          val (newRulesToAdd, newRulesToDelete) = applyAspectToAction(subj, action, obj, privacyLevel)
          rulesToAdd ++= newRulesToAdd
          rulesToDelete ++= newRulesToDelete
        }
        case _ => println("Cannot process this type of action.")
      }
    }

    (rulesToAdd, rulesToDelete)
  }

}

class EnsembleAspectProcessor(aspects: scala.collection.mutable.ListBuffer[EnsembleAspect]) {

  def process(ensembleActions: ListBuffer[Action]): ListBuffer[Action] = {

//    println("******")
//    println(ensembleActions)

    val rulesToAdd = scala.collection.mutable.Set[Action]()
    val rulesToDelete = scala.collection.mutable.Set[Action]()

    for (aspect <- aspects) {
      println("Applying aspect: " + aspect)
      val (newRulesToAdd, newRulesToDelete) = aspect.applyTo(ensembleActions)
      rulesToAdd ++= newRulesToAdd
      rulesToDelete ++= newRulesToDelete
    }

//    println(rulesToAdd)
//    println(rulesToDelete)
//    println("******")

    (ensembleActions ++ rulesToAdd) -- rulesToDelete
  }

}
