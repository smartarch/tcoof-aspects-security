package tcof

import tcof.InitStages.InitStages
import org.chocosolver.solver.variables.SetVar

import scala.reflect.ClassTag

trait WithMembers[+MemberType] extends WithConfig {

  private[tcof] def allMembersVarName: String

  private[tcof] def allMembers: Members[MemberType]

  private[tcof] var allMembersVar: SetVar = null

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        allMembersVar = _solverModel.setVar(allMembersVarName, Array.empty[Int], 0 until allMembers.size toArray)
      case _ =>
    }
  }

  def cardinality: Integer = _solverModel.IntegerIntVar(allMembersVar.getCard)

  def contains(member: Any): Logical = some(x => LogicalBoolean(x == member))
  def containsOtherThan(member: Any): Logical = some(x => LogicalBoolean(x != member))
  def containsOnly(member: Any): Logical = all(x => LogicalBoolean(x == member))

  def sum(fun: MemberType => Integer): Integer = _solverModel.sumBasedOnMembership(allMembersVar, allMembers.values.map(fun))

  def all(fun: MemberType => Logical): Logical =
    _solverModel.forAllSelected(allMembers.values.map(fun), allMembersVar)

  def some(fun: MemberType => Logical): Logical =
    _solverModel.existsSelected(allMembers.values.map(fun), allMembersVar)

  def disjointAfterMap[OtherMemberType, T: ClassTag](funThis: MemberType => T , other: WithMembers[OtherMemberType], funOther: OtherMemberType => T): Logical = {
    val thisValues = allMembers.map(funThis)
    val otherValues = other.allMembers.map(funOther)

    val allMap = thisValues.toSet.union(otherValues.toSet).zipWithIndex.toMap

    val thisVar = _solverModel.setVar(Array.empty[Int], thisValues.map(allMap(_)).toArray)
    val otherVar = _solverModel.setVar(Array.empty[Int], otherValues.map(allMap(_)).toArray)

    val thisMembers = thisValues.zipWithIndex
    for ((member, idx) <- thisMembers) {
      _solverModel.ifOnlyIf(_solverModel.member(idx, allMembersVar), _solverModel.member(allMap(member), thisVar))
    }

    val otherMembers = otherValues.zipWithIndex
    for ((member, idx) <- otherMembers) {
      _solverModel.ifOnlyIf(_solverModel.member(idx, other.allMembersVar), _solverModel.member(allMap(member), otherVar))
    }

    LogicalBoolVar(_solverModel.disjoint(thisVar, otherVar).reify())
  }



  def foreachBySelection(forSelected: MemberType => Unit, forNotSelected: MemberType => Unit): Unit = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    for ((member, idx) <- allMembers.values.zipWithIndex) {
      if (selection.contains(idx))
        forSelected(member)
      else
        forNotSelected(member)
    }
  }

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.values.zipWithIndex.map{case (member, idx) => (selection.contains(idx), member)}
  }

  def selectedMembers: Iterable[MemberType] = {
    val values = allMembers.values.toIndexedSeq
    for (idx <- _solverModel.solution.getSetVal(allMembersVar)) yield values(idx)
  }
}

