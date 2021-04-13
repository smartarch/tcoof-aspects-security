package tcof

import scala.collection.mutable

abstract class Model {
  private var _universe = Seq.empty[Component]
  val _aspects = mutable.ListBuffer.empty[EnsembleAspect]

  def components_= (univ: Seq[Component]): Unit = _universe = univ
  def components: Seq[Component] = _universe

  protected def root[EnsembleType <: RootEnsemble](builder: => EnsembleType): RootEnsembleAnchor[EnsembleType] = {
    new RootEnsembleAnchor(builder _, _aspects)
  }

  import scala.reflect.runtime.universe._
  def getSubjectType[T <: EnsembleAspect: TypeTag](obj: T) = {
    typeTag[T].tpe.member(TypeName("SubjectType")).asType.toType
//    ttype.decls.sorted(0).asMethod.returnType
  }
  def getObjectType[T <: EnsembleAspect: TypeTag](obj: T) = {
    typeTag[T].tpe.member(TypeName("ObjectType")).asType.toType
//    ttype.decls.sorted(0).asMethod.returnType
  }

  def aspect[K <: EnsembleAspect: TypeTag](asp: K): Unit = {
    val subjectType = getSubjectType(asp)
    val objectType = getObjectType(asp)
    asp.addTypes(subjectType, objectType)
    _aspects += asp
  }

}
