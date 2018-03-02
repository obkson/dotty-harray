package harray

class Field[L <: String, +V](val label: L, val value: V) {
  override def equals(that : Any) = that match {
    case Field(l, v) => label == l && value == v
    case _ => false
  }
  override def toString = s"($label ->> $value)"
}

object Field {
  def apply[L <: String, V](label: L, value: V) = new Field[label.type, V](label, value)
  def unapply(f: Field[String, Any]): Option[(String, Any)] = Some((f.label, f.value))

  implicit def partiallyApplyField[L <: String](l: L): PartField[l.type] = new PartField[l.type](l)
  class PartField[L <: String](val l: L) {
    def ->>[V](v: V): Field[l.type, V] = new Field[l.type, V](l, v)
  }
}

class Record[L <: HList](val labels: Array[String], val values: Array[Any]) {
  override def equals(that: Any) = that match {
    case (r: Record[L]) => labels.sameElements(r.labels) && values.sameElements(r.values)
    case _ => false
  }
  override def toString = s"Record(${labels.zip(values).map{ case (l, v) => s"$l=$v" }.mkString(", ")})"
}

object Record {
  def apply[L <: HList](l: L) = new Record[L](l.labels.toArray, l.values.toArray)
}
