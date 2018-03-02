package harray

sealed trait HList {
  def labels: List[String]
  def values: List[Any]
}
final case class HCons[+H, +T <: HList](val head: H, val tail: T) extends HList {
  def :*:[H2](h: H2): HCons[H2, this.type] = HCons(h, this)
  def labels = head match {
    case Field(label, _) => label :: tail.labels
    case _ => tail.labels
  }
  def values = head match {
    case Field(_, value) => value :: tail.values
    case _ => tail.values
  }
}
sealed trait HNil extends HList // make HNil a proper type
final case object HNil extends HNil {
  def :*:[H2](h: H2): HCons[H2, HNil] = HCons(h, this)
  def labels = List()
  def values = List()
}
