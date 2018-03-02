package harray

trait Selector[F <: String, L <: HList] {
  type V <: Any
  val index: Int
}

object Selector {

  type SelectorAux[F <: String, L <: HList, VOut] = Selector[F, L] {
    type V = VOut
  }

  implicit def headSelector[F <: String, T <: HList, VOut]: SelectorAux[F, HCons[Field[F, VOut], T], VOut]
  = new Selector[F, HCons[Field[F, VOut], T]] {
      type V = VOut
      val index = 0
  }

  implicit def iterSelector[F <: String, H, T <: HList, VOut](
    implicit ts: SelectorAux[F, T, VOut])
    : SelectorAux[F, HCons[H, T], VOut]
  = new Selector[F, HCons[H, T]] {
      type V = VOut
      val index = ts.index + 1
  }

  implicit class RecordOps[L <: HList, VOut](val r: Record[L]) extends AnyVal {
    def get[F <: String](implicit sel: SelectorAux[F, L, VOut]): VOut = r.values(sel.index).asInstanceOf[VOut]
  }
}
