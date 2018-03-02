import harray._, Field._, Selector._

object Main {

  def main(args: Array[String]): Unit = {

    val r = Record("foo"->>42 :*: "bar" ->> "baz" :*: HNil)
    println(r)

    val v1: Int = r.get["foo"]
    val v2: String = r.get["bar"]
    println(v1)
    println(v2)

    val s = Record("foo"->>42 :*: "bar" ->> "baz" :*: HNil)
    val t = Record("f00"->>42 :*: "bar" ->> "baz" :*: HNil)
    println(r == s)
    println(r == t)

    val rr = Record("a"->>1 :*: "b"->>(Record("c"->>3 :*: HNil)) :*: HNil)
    println(rr)
    println(rr.get["b"].get["c"])
  }
}
