import defs.Defs._

object Ref {
  val a = new A
  val b = new defs.Defs.B[Int]

  val q = b.x(3)

  val k = NestedDefs.K.K2
}
