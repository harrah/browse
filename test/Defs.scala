package defs

object Defs
{
	class A
	class B[T] {
		def x(t: T): T = t
	}
	class C[T <: Y, Y]
	class D[T](t: T)
	class E extends B[Int]

	trait F
	trait G extends F
	class H extends A with G
	class I extends F

	private[defs] final class J private(val i: Int) extends H with G

	lazy val x = 3
	lazy val xx: Int = 3

	def q[T](t: T) = t
	def qq[T](t: T): List[T] = t :: Nil

	private[this] def z: Int = (new E).x(3)
}