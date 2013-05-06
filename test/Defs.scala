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

	var v: Int = 3
	v = 2

	lazy val x = v
	lazy val xx: Int = 3

	def q[T](t: T) = t
	def qq[T](t: T): List[T] = t :: Nil
	def qq[T](t: T, z: T, b: Boolean): T = if(b) t else z

	private[this] def z: Int = (new E).x(3)

	def nested = {
		val v = x
		def xx: Int = {
			val v = q(2)
			val x = {
				v + xx
			}
			x + v
		}

		// TODO: scheme that handles unnamed, nested scopes
		{
			val v = xx

			{
				val xx = 3
				v + xx
			}
		}
	}
}