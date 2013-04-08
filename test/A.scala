object A {
	val b = {
		// FIXME: d is not linked. This only happens with a type ascription.
val f = (d: Double) =>
B(d.toInt)
		f(3.0)
	}

	val b2 = {
		import b.xyz
		B(xyz)
	}

	// FIXME: case should be typed (doesn't get the right token position, so it is disabled)
	val c = (113: Int) match {
		case 50 => 19
		case 40 => 33.0
		case _ => true
	}

	val arr = List(3,4,5).toArray[Int]

	def highlightInfix(q: Q) = q m2 (q m -3, q.m(-4))

}