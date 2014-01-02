object NestedDefs
{
	object K {
		object K2
		class K3
		lazy val k4 = {
			def c = "asdf"
			lazy val k5 = "jkl"
			object K6 {
				class K7
			}
			c + k5 + (new K6.K7).hashCode
		}
	}

  // test case class usage within a method
  def foo(x : Int) = {
    case class Result(i : Int)
    Result(x)
  }
}