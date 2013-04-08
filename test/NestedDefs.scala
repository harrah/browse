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
}