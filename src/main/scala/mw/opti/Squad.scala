package mw.opti

class Squad(val name: String, var dependencies: Map[Squad, Int]) {
	lazy val bubble = Bubble(this)
	def <->(that: Squad) = dependencies.getOrElse(that, 0)
	def +=(pair: (Squad, Int)) = dependencies += pair
	override def toString = s"Squad($name)"
}
object Squad {
	def apply(name: String) = new Squad(name, Map.empty)
}
