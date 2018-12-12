package mw.optimum.model

trait Squad {
  def name: String
  def weights: Map[Squad, Int]
  def <->(that: Squad) = weights.getOrElse(that, 0)
  override def toString = s"Squad($name)"
}
object Squad {
  class Mutable(val name: String) extends Squad {
    var weights = Map.empty[Squad, Int]
    def +=(pair: (Squad, Int)) = weights.get(pair._1) match {
      case Some(w) => weights += pair._1 -> (pair._2 + w)
      case None => weights += pair
    }
  }
}
