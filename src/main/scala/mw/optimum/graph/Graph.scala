package mw.optimum.graph

import mw.optimum.model.Company

import scala.collection.mutable

trait Graph {
  outer =>
  def tribeBubbles: List[TribeBubble]
  def squadBubbles: List[SquadBubble]
  def nextLayout: Option[Graph] = {
    val vecs = mutable.Map.empty[Bubble, Vector]
    for {
      bubble1 <- tribeBubbles
      bubble2 <- tribeBubbles if bubble2 != bubble1
    } {
      val vec = vecs.getOrElse(bubble1, Vector.zero) + (bubble1 correction bubble2)
      vecs += bubble1 -> vec
    }
    for {
      bubble1 <- squadBubbles
      bubble2 = bubble1.parent
      if bubble2.tribe.size > 1
    } {
      val vec = vecs.getOrElse(bubble1, Vector.zero) + (bubble1 correction bubble2)
      vecs += bubble1 -> vec
    }
    val moving = (false /: vecs) { case (acc, (_, vec)) =>
      acc || vec.size > 3
    }
    if (moving) Some {
      new Graph {
        val tribeBubbles = outer.tribeBubbles.map { bubble =>
          bubble + vecs.getOrElse(bubble, Vector.zero)
        }
        val squadBubbles = outer.squadBubbles.map { bubble =>
          bubble + vecs.getOrElse(bubble, Vector.zero)
        }
      }
    } else {
      None
    }
  }
}
object Graph {
  val empty: Graph = new Graph {
    val tribeBubbles = Nil
    val squadBubbles = Nil
    override val nextLayout = None
  }
  def apply(company: Company, maxTribeSize: Int): Graph = {
    val links = company.mostCoupled(maxTribeSize)
    new Graph {
      val tribeBubbles = for (tribe <- company.tribes) yield {
        val linked = links.filter { case (t1, _, _) =>
          t1 == tribe
        }.map { case (_, w, t2) =>
          t2 -> w
        }.toMap
        TribeBubble(tribe, linked)
      }
      val squadBubbles = for {
        bubble <- tribeBubbles
        tribe = bubble.tribe
        if tribe.size > 1
        squad <- tribe.squads
      } yield {
        SquadBubble(squad, bubble)
      }
    }
  }
}
