package mw.opti

import java.io.File

class Company(var tribes: Map[String, Tribe]) {
	def tribe(name: String) = tribes.get(name) match {
		case Some(tribe) => tribe
		case None =>
			val tribe = Tribe(name)
			tribes += name -> tribe
			tribe
	}
	def tribe(squad: Squad) = tribes.find { case (_, tribe) =>
		tribe.squads.values.toSet.contains(squad)
	}.map(_._2).get
	def +=(tribe: Tribe) = tribes += tribe.name -> tribe
	def -=(tribe: Tribe) = tribes -= tribe.name
	def score = {
		val coll = for {
			(_, tribe1) <- tribes
			(_, tribe2) <- tribes
			if tribe1.name < tribe2.name
		} yield tribe1 <-> tribe2
		coll.sum
	}
	def maxWeight = {
		val coll = for {
			(_, tribe1) <- tribes
			(_, tribe2) <- tribes
			if tribe1.name < tribe2.name
		} yield tribe1 <-> tribe2
		coll.max
	}
	def start = {
		val company = Company()
		for {
			(_, tribe) <- tribes
			(_, squad) <- tribe.squads
		} {
			val tribe = company.tribe(squad.name)
			tribe += squad
		}
		company
	}
	override def toString = s"Company(${tribes.size} tribes)"
}
object Company {
	def apply() = new Company(Map.empty)
	def load(file: File) = {
		val company = new Company(Map.empty)
		val input = CsvFile(file)
		for ((t1, s1, weight, s2, t2) <- input) {
			val tribe1 = company.tribe(t1)
			val squad1 = tribe1.squad(s1)
			val tribe2 = company.tribe(t2)
			val squad2 = tribe2.squad(s2)
			squad1 += squad2 -> weight
		}
		company
	}
}
