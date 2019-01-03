package mw.optimum.model

import java.io._

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.Try

trait Company {
  def tribes: List[Tribe]
  def tribe(name: String) = tribes.find(_.name == name)
  lazy val start = {
    val dummyTribes = for {
      tribe <- tribes
      squad <- tribe.squads
    } yield Tribe(squads = squad :: Nil)
    Company(dummyTribes)
  }
  def maxWeight(maxTribeSize: Int) = {
    val weights = for {
      tribe1 <- tribes
      tribe2 <- tribes
      if tribe1.name < tribe2.name && tribe1.size + tribe2.size <= maxTribeSize
    } yield tribe1 <-> tribe2
    Try(weights.max).getOrElse(0)
  }
  def mostCoupled(maxTribeSize: Int) = {
    val maxWeight = this.maxWeight(maxTribeSize)
    if (maxWeight == 0) Nil
    else for {
      tribe1 <- tribes
      tribe2 <- tribes
      if tribe1.name < tribe2.name
      weight = tribe1 <-> tribe2
      if weight == maxWeight && tribe1.size + tribe2.size <= maxTribeSize
    } yield (tribe1, weight, tribe2)
  }
  def optimum(maxTribeSize: Int): Company = mostCoupled(maxTribeSize) match {
    case Nil => this
    case (tribe1, _, tribe2) :: _ =>
      val newCompany = merge(tribe1, tribe2)
      newCompany.optimum(maxTribeSize)
  }
  def ignored(cutWeight: Int) = for {
    tribe1 <- tribes
    tribe2 <- tribes
    if tribe1.name < tribe2.name
    weight = tribe1 <-> tribe2
    if weight > cutWeight
  } yield (tribe1, weight, tribe2)
  lazy val score = {
    val weights = for {
      tribe1 <- tribes
      tribe2 <- tribes if tribe1.name < tribe2.name
    } yield tribe1 <-> tribe2
    Try(weights.sum).getOrElse(0)
  }
  def nextCoupleAndBestScore(maxTribeSize: Int): (Option[(Tribe, Int, Tribe)], Int) = {
    mostCoupled(maxTribeSize).headOption match {
      case None => (None, score)
      case link@Some((tribe1, _, tribe2)) =>
        val (_, score) = merge(tribe1, tribe2).nextCoupleAndBestScore(maxTribeSize)
        (link, score)
    }
  }
  def +(tribe: Tribe) = Company(tribe :: tribes)
  def -(tribe: Tribe) = Company(tribes.filter(_ != tribe))
  def merge(tribe1: Tribe, tribe2: Tribe) = {
    val newTribe = if (tribe1.size > tribe2.size) tribe1 ++ tribe2 else tribe2 ++ tribe1
    this - tribe1 - tribe2 + newTribe
  }
  def move(squad: Squad, toTribe: Tribe) = {
    val fromTribe = tribes.find(_.contains(squad)).get
    val newFromTribe = fromTribe - squad
    val newToTribe = toTribe + squad
    if (newFromTribe.isEmpty) this - fromTribe - toTribe + newToTribe
    else this - fromTribe - toTribe + newFromTribe + newToTribe
  }
  def saveTo(file: File) = {
    def format(field: String) = {
      if (field.contains(",") || field.contains("\""))
        s""""${field.replaceAllLiterally("\"", "\"\"")}""""
      else field
    }
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    output.write(",Tribes")
    for {
      tribe <- tribes
      _ <- tribe.squads
    } output.write(s",${format(tribe.name)}")
    output.newLine()
    output.write("Tribes,Squads")
    for {
      tribe <- tribes
      squad <- tribe.squads
    } output.write(s",${format(squad.name)}")
    output.newLine()
    for {
      tribe1 <- tribes
      squad1 <- tribe1.squads
    } {
      output.write(s"${format(tribe1.name)},${format(squad1.name)}")
      for {
        tribe2 <- tribes
        squad2 <- tribe2.squads
      } {
        val weight = squad1 <-> squad2
        if (weight == 0) output.write(",")
        else output.write(s",$weight")
      }
      output.newLine()
    }
    output.flush()
    output.close()
  }
  override def toString = s"Company(${tribes.size} tribes)"
}
object Company extends RegexParsers {
  val empty: Company = new Company {
    val tribes = Nil
  }
  def loadFrom(file: File): Company = {
    val input = Source.fromInputStream(new FileInputStream(file)).getLines.drop(1)
    val company = new Company.Mutable
    for (line <- input) {
      parseAll(record, line) match {
        case Success((t1, s1, w, s2, t2), _) =>
          val tribe1 = company.tribe(t1).getOrElse(new Tribe.Mutable(t1))
          val tribe2 = company.tribe(t2).getOrElse(new Tribe.Mutable(t2))
          val squad1 = tribe1.squad(s1).getOrElse(new Squad.Mutable(s1))
          val squad2 = tribe2.squad(s2).getOrElse(new Squad.Mutable(s2))
          squad1 += squad2 -> w
          squad2 += squad1 -> w
          tribe1 += squad1
          tribe2 += squad2
          company += tribe1
          company += tribe2
        case NoSuccess(msg, _) =>
          throw new FileFormatException(msg)
      }
    }
    company
  }
  class Mutable extends Company {
    var tribes = List.empty[Tribe.Mutable]
    override def tribe(name: String) = tribes.find(_.name == name)
    def +=(tribe: Tribe.Mutable) =
      if (!tribes.contains(tribe))
        tribes ::= tribe
  }
  class Immutable(val tribes: List[Tribe]) extends Company
  def apply(tribes: List[Tribe] = Nil): Company = new Immutable(tribes)
  def record: Parser[(String, String, Int, String, String)] =
    field ~ "," ~ field ~ "," ~ field ~ "," ~ field ~ "," ~ field ^^ {
      case t1 ~ _ ~ s1 ~ _ ~ w ~ _ ~ s2 ~ _ ~ t2 =>
        (t1, s1, w.toInt, s2, t2)
    }
  def field: Parser[String] = quoted | unquoted
  def quoted: Parser[String] ="""("[^"]*")+""".r ^^ (_.drop(1).dropRight(1).replaceAllLiterally("\"\"", "\""))
  def unquoted: Parser[String] ="""[^,]*""".r
  class FileFormatException(msg: String) extends Exception(msg)
}
