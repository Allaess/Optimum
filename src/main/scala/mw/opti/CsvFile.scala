package mw.opti

import java.io.{File, FileInputStream}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CsvFile(file: File) extends Iterator[(String, String, Int, String, String)] {
	val input = Source.fromInputStream(new FileInputStream(file)).getLines.drop(1)
	def hasNext = input.hasNext
	def next() = CsvFile.parseAll(CsvFile.record, input.next) match {
		case CsvFile.Success(obj, _) => obj
		case CsvFile.NoSuccess(msg, _) => throw CsvFile.FormatException(msg)
	}
	override def toString = s"CsvFile($file)"
}
object CsvFile extends RegexParsers {
	def apply(file: File) = new CsvFile(file)
	def record: Parser[(String, String, Int, String, String)] =
		field ~ "," ~ field ~ "," ~ field ~ "," ~ field ~ "," ~ field ^^ {
			case tribe1 ~ _ ~ squad1 ~ _ ~ weight ~ _ ~ squad2 ~ _ ~ tribe2 =>
				(tribe1, squad1, weight.toInt, squad2, tribe2)
		}
	def field: Parser[String] = quoted | unquoted
	def quoted: Parser[String] = """("[^"]*")+""".r ^^ (_.drop(1).dropRight(1).replaceAllLiterally("\"\"", "\""))
	def unquoted: Parser[String] = """[^,]*""".r
	class FormatException(msg: String) extends Exception(msg)
	object FormatException {
		def apply(msg: String) = new FormatException(msg)
		def unapply(e: FormatException) = Some(e.getMessage)
	}
}
