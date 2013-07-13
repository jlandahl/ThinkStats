import scala.util.control.Exception.catching
import scala.io.Source
import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream

case class Respondent(caseid: Int)

case class Pregnancy(
  caseid: Int,
  nbrnaliv: Option[Int],
  babysex: Option[Int],
  birthwgt_lb: Option[Int],
  birthwgt_oz: Option[Int],
  prglength: Int,
  outcome: Int,
  birthord: Option[Int],
  agepreg: Option[Double],
  finalwgt: Double,
  totalwgt_oz: Option[Int]
)

abstract class Table[A](dir: String, filename: String) {
  val records: List[A] = {
    val file = new File(dir, filename)
    val source = if (filename.endsWith(".gz")) {
      val fis = new FileInputStream(file)
      Source.fromInputStream(new GZIPInputStream(fis))
    } else
      Source.fromFile(file)
    source.getLines.map(readRecord).toList
  }
  def readRecord(line: String): A
}

class Pregnancies(dir: String, filename: String) extends Table[Pregnancy](dir, filename) {
  def readRecord(line: String): Pregnancy = {
    val NFE = classOf[NumberFormatException]
    val caseid      = line.slice(1, 12).trim.toInt
    val nbrnaliv    = catching(NFE) opt line.slice(21, 22).trim.toInt
    val babysex     = catching(NFE) opt line.slice(55, 56).trim.toInt
    val birthwgt_lb = catching(NFE) opt line.slice(56, 58).trim.toInt
    val birthwgt_oz = catching(NFE) opt line.slice(58, 60).trim.toInt
    val prglength   = line.slice(274, 276).trim.toInt
    val outcome     = line.slice(276, 277).trim.toInt
    val birthord    = catching(NFE) opt line.slice(277, 279).trim.toInt
    val agepreg     = catching(NFE) opt line.slice(283, 287).trim.toInt
    val finalwgt    = line.slice(422, 440).trim.toFloat

    // recodes
    val agepreg_100 = agepreg.map(_ / 100.0)
    val totalwgt_oz = for {
      lb <- birthwgt_lb if lb < 20
      oz <- birthwgt_oz if oz <= 16
    } yield lb * 16 + oz

    new Pregnancy(caseid, nbrnaliv, babysex, birthwgt_lb, birthwgt_oz, prglength, outcome, birthord, agepreg_100, finalwgt, totalwgt_oz)
  }
}

class Respondents(dir: String, filename: String) extends Table[Respondent](dir, filename) {
  def readRecord(line: String) = {
    val caseid = line.slice(1, 12).toInt
    new Respondent(caseid)
  }
}
