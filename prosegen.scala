
import io.Source
import util.Random

implicit class Piped[T](val self: T) {
  def >>>[U](receiver: T=>U): U = receiver(self)
  def -->(receiver: T=>Unit): Piped[T] = { receiver(self); this }
}

implicit class RandomlyPick[T](collection: Seq[T]) {
  def random()(implicit rand: Random) = 
      collection(rand.nextInt(collection.length))
}

class SentenceStream(in: Source) extends Iterator[List[String]] {
  
  private val lengthLimit = 4
  
  private val sentenceDelimiter = "[.;:!?]"
  
  private val source = 
      in.mkString
      .split(sentenceDelimiter)
      .iterator
      .filter { sentence => words(sentence).length > lengthLimit }
  
  def hasNext: Boolean = source.hasNext
  
  def next: List[String] = {
    source.next.trim
      .>>>(trimWhiteSpace)
      .>>>(tagEnumeration)
      .>>>(tagDate)
      .>>>(tagAkta)
      .>>>(tagYear)
      .>>>(tagNumber)
      .>>>(tagRomanNumeral)
      .>>>(removeParens)
      .>>>(removeDash)
      .>>>(words).toList
  }
  
  private def trimWhiteSpace(sentence: String) =
    sentence.replaceAll("""\s+""", " ")
  
  private def tagEnumeration(sentence: String) =
    sentence.replaceAll("""[0-9]*\([0-9a-z]+\)""", "__ENUMERATION")
  
  private def tagDate(sentence: String) =
    sentence.replaceAll("""[0-9]+\s*-[0-9]+\s*-[0-9]+""", "__DATE")
  
  private def tagAkta(sentence: String) =
    sentence
      .replaceAll("""[0-9]*[A-Z][0-9]+""", "__AKTANUM")
      .replaceAll("""[0-9]+/[0-9]+""", "__AKTANUM")
  
  private def tagYear(sentence: String) =
    sentence.replaceAll("""[1|2][0-9][0-9][0-9]""", "__YEAR")
  
  private def tagNumber(sentence: String) =
    sentence.replaceAll("""[0-9]+""", "__NUM")
  
  private def tagRomanNumeral(sentence: String) =
    sentence.replaceAll("""[VXMvxm][IVXMivxm]*""", "__ROMNUM")
  
  private def removeParens(sentence: String) =
    sentence.replaceAll("""\(|\)""", "")
  
  private def removeDash(sentence: String) =
    sentence.replaceAll("—|-", "")
  
  private def words(sentence: String) =
    sentence.split("""\s+""")
  
}

object ArtifactGenerator {
  
  val numbers = '0' to '9'
  val letters = 'a' to 'z'
  val upperLetters = 'A' to 'Z'
  val roman = "IVXML".toList
  
  def enumeration(implicit rand: Random): String = {
    {
      if (rand.nextBoolean()) ""
      else (rand.nextInt(50) + 1)
    } + {
      if (rand.nextBoolean()) s"(${numbers.random()})"
      else s"(${letters.random()})"
    }
  }
  
  def date(implicit rand: Random): String = {
    s"${(1 to 30).random()}-${(1 to 12).random()}-${(1948 to 2020).random()}"
  }
  
  def akta(implicit rand: Random): String = {
    if (rand.nextBoolean()) {
      s"A${(50 to 800).random()}"
    } else {
      s"${(1 to 50).random()}/${(1000 to 3000).random()}"
    }
  }
  
  def year(implicit rand: Random): String = {
    (1948 to 2020).random().toString
  }
  
  def number(implicit rand: Random): String = {
    (10 to 2000).random().toString
  }
  
  def romanNumeral(implicit rand: Random): String = {
    // fuck it. If it looks Roman, it's Roman
    (-2 to rand.nextInt(4))
      .map { _ => roman.random()}
      .mkString
  }
  
}

object ProseGenerator {
  
}

class ProseGenerator(trainingSet: Iterator[String]) {
  
  val rand = new Random(System.currentTimeMillis)
  var wordIndex = List.empty[String]
  var frequency = Map.empty[(Int,Int,Int),Int]
  
}




/*
val sentences = new SentenceStream(Source.stdin)

val proses = new ProseGenerator(sentences)
*/
