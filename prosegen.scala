
import io.Source

implicit class Piped[T](val self: T) {
  def >>>[U](receiver: T=>U): U = receiver(self)
  def -->(receiver: T=>Unit): Piped[T] = { receiver(self); this }
}

class SentenceStream(in: Source) extends Iterator[String] {
  
  val lengthLimit = 4
  
  val sentenceDelimiter = "[.;:!?]"
  
  val source = 
      in.mkString
      .split(sentenceDelimiter)
      .iterator
      .filter { sentence => words(sentence).length > lengthLimit }
  
  def hasNext: Boolean = source.hasNext
  
  def next: String = {
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
  }
  
  def trimWhiteSpace(sentence: String) =
    sentence.replaceAll("""\s+""", " ")
  
  def tagEnumeration(sentence: String) =
    sentence.replaceAll("""[0-9]*\([0-9a-z]+\)""", "__ENUMERATION")
  
  def tagDate(sentence: String) =
    sentence.replaceAll("""[0-9]+\s*-[0-9]+\s*-[0-9]+""", "__DATE")
  
  def tagAkta(sentence: String) =
    sentence
      .replaceAll("""[0-9]*[A-Z][0-9]+""", "__AKTANUM")
      .replaceAll("""[0-9]+/[0-9]+""", "__AKTANUM")
  
  def tagYear(sentence: String) =
    sentence.replaceAll("""[1|2][0-9][0-9][0-9]""", "__YEAR")
  
  def tagNumber(sentence: String) =
    sentence.replaceAll("""[0-9]+""", "__NUM")
  
  def tagRomanNumeral(sentence: String) =
    sentence.replaceAll("""[IVXM]+""", "__ROMNUM")
  
  def removeParens(sentence: String) =
    sentence.replaceAll("""\(|\)""", "")
  
  def removeDash(sentence: String) =
    sentence.replaceAll("—|-", "")
  
  def words(sentence: String) = {
    sentence.split("""\s+""")
  }
  
}

object ProseGenerator {
  
}

class ProseGenerator(trainingSet: Iterator[String]) {
  
  val rand = new util.Random(System.currentTimeMillis)
  
  
}





val sentences = new SentenceStream(Source.stdin)

val proses = new ProseGenerator(sentences)
