
import io.Source
import util.Random

type Phrase = List[String]

implicit class Piped[T](val self: T) {
  def >>>[U](receiver: T=>U): U = receiver(self)
  def -->(receiver: T=>Unit): Piped[T] = { receiver(self); this }
}

implicit class RandomlyPick[T](collection: Seq[T]) {
  def random()(implicit rand: Random) = 
      collection(rand.nextInt(collection.length))
}

class SentenceStream(in: Source) extends Iterator[Phrase] {
  
  private val lengthLimit = 4
  
  private val sentenceDelimiter = "[.;:!?]"
  
  private val source = 
      in.mkString // TODO process source in chunks. don't load all of it
      .split(sentenceDelimiter).iterator
      .filter { sentence => words(sentence).length > lengthLimit }
  
  def hasNext: Boolean = source.hasNext
  
  def next: Phrase = {
    source.next.trim
      .>>>(trimWhiteSpace)
      .>>>(tagEnumeration)
      .>>>(tagDate)
      .>>>(tagAkta)
      .>>>(tagYear)
      .>>>(tagNumber)
      .>>>(tagRomanNumeral)
      .>>>(removeQuotes)
      .>>>(removeParens)
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
    sentence.replaceAll("""\s+[VXM][IVXM]*\s+""", " __ROMNUM ")
  
  private def removeQuotes(sentence: String) =
    sentence.replace("\"", "")
  
  private def removeParens(sentence: String) =
    sentence.replaceAll("""\(|\)""", "")
  
  private def words(sentence: String) =
    sentence.split("""\s+""")
  
}

object ArtifactGenerator {
  
  val numbers = '0' to '9'
  val letters = 'a' to 'z'
  val upperLetters = 'A' to 'Z'
  val roman = "IVXM".toList
  
  def enumeration()(implicit rand: Random): String = {
    {
      if (rand.nextBoolean()) ""
      else (1 to 50).random().toString
    } + {
      if (rand.nextBoolean()) s"(${numbers.random()})"
      else s"(${letters.random()})"
    }
  }
  
  def date()(implicit rand: Random): String = {
    s"${(1 to 31).random()}-${(1 to 12).random()}-${(1948 to 2020).random()}"
  }
  
  def akta()(implicit rand: Random): String = {
    if (rand.nextBoolean()) s"A${(50 to 800).random()}"
    else s"${(1 to 50).random()}/${(1000 to 3000).random()}"
  }
  
  def year()(implicit rand: Random): String = {
    (1948 to 2020).random().toString
  }
  
  def number()(implicit rand: Random): String = {
    (10 to 2000).random().toString
  }
  
  def romanNumeral()(implicit rand: Random): String = {
    // Fuck it. If it looks Roman, it's Roman
    (-2 to rand.nextInt(4))
      .map { _ => roman.random()}
      .mkString
  }
  
  def reifyTags(word: String)(implicit rand: Random) = {
    word
      .replace("__ENUMERATION", enumeration())
      .replace("__DATE"       , date())
      .replace("__AKTANUM"    , akta())
      .replace("__YEAR"       , year())
      .replace("__NUM"        , number())
      .replace("__ROMNUM"     , romanNumeral())
  }
  
}

class ProseGenerator(trainingSet: Iterator[Phrase]) {
  import ArtifactGenerator._
  
  val order         = 2
  implicit val rand = new Random(System.currentTimeMillis)
  var frequency     = Map.empty[Phrase,List[String]]
  
  private val emptyStub = List.fill(order)("")
  
  trainingSet.foreach { sentence =>
    val padded = emptyStub ++ sentence :+ ""
    padded.sliding(order + 1).foreach(add)
  }
  
  private def add(phrase: Phrase): Unit = {
    assert(phrase.length == order.+(1))
    val next :: prevsReversed = phrase.reverse
    frequency.get(prevsReversed) match {
      case Some(allNext) => frequency += (prevsReversed -> (next :: allNext))
      case None          => frequency += (prevsReversed -> List(next))
    }
  }
  
  private def sample(stub: Phrase): String = {
    assert(stub.length == order)
    frequency.get(stub) match {
      case Some(allNext) => allNext.random()
      case _             => sample(List("", "", ""))
    }
  }
  
  def valid(in: Phrase): Boolean = {
    in.length > 5 &&
    in.map(_.length).sum < 140
  }
  
  def generate(): Phrase = {
    
    def grow(current: Phrase): Phrase = {
      sample(current.take(order)) match {
        case ""   => current
        case next => grow(next :: current)
      }
    }
    
    grow(List.fill(order)("")).reverse.drop(3).map(reifyTags)
      .>>>(gen => if (valid(gen)) gen else generate())
  }
  
}

val gen = new ProseGenerator(new SentenceStream(Source.fromFile("perlembagaan_persekutuan.my")) ++ new SentenceStream(Source.fromFile("novels.txt")))


