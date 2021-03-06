
object WordIterator {
  
  val wordRegex = ('a' to 'z') ++ ('A' to 'Z') toSet
  
  def apply(lines: Iterator[String]): Iterator[String] = {
    lines
        .flatMap { _.split("\\s+|-").toIterator }
        .map { _.filter(wordRegex) }
  }
  
}

object WordGenerator {
  
  type Row = Array[Double]
  type Mx2 = Array[Row]
  type Mx3 = Array[Mx2]
  
  def normalize(row: Row): Row = {
    val t = row.foldLeft(List(0.0)) { (prev, num) => (prev.head + num) :: prev }
               .reverse.tail
    t.map { _ / t.last }
     .toArray
  }
  
  def wordToIndices(word: String): List[Int] = {
    word.map { c => if (c == ' ') 26 else c - 'a' }
        .toList
  }
  
  def indicesToWord(ixs: List[Int]): String = {
    ixs.map { i => if (i == 26) ' ' else i.+('a').toChar }
       .mkString
  }
  
  def sampleRow(row: Row)(implicit random: util.Random): Int = {
    val dice = random.nextDouble
    normalize(row).indexWhere(dice < _)
  }
  
  def isValid(word: String) = {
    !(word.contains('x') || word.contains('v')) && word.length > 4
  }
}

class WordGenerator(trainingSet: Iterator[String]) {
  import WordGenerator._
  
  private val transition: Mx3 = Array.fill(27, 27, 27)(1.0)
  private implicit val random = new util.Random
  
  trainingSet.foreach { word =>
    addWord("  " ++ word.take(1))
    addWord(" " ++ word.take(2))
    word.sliding(3).foreach { addWord(_) }
    addWord(word.takeRight(2) ++ " ")
  }
  
  private def addWord(word: String) {
    assert(word.length == 3)
    val indices = wordToIndices(word)
    transition(indices(0))(indices(1))(indices(2)) += 1
  }
  
  private def sampleStarter(): List[Int] = {
    val first  = sampleRow(transition(26)(26))
    val second = sampleRow(transition(26)(first))
    List(first, second)
  }
  
  def generate(): String = {
    
    def grow(current: List[Int]): List[Int] = {
      val next = sampleRow(transition(current(1))(current(0)))
      if (next == 26) current
      else grow(next :: current)
    }
    
    val starter = sampleStarter()
    val gen = indicesToWord(grow(starter.reverse).reverse)
    if (!isValid(gen))
      generate()
    else
      gen
  }
  
  def score(word: String): Double =
    ("  " + word + " ").sliding(3)
      .map(wordToIndices)
      .map { case List(a, b, c) => transition(a)(b)(c) }
      .sum./(word.length)

}

val wi = WordIterator(io.Source.stdin.getLines)

val conditioned = wi.filter(_.length > 3)
                    .map(_.toLowerCase)

val markov = new WordGenerator(conditioned)

println("Markov chain second order, with stastistical ending")
(1 to 100).foreach { _ => println(markov.generate()) }

