
import java.util.Scanner
import java.io.InputStream

class WordIterator(in: InputStream) extends Iterator[String] {
  val wordRegex = "[a-zA-Z]+" 
  private val scanner = new Scanner(in)
  private var current = scanner.findWithinHorizon(wordRegex, 0)
  
  def hasNext: Boolean = (current != null)
  
  def next: String = {
    val next = current
    current = scanner.findWithinHorizon(wordRegex, 0)
    next
  }
  
}

type Matrix = Array[Array[Double]]
type    Row = Array[Double]

class Markovy(trainingSet: Iterator[String]) {
  val transition: Matrix = Array.fill(26, 26)(0.0)
  val starting: Row      = Array.fill(26)(0.0)
  
  val random = new util.Random
  
  trainingSet.foreach { word =>
    starting(word.head - 'a') += 1
    word.toList.sliding(2).foreach { 
      case a :: b :: Nil =>
        transition(a - 'a')(b - 'a') += 1
      case _ => }
  }
  
  normalize(starting)
  transition.foreach { row => normalize(row) }
  
  private def normalize(row: Row) {
    (1 to 25).foreach { i => row(i) = row(i) + row(i-1) }
    (0 to 25).foreach { i => row(i) /= row.last }
  }
  
  private def sample(row: Row) = {
    val dice = random.nextDouble
    row.indexWhere(dice < _)
  }
  
  def generate(length: Int): String = {
    val first = sample(starting)
    
    val gen = (2 to length).foldLeft(List(first)) { 
      case (prevs, _) =>
        sample(transition(prevs.head)) :: prevs
    }
    
    gen.reverse
       .map(_ + 'a')
       .map(_.toChar)
       .mkString
  }
  
}

val wi = new WordIterator(System.in)

val conditioned = wi.filter(_.length > 1)
                    .map(_.toLowerCase)

val markov = new Markovy(conditioned)

(1 to 100).foreach { _ => println(markov.generate(10)) }
