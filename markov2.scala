
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

type    Row = Array[Double]
type Matrix = Array[Row]
type    Mx3 = Array[Matrix]

class Markovyvy(trainingSet: Iterator[String]) {
  val transition: Mx3 = Array.fill(26, 26, 26)(0.0)
  val starting: Matrix = Array.fill(26, 26)(0.0)
  
  val random = new util.Random
  
  trainingSet.foreach { word =>
    starting(word.head - 'a')(word.tail.head - 'a') += 1
    word.toList.sliding(3).foreach { 
      case a :: b :: c :: Nil =>
        transition(a - 'a')(b - 'a')(c - 'a') += 1
      case _ => }
  }
  
  normalize(starting)
  transition.foreach { a => 
    a.foreach { b =>
      normalize(b) } }
  
  private def normalize(row: Row) {
    (1 to 25).foreach { i => row(i) = row(i) + row(i-1) }
    (0 to 25).foreach { i => row(i) /= row.last }
  }
  
  private def normalize(mx: Matrix) {
    mx.foreach { row => normalize(row) }
    (0 to 25).foreach { i =>
      (0 to 25).foreach { j =>
        mx(i)(j) += i
        mx(i)(j) /= 26 } }
  }
  
  private def sampleRow(row: Row) = {
    val dice = random.nextDouble
    row.indexWhere(dice < _)
  }
  
  private def sampleStarter: List[Int] = {
    val dice = random.nextDouble
    var i = 0
    var j = -1
    var found = false
    //println("dice: " + dice)
    while(!found) {
      j = j + 1
      i = i + (j / 26)
      j = j % 26
      //println("compare: " + starting(i)(j))
      //println("i: " + i + "   j: " + j)
      found = dice < starting(i)(j)
    }
    List(j, i)
  }
  
  def generate(length: Int): String = {
    val starter = sampleStarter
    //println(starter)
    val gen = (3 to length).foldLeft(starter) { 
      case (prevs, _) =>
        sampleRow((transition(prevs.tail.head)(prevs.head))) :: prevs
    }
    
    val word = gen.reverse
                  .map(_ + 'a')
                  .map(_.toChar)
                  .mkString
    if (word.contains('x') || word.contains('v')) {
      generate(length)
    } else {
      word
    }
  }
  
}


val wi = new WordIterator(System.in)

val conditioned = wi.filter(_.length > 2)
                    .map(_.toLowerCase)

val markov = new Markovyvy(conditioned)

//println(markov.starting.map(_.sum).sum)

(1 to 100).foreach { _ => println(markov.generate(10)) }
