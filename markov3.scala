
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

object Markovyvyvy {
  
  type     Row = Array[Double]
  type  Matrix = Array[Row]
  type     Mx3 = Array[Matrix]
  type     Mx4 = Array[Mx3]

  implicit class Mx3Ops(mx3: Mx3) {
    def arrayMutate(mutator: (Double,Int,Int,Int)=>Double) {
      for (k <- 0 until mx3.length;
           j <- 0 until mx3.length;
           i <- 0 until mx3.length) {
        mx3(i)(j)(k) = mutator(mx3(i)(j)(k), i, j, k)
      }
    }
  }
  
  implicit class MatrixOps(mx: Matrix) {
    def arrayMutate(mutator: (Double,Int,Int)=>Double) {
      for (j <- 0 until mx.length; i <- 0 until mx.length) {
        mx(i)(j) = mutator(mx(i)(j), i, j)
      }
    }
  }
  
  def normalize(row: Row) = {
    val normed = row.clone
    (1 until normed.length).foreach { i => normed(i) += normed(i-1) }
    (0 until normed.length).foreach { i => normed(i) /= normed.last }
    normed
  }
  
}

class Markovyvyvy(trainingSet: Iterator[String]) {
  import Markovyvyvy._
  
  val transition: Mx4 = Array.fill(26, 26, 26, 26)(0.0)
  val starting: Mx3   = Array.fill(26, 26, 26)(0.0)
  val random = new util.Random
  
  trainingSet.foreach { word =>
    val first3 = word.take(3).map(_.toInt - 'a')
    starting(first3(0))(first3(1))(first3(2)) += 1
    
    word.toList.sliding(4).foreach { 
      case a :: b :: c :: d :: Nil =>
        transition(a - 'a')(b - 'a')(c - 'a')(d - 'a') += 1
      case _ => }
  }
  
  starting.arrayMutate { (v, i, j, k) =>
    if (i == 0 && j == 0 && k == 0) {
      v
    } else if (i == 0 && j == 0) {
      v + starting(25)(25)(k - 1)
    } else if (i == 0) {
      v + starting(25)(j - 1)(k)
    } else {
      v + starting(i - 1)(j)(k)
    }
  }
  //println(s"Last value of starting: ${starting(25)(25)(25)}")
  starting.arrayMutate { (v, i, j, k) =>
    v / starting(25)(25)(25)
  }
  
  private def sampleRow(row: Row): Int = {
    val dice = random.nextDouble
    val ix = normalize(row).indexWhere(dice < _)
    if (ix == -1) 25 else ix
  }
  
  def sampleStarter: List[Int] = {
    val dice = random.nextDouble
    //println(s"random number: $dice")
    var found = false
    var ii = 0
    var jj = 0
    var kk = 0
    starting.arrayMutate { (v, i, j, k) =>
      if (dice <= v && !found) {
        //println(s"found match. dice: $dice value: $v")
        found = true
        ii = i
        jj = j
        kk = k
      }
      v
    }
    List(ii, jj, kk)
  }
  
  def generate(length: Int): String = {
    assert(length > 3)
    val starter = sampleStarter.reverse
    //println(starter)
    val gen = (4 to length).foldLeft(starter) { 
      case (prevs, _) =>
        sampleRow((transition(prevs(2))(prevs(1))(prevs(0)))) :: prevs
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

val conditioned = wi.filter(_.length > 3)
                    .map(_.toLowerCase)

val markov = new Markovyvyvy(conditioned)

(1 to 100).foreach { _ => println(markov.generate(10)) }

