
import java.util.Scanner
import java.io.InputStream

class WordIterator(in: InputStream) extends Iterator[String] {
  val wordRegex = "[a-zA-Z]+" 
  private val scanner = new Scanner(in)
  private var current = scanner.findWithinHorizon(wordRegex, 0)
  
  def hasNext: Boolean = (current != null)
  
  def next: String = {
    val tbr = current
    current = scanner.findWithinHorizon(wordRegex, 0)
    tbr
  }
  
}

val wi = new WordIterator(System.in)

wi.filter(_.length > 1)
  .map(_.toLowerCase)
  .take(100)
  .foreach(println(_))

