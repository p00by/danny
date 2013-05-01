package be.danny.examples.latinsquares

class LatinSquareGenerator {

  def generateLatinSquares(size: Int): Unit = {
    val start = new LatinSquare(size);
    val count = generateLatinSquares(start)
    
    println("Generated " + count + " latin squares")
  }
  
  private def generateLatinSquares(parent: LatinSquare): Int = {
    if (parent.isComplete) {
      println(parent)
      println
      1
    } else {
      val counts = for (child <- parent.children) yield(generateLatinSquares(child))
      counts.foldLeft(0)((sum, y) => sum + y)
    }
  }
  
}

object LatinSquareGenerator {
  
  def main(args: Array[String]) = {
    def generator = new LatinSquareGenerator()    
    generator.generateLatinSquares(5);
  }
  
}