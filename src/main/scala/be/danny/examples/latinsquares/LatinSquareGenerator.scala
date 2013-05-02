package be.danny.examples.latinsquares

import be.danny.generator.MatrixGenerator
import be.danny.generator.MatrixRestrictionHelp

object LatinSquareGenerator extends MatrixRestrictionHelp[Int] {
  
  def main(args: Array[String]) = {
    val n = 4;
    
    def unique = {x: Vector[Int] => x.toSet.size == x.size}
    
    val generator = MatrixGenerator(elements = 1 to n)
    	.addRestriction(rowRestriction(unique)(_))
    	.addRestriction(columnRestriction(unique)(_))
    	
    generator.generate(n).foreach { x =>
      println(x + "\n")
    }
  }
}