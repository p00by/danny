package be.danny.generator

import be.danny.structure.MatrixStructure
import be.danny.structure.MatrixStructure
import be.danny.structure.MatrixStructure

case class MatrixGenerator[E] (elements: Seq[E], restrictions: List[MatrixStructure[E] => Boolean] = Nil) {

  def addRestriction(restriction: MatrixStructure[E] => Boolean) = copy(restrictions = restriction :: this.restrictions)

  def generate(n: Int): Stream[MatrixStructure[E]] = {
    val start = new MatrixStructure[E](n);
    generate(start)
  }
  
  def generate(current: MatrixStructure[E]) : Stream[MatrixStructure[E]] = {
    val valid = restrictions.foldLeft(true)((s, el) => s && el(current))
    if (valid) {
      if (current.isFull) {
        Stream(current);
      } else {
        elements.toStream.flatMap(element => generate(current.append(element)))
      }
    } else {
      Stream.Empty;
    }
  }
  
}