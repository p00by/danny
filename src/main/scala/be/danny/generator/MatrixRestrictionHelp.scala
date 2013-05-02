package be.danny.generator

import be.danny.structure.MatrixStructure

trait MatrixRestrictionHelp[E] {

  def rowRestriction(rowRestriction: Vector[E] => Boolean)(implicit matrix: MatrixStructure[E]) : Boolean = {
    for (row <- matrix.matrix) {
      if (!rowRestriction(row.flatten)) {
        return false;
      }
    }
    true;
  }
  
  def columnRestriction(columnRestriction: Vector[E] => Boolean)(implicit matrix: MatrixStructure[E]) : Boolean = {
    for (i <- 0 until matrix.matrix.size) {
      if (!columnRestriction(matrix.matrix.map(row => row(i)).flatten)) {
        return false;
      }
    }
    true;
  }
  
}