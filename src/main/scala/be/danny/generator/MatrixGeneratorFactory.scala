package be.danny.generator

import be.danny.structure.MatrixStructure

case class MatrixGeneratorFactory[E] (restrictions: List[MatrixStructure[E] => Boolean]) {

	def addRestriction(restriction: MatrixStructure[E] => Boolean) = copy(restrictions = restriction :: this.restrictions)
  
}