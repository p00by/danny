package be.danny.structure

case class MatrixStructure[E] (val matrix: Vector[Vector[Option[E]]]) {

  def this(size: Int) {
    this(Vector.fill(size)(Vector.fill(size)(None)))
  }
  
  def isFull = findFirstEmptyPosition.isEmpty
  
  def append(replacement: E) : MatrixStructure[E] = {
    findFirstEmptyPosition match {
      case Some((x,y)) =>
        val newRow = matrix(x).updated(y, Some(replacement))
	    MatrixStructure(matrix.updated(x, newRow))
      case _ => throw new IllegalArgumentException("This is already full")
    }
  }
  
  private def findFirstEmptyPosition() : Option[(Int, Int)] = {
    {
      for (
        x <- 0 until matrix.size;
        y <- 0 until matrix.size;
        if (matrix(x)(y).isEmpty)
      ) yield (x, y) 
    }.headOption
  }
  
  override def toString() : String = {
    matrix.map(_.map(_.getOrElse("?")).mkString(" ")).mkString("\n")
  }
  
}