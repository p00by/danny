package be.danny.examples.latinsquares

case class LatinSquare(val matrix: Vector[Vector[Option[Int]]]) {
  
  def this(size: Int) {
    this(Vector.fill(size)(Vector.fill(size)(None)))
  }
  
  def isComplete() : Boolean = {
    findFirstEmptyPosition.isEmpty
  }
  
  def children() : List[LatinSquare] = {
    val optionalEmptyPosition = findFirstEmptyPosition();
    optionalEmptyPosition match {
      case Some((x, y)) => 
        val possibilities = findPossibilities(x, y);
	    
	    for (
	      possibility <- possibilities
	    ) yield (transform(x, y, possibility))
      case _ => Nil
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
  
  private def findPossibilities(x: Int, y: Int) : List[Int] = {
    val rowElements = for (
      row <- 0 until matrix.size;
      el <- matrix(x)(row).toList
    ) yield (el) 
    
    val columnElements = for (
      column <- 0 until matrix.size;
      el <- matrix(column)(y).toList
    ) yield (el)
    
    val exclude = rowElements.toSet ++ columnElements.toSet
    
    val possibilities = (1 to matrix.size)
    
    possibilities.filterNot(exclude.contains(_)).toList
  }
  
  private def transform(x: Int, y: Int, replacement: Int) : LatinSquare = {
    val newRow = matrix(x).updated(y, Some(replacement))
    LatinSquare(matrix.updated(x, newRow))
  }
  
  override def toString() : String = {
    matrix.map(_.map(_.getOrElse("?")).mkString(" ")).mkString("\n")
  }
  
}