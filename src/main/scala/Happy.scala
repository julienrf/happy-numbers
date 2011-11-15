object Happy {

  def sumSquare(ds: List[Int]): Int = ds map (d => d * d) sum

  def digits(n: Int): List[Int] = if (n < 10) List(n) else (n % 10) :: digits(n / 10)

  def happy(n: Int, visited: List[Int] = Nil): Boolean = sumSquare(digits(n)) match {
    case 1 => true
    case n => if (visited contains n) false else happy(n, n :: visited)
  }

  def main(args: Array[String]) {
    val happys = Set(1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193, 203, 208, 219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310, 313, 319, 320, 326, 329, 331, 338, 356, 362, 365, 367, 368, 376, 379, 383, 386, 391, 392, 397, 404, 409, 440, 446, 464, 469, 478, 487, 490, 496)

    (1 to 500).toList map (n => (n, happy(n))) foreach { case (i, p) => { 
      val passes = p == (happys contains i)
      if(!passes)
        println("happy fails for " + i)
    }}
  }
}
