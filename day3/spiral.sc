val input = 325489

def distance(i: Int): Int = {
  def findCapacity(capacity: Int, depth: Int): (Int, Int) = {
    val newCapacity = capacity + 8 * (depth+1)

    if (newCapacity >= i) {
      (i - capacity, depth + 1)
    } else {
      findCapacity(newCapacity, depth + 1)
    }
  }

  def walk(coord: (Int, Int), depth: Int, i: Int): (Int, Int) = {
    if (i == 0) coord
    else coord match {
      // Moving left along top
      case (x,y) if (y == depth) && x > (depth * -1) => walk((x-1, y), depth, i-1)
      // Moving right along bottom
      case (x,y) if (y == depth * -1) && x < depth => walk((x+1, y), depth, i-1)
      // Moving up right side
      case (x,y) if (x == depth) && y < depth => walk((x, y+1), depth, i-1)
      // Moving down left side
      case (x,y) if (x == depth * -1) && y > (depth * -1) => walk((x, y-1), depth, i-1)
    }
  }

  val (diff, depth) = findCapacity(1, 0)

  val (x, y) = walk((depth, (depth * -1) + 1), depth, diff - 1)

  Math.abs(x) + Math.abs(y)
}

distance(12) // (2,1) 3 steps
distance(23) // (0, -2) 2 steps
distance(1024) // ? 31 steps
distance(325489)