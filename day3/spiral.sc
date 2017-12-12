val input = 325489

def distance(i: Int): (Int, Int) = {
  def findCapacity(capacity: Int, depth: Int): (Int, Int) = {
    println(s"cap: ${capacity} dep: ${depth}")
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
  println(s"diff: ${diff} depth: ${depth}")

  walk((depth, (depth * -1) + 1), depth, diff)
}

distance(12)
distance(23)
distance(1024)
distance(325489)