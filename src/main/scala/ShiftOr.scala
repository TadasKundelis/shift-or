// Time complexity O(m + n)
object ShiftOr extends App {
  def search(input: String, pattern: String): Int = {
    val patternLength = pattern.length
    if (patternLength == 0 || patternLength > 31) {
      return -1
    }

    var state = 0L
    val masks = buildMasks(pattern)
    for (index <- 0 until input.length) {
      state = (state << 1) + 1
      state &= masks.getOrElse(input(index), 0L)
      if ((state & 1 << (patternLength - 1)) != 0) {
        return index - patternLength + 1
      }
    }
    return -1
  }

  private def buildMasks(pattern: String): Map[Char, Long] = {
    pattern.zipWithIndex.foldLeft(Map.empty[Char, Long]) {
      case (acc, (curr, index)) =>
        acc ++ Map(curr -> (acc.getOrElse(curr, 0L) | 1 << index))
    }
  }

  val input = "a" * 100000000 + "b"
  val pattern = "a" * 30 + "b"
  val startTime = System.currentTimeMillis
  val res = search(input, pattern)
  val endTime = System.currentTimeMillis
  println(endTime - startTime) // 170ms
}
