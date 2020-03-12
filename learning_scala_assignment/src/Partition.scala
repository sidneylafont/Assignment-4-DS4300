class Partition {

  def moved(records: Int, startN: Int, endN: Int): Double = {
    var numReassigned = 0
    for( i <- 0 to records) {
      val start = i % startN
      val end = i % endN
      if (start == end) {
        numReassigned += 1
      }
    }

    numReassigned / records.toDouble
  }

  def main() = {
    moved(1000000, 100, 107)
  }

}
