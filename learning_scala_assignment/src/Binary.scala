import breeze.plot._

object Binary {

  def toBinary(x: Int, bits: Int): String = {
    if (x == 0) {
      "0" * bits
    }

    else if (x == 1) {
      "0" * (bits-1) + "1"
    }

    else toBinary(x/2, bits - 1) + (x % 2).toString
  }

  def weight(b: String): Int = {
    try {
      b.toArray.groupBy(identity).mapValues(_.size)('1')
    }
    catch {
      case _: Throwable => return 0;
    }
  }

  def main() = {
    val bin = toBinary(37, 8)
    print(weight(bin))

    val xs = Range(0,1025)
    val ys = xs.map(x=>weight(toBinary(x,8)))

    val fig = Figure()
    val plt = fig.subplot(0)
    plt += plot(xs,ys)
    fig.refresh()
  }
}
