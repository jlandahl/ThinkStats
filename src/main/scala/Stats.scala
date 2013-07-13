object Stats {
  def mean(seq: Seq[Double]) = {
    seq.reduceLeft(_ + _) / seq.length
  }

  def variance(seq: Seq[Double]) = {
    val dev2 = for (x <- seq; mu = mean(seq)) yield Math.pow(x - mu, 2)
    mean(dev2)
  }

  def stddev(seq: Seq[Double]) = {
    Math.sqrt(variance(seq))
  }
}

class Histogram[A](seq: Seq[A]) {
  val hash = seq.groupBy(x => x).mapValues(_.length).withDefaultValue(0)
  def apply(v: A) = hash(v)
  def freq(v: A) = hash(v)
  def values = hash.keys
  def mode = hash.maxBy(e => e._2)._1
  override def toString = hash.keys.toString
}

object Histogram {
  def apply[A](seq: A*) = new Histogram(seq)
}