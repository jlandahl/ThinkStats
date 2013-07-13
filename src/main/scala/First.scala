object First extends App {
  val pregnancies = new Pregnancies("e:/Downloads", "2002FemPreg.dat.gz")
  println(pregnancies.records.length)
  val liveBirths = pregnancies.records.filter(_.outcome == 1)
  println("Live births: " + liveBirths.length)
  val (firstChild, nonFirst) = liveBirths.partition(_.birthord.getOrElse(0) == 1)
  val avgLenFirstChild = firstChild.foldLeft(0)((sum, p) => sum + p.prglength).toFloat / firstChild.length
  val avgLenNonFirst = nonFirst.foldLeft(0)((sum, p) => sum + p.prglength).toFloat / nonFirst.length
  println("Average length of pregnancy for first child: " + avgLenFirstChild)
  println("Average length of pregnancy (non-first): " + avgLenNonFirst)

  println("Standard deviation (first child): " + Stats.stddev(firstChild.map(_.prglength.toDouble)))
  println("Standard deviation (non-first): " + Stats.stddev(nonFirst.map(_.prglength.toDouble)))
}
