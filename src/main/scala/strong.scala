import strong._

object Strong {

  def main(args: Array[String]): Unit = {

    def bestSets(sets: List[Set], exercise: String): List[Set] =
      sets.filter(_.exercise == exercise)
        .groupBy(_.weight)
        .map( item => item._2.maxBy(_.reps) )
        .toList

    def formatWeight(w: Double): String = "%6.2f".format(w)
    def formatReps(r: Int): String = "%2d".format(r)
    def formatString(s: String, l: Int): String = s.padTo(l, ' ')

    val dataset = IO.generateDataSet("strong.csv")

    val exercises = List("Squat", "Bench Press", "Deadlift", "Shoulder Press")
    val max_exercise_name_length = exercises.map(_.length).max

    def formatExercise(e: String): String = formatString(e, max_exercise_name_length)

    // Best sets per exercise
    for (e <- exercises) {
      println("****** Best " + e + " Sets ******")
      Utils.bestSets(dataset, e)
        .foreach(set => println(e + ": " + formatWeight(set.weight) + "kg x " +  formatReps(set.reps) + " reps on " + set.date + " (1RM: " + formatWeight(Utils.calculate1RM(set)) + " kg)"))
      println("=============================================")
    }

    val best_series = Utils.bestSeriesWithTotalWeight(dataset)

    println("****** Best Total Weight Series ******")
    best_series.filter(i => exercises.contains(i._2))
      .foreach( item => println(formatExercise(item._2) + " serie with a total of " + item._3 + " kg on " + item._1))
    println("=============================================")

    // Best 1RM
    println("****** Best 1RM ******")
    for (e <- exercises) {
      val best_1rm = Utils.get1RMForExercise(dataset, e)
      println(formatExercise(e) + " 1RM of " + formatWeight(best_1rm._2) + " kg (" + formatWeight(best_1rm._1.weight) + " kg x " + formatReps(best_1rm._1.reps) + ") on " + best_1rm._1.date)
    }
    println("=============================================")

    // 1RM, max weight and total over time
    val bests_over_time = dataset.groupBy(i => (i.date, i.exercise))
      .map( item => {
        val max_set = item._2.maxBy(_.weight)
        val total = item._2.foldLeft(0.0)((acc, i) => (acc + i.weight * i.reps))
        Best(item._1._1, item._1._2, max_set.weight, max_set.reps, total)
      })
      .toList
      .sortBy(_.date)

    for (e <- exercises) {
      IO.generateOutputForExercise(bests_over_time, e)
    }
  }

}
