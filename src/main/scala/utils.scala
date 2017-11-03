import strong._

package strong {

  object Utils {

    def cleanString(s: String): String = s.tail.dropRight(1)

    def parseSet(line: String): Set =
      // Date,Workout Name,Exercise Name,Set Order,kg,Reps,km,Seconds,Notes
      line.split(",") match {
        case Array(date, wo, ex, set, w, reps, km, sec, n) => Set(date.split(" ")(0), cleanString(wo), cleanString(ex), w.toDouble, reps.toInt)
      }

    def calculate1RM(set: Item): Double = set.reps match {
      case 1 => set.weight
      case _ => set.weight * (1 + 0.033 * set.reps)
    }

    def bestSets(dataset: List[Set], exercise: String): List[Set] =
      dataset.filter(_.exercise == exercise)
        .groupBy(_.weight)
        .map( item => item._2.maxBy(_.reps) )
        .toList
        .sortBy(_.weight)
        .reverse


    def bestSeriesWithTotalWeight(dataset: List[Set]): List[(String, String, Double)] =
      dataset.groupBy(i => (i.date, i.exercise))
        .map( item => {
          val total = item._2.foldLeft(0.0)((acc, i) => (acc + i.weight * i.reps))
          (item._1._1, item._1._2, total)
        })
        .groupBy(_._2)
        .map( item => item._2.maxBy(_._3) )
        .toList

    def get1RMForExercise(dataset: List[Set], exercise: String): (Set, Double) = bestSets(dataset, exercise)
        .map(set => (set, Utils.calculate1RM(set)))
        .maxBy(_._2)

  }

}
