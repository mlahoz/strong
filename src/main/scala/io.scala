import scala.io.Source
import java.io._
import strong._

package strong {

  object IO {

    def generateDataSet(filename: String): List[Set] = scala.io.Source.fromFile(filename)
      .getLines
      .drop(1)
      .map(Utils.parseSet)
      .toList

    def generateOutputForExercise(bests: List[Best], exercise: String): Unit = {
      val writer = new PrintWriter(new File(exercise.toLowerCase.replace(' ', '_') + ".csv"), "UTF-8")

      writer.write("date,max,1rm,total\n")
      bests.filter(_.exercise == exercise)
        .foreach(i => writer.write(i.date + "," + i.weight + "," + Utils.calculate1RM(i) + "," + i.total + "\n"))
      writer.close
    }
  }

}
