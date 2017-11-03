import org.scalatest._
import strong._

class UtilsTest extends FunSuite {

  // 1RM calculator
  test("1RM is calculated correctly for Set case class") {
    val set = Set("2017-10-10", "Squat","WO 1", 100.0, 3)
    assert("%6.2f".format(Utils.calculate1RM(set)) == "109.90")
  }

  test("1RM is calculated correctly for Best case class") {
    val best = Best("2017-10-10", "Squat", 120.0, 4, 1000.0)
    assert("%6.2f".format(Utils.calculate1RM(best)) == "135.84")
  }

  test("1RM is calculated correctly for with 1 rep") {
    val set = Set("2017-10-10", "Squat","WO 1", 100.0, 1)
    assert("%6.2f".format(Utils.calculate1RM(set)) == "100.00")
  }

  test("cleanString removes first and last character") {
    assert(Utils.cleanString("123456") == "2345")
  }

  test("Set is correctly parsed") {
    val raw = "2017-10-30 19:20:49,\"5/3/1 KB D1 W2\",\"Power Clean\",3,47.5,3,0.0,0,\"\""
    val set = Utils.parseSet(raw)
    assert(set.date == "2017-10-30")
    assert(set.wo_name == "5/3/1 KB D1 W2")
    assert(set.exercise == "Power Clean")
    assert(set.weight == 47.5)
    assert(set.reps == 3)
    assert(set == Set("2017-10-30","5/3/1 KB D1 W2","Power Clean",47.5,3))
  }



}
