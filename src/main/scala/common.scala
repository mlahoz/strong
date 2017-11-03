package strong {

  trait Item {
    val weight: Double
    val reps: Int
  }

  case class Set(
    date: String,
    wo_name: String,
    exercise: String,
    weight: Double,
    reps: Int
  ) extends Item

  case class Best(
    date: String,
    exercise: String,
    weight: Double,
    reps: Int,
    total: Double
  ) extends Item

}
