object Analysis {
  def mean(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val mean = numericValues.sum / numericValues.length
      println(s"Mean: $mean")
    } else {
      println("No numeric data available.")
    }
  }

  def median(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption).sorted

    if (numericValues.nonEmpty) {
      val length = numericValues.length
      val median =
        if (length % 2 == 0)
          (numericValues(length / 2 - 1) + numericValues(length / 2)) / 2.0
        else
          numericValues(length / 2)

      println(s"Median: $median")
    } else {
      println("No numeric data available.")
    }
  }

  def mode(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val occurrences = values.groupBy(identity).view.mapValues(_.size).toMap


    if (occurrences.nonEmpty) {
      val maxCount = occurrences.values.max
      val modes = occurrences.filter(_._2 == maxCount).keys.toList

      if (modes.length == 1) {
        println(s"Mode: ${modes.head}")
      } else {
        println("Multiple modes found. Choosing one mode:")
        println(s"Mode: ${modes.head}")
      }
    } else {
      println("No data available.")
    }
  }

  def range(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val minValue = numericValues.min
      val maxValue = numericValues.max
      val rangeValue = maxValue - minValue

      println(s"Range: $rangeValue")
    } else {
      println("No numeric data available.")
    }
  }

  def midrange(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val minValue = numericValues.min
      val maxValue = numericValues.max
      val midrangeValue = (minValue + maxValue) / 2

      println(s"Midrange: $midrangeValue")
    } else {
      println("No numeric data available.")
    }
  }

  def minimum(data: List[List[String]]): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    val values = data.flatMap(_.tail) // Exclude the header row
    val numericValues = values.flatMap(str => scala.util.Try(str.toDouble).toOption)

    if (numericValues.nonEmpty) {
      val minValue = numericValues.min
      println(s"Minimum Value: $minValue")
    } else {
      println("No numeric data available.")
    }
  }

}
