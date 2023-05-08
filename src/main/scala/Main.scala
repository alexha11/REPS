import com.github.tototoshi.csv._
import java.io.File


object Main extends App {
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
    val occurrences = values.groupBy(identity).mapValues(_.size)

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

  private val HydroCSV = CSVReader.open(new File("./src/main/scala/Hydro.csv"))
  private val SolarCSV = CSVReader.open(new File("./src/main/scala/Solar.csv"))
  private val WindCSV = CSVReader.open(new File("./src/main/scala/Wind.csv"))
  private val HydroData = HydroCSV.all()
  private val SolarData = SolarCSV.all()
  private val WindData = WindCSV.all()

  private var running = true

  while (running) {
    println("Menu:")
    println("1. View Hydro Data")
    println("2. View Solar Data")
    println("3. View Wind Data")
    println("4. Exit")

    print("Please enter your choice: ")
    val choice = scala.io.StdIn.readInt()

    choice match {
      case 1 =>
        mean(HydroData)
        median(HydroData)
        mode(HydroData)
        range(HydroData)
        midrange(HydroData)
      case 2 =>
        mean(SolarData)
        median(SolarData)
        mode(SolarData)
        range(SolarData)
        midrange(SolarData)
      case 3 =>
        mean(WindData)
        median(WindData)
        mode(WindData)
        range(WindData)
        midrange(WindData)
      case 4 =>
        running = false
        println("Goodbye!")
      case _ =>
        println("Invalid choice. Please try again.")
    }
    println()
  }
}
