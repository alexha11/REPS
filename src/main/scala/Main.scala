import com.github.tototoshi.csv._
import java.io.File
import sys.process._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Main extends App {

  val today = LocalDate.now().format(DateTimeFormatter.ISO_DATE)
  val windURL = s"https://api.fingrid.fi/v1/variable/75/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time="+ today +"T00%3A00%3A00%2B02%3A00"
  val windCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$windURL' -o Wind.csv"
  val solarURL = s"https://api.fingrid.fi/v1/variable/248/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time=" + today + "T00%3A00%3A00%2B02%3A00"
  val solarCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$solarURL' -o Solar.csv"
  val hydroURL = s"https://api.fingrid.fi/v1/variable/191/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time=" + today + "T00%3A00%3A00%2B02%3A00"
  val hydroCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$hydroURL' -o Hydro.csv"

  val windExitCode = windCMD.!
  val solarExitCode = solarCMD.!
  val hydroExitCode = hydroCMD.!

  if (windExitCode == 0 || solarExitCode == 0 || hydroExitCode == 0) {
    println("CSV file downloaded and stored.")
  } else {
    println("Failed to download the CSV file.")
  }

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

  private val HydroCSV = CSVReader.open(new File("Hydro.csv"))
  private val SolarCSV = CSVReader.open(new File("Solar.csv"))
  private val WindCSV = CSVReader.open(new File("Wind.csv"))
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
