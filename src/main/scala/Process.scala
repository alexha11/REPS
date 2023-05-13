import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object Process {

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ")

  def readIntFromStdIn(prompt: String): Try[Int] = {
    Try {
      print(prompt)
      scala.io.StdIn.readInt()
    }
  }

  def createRenewableData(data: Seq[Array[String]]): Seq[RenewableData] = {
    data.map { line =>
      RenewableData(
        LocalDateTime.parse(line(0), dateTimeFormatter),
        LocalDateTime.parse(line(1), dateTimeFormatter),
        line(2).toDouble
      )
    }
  }

  private def filterDataByTimePeriod(data: Seq[RenewableData], startTime: LocalDateTime, endTime: LocalDateTime): Seq[RenewableData] = {
    data.filter(d => d.startTime.isAfter(startTime) && d.endTime.isBefore(endTime))
  }

  def viewData(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData]): Unit = {
    val currentTime = LocalDateTime.now().minusHours(24)

    val choiceResult = readIntFromStdIn("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice2Result = readIntFromStdIn("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Search for a date\nPlease enter your choice: ")

    val (startTime, endTime) = choice2Result.flatMap {
      case 1 => Success((currentTime.minusHours(1), currentTime))
      case 2 => Success((currentTime.minusDays(1), currentTime))
      case 3 => Success((currentTime.minusDays(7), currentTime))
      case 4 => Success((currentTime.minusDays(30), currentTime))
      case 5 =>
        val searchDate = readDateFromStdIn()
        Success((searchDate.atStartOfDay(), searchDate.plusDays(1).atStartOfDay().minusSeconds(1)))
      case _ => Failure(new IllegalArgumentException("Invalid filter choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }

    val filteredData = choiceResult.flatMap {
      case 1 => Success(filterDataByTimePeriod(dataHydro, startTime, endTime))
      case 2 => Success(filterDataByTimePeriod(dataSolar, startTime, endTime))
      case 3 => Success(filterDataByTimePeriod(dataWind, startTime, endTime))
      case _ => Failure(new IllegalArgumentException("Invalid plant choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }

    val choice3Result = readIntFromStdIn("Sort By:\n1. Time\n2. Production Value\nPlease enter your choice: ")
    val choice4Result = readIntFromStdIn("Ascending or Descending:\n1. Ascending\n2. Descending\nPlease enter your choice: ")

    val sortedFilteredData = choice3Result.flatMap {
      case 1 => Success(filteredData.sortBy(_.startTime))
      case 2 => Success(filteredData.sortBy(_.powerProduction))
      case _ => Failure(new IllegalArgumentException("Invalid sort choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }

    val sortedFilteredDataOrder = choice4Result.flatMap {
      case 1 => Success(sortedFilteredData)
      case 2 => Success(sortedFilteredData.reverse)
      case _ => Failure(new IllegalArgumentException("Invalid sort choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }

    sortedFilteredDataOrder.foreach { data =>
      val start = data.startTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
      println(s"Time: $start  |  Value: ${data.powerProduction}")
    }
  }

  private def readDateFromStdIn(): LocalDate = {
    println("Enter the date (yyyy-MM-dd): ")
    val input = scala.io.StdIn.readLine()
    LocalDate.parse(input, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
  }

  def viewAnalysis(hydroData: List[List[String]], solarData: List[List[String]], windData: List[List[String]]): Unit = {
    val choiceResult = readIntFromStdIn("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice2Result = readIntFromStdIn("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Search for a date\nPlease enter your choice: ")

    val sortedData = (choiceResult, choice2Result) match {
      case (Success(choice), Success(choice2)) =>
        val data = (choice, choice2) match {
          case (1, _) => hydroData
          case (2, _) => solarData
          case (3, _) => windData
          case _ => List.empty[List[String]]
        }
        val filteredData = filterDataByChoice(choice2, data)
        sortData(filteredData)

      case _ =>
        println("Invalid input. Please enter valid choices.")
        return
    }

    displaySortedData(sortedData)
  }

  private def filterDataByChoice(choice: Int, data: List[List[String]]): List[List[String]] = {
    val currentTime = LocalDateTime.now().minusHours(24)

    choice match {
      case 1 => data.filter { line =>
        val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
        dateTime.isAfter(currentTime.minusHours(1)) && dateTime.isBefore(currentTime)
      }
      case 2 => data.filter { line =>
        val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
        dateTime.isAfter(currentTime.minusDays(1)) && dateTime.isBefore(currentTime)
      }
      case 3 => data.filter { line =>
        val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
        dateTime.isAfter(currentTime.minusDays(7)) && dateTime.isBefore(currentTime)
      }
      case 4 => data.filter { line =>
        val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
        dateTime.isAfter(currentTime.minusDays(30)) && dateTime.isBefore(currentTime)
      }
      case 5 =>
        val searchDate = readDateFromStdIn()
        data.filter { line =>
          val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
          dateTime.isAfter(searchDate.atStartOfDay()) && dateTime.isBefore(searchDate.plusDays(1).atStartOfDay().minusSeconds(1))
        }
      case _ =>
        println("Invalid choice. Returning empty data.")
        List.empty[List[String]]
    }
  }

  private def sortData(data: List[List[String]]): List[List[String]] = {
    data.sortBy(line => LocalDateTime.parse(line(0), dateTimeFormatter))
  }

  private def displaySortedData(sortedData: List[List[String]]): Unit = {
    if (sortedData.isEmpty) {
      println("No data to display.")
    } else {
      println("Sorted Data:")
      sortedData.foreach { line =>
        val dateTime = LocalDateTime.parse(line(0), dateTimeFormatter)
        val formattedDateTime = dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
        val value = line(3).toDouble
        println(s"Date-Time: $formattedDateTime  |  Value: $value")
      }

      val values = sortedData.map(line => line(1).toDouble)
      val mean = Analysis.mean(sortedData)
      val median = Analysis.median(sortedData)
      val mode = Analysis.mode(sortedData)
      val range = Analysis.range(sortedData)
      val midrange = Analysis.midrange(sortedData)
      val minimum = Analysis.minimum(sortedData)

      println(s"\nAnalysis:")
      println(s"Mean: $mean")
      println(s"Median: $median")
      println(s"Mode: $mode")
      println(s"Range: $range")
      println(s"Midrange: $midrange")
      println(s"Minimum value: $minimum")
    }
  }

  def checkData(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData], powerPlants: List[PowerPlant]): Unit = {
    print("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")

    val choiceResult = readIntFromStdIn("Please enter your choice: ")

    choiceResult match {
      case Success(choice) =>
        val currentTime = LocalDateTime.now().minusHours(24)
        val startTime = currentTime.minusHours(24)
        val endTime = currentTime

        choice match {
          case 1 =>
            val powerPlant = powerPlants.find(_.name == "Hydro")
            powerPlant.foreach { plant =>
              if (plant.shutdown) {
                println("Warning: Hydro power plant is shut down.")
              }
            }
            val data = filterDataByTimePeriod(dataHydro, startTime, endTime)
            processDataForCheckData(data, 1500)
          case 2 =>
            val data = filterDataByTimePeriod(dataSolar, startTime, endTime)
            processDataForCheckData(data, 250)
          case 3 =>
            val data = filterDataByTimePeriod(dataWind, startTime, endTime)
            processDataForCheckData(data, 1500)
          case _ =>
            println("Invalid choice. Please try again.")
        }
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
    }
  }

  private def processDataForCheckData(data: Seq[RenewableData], Threshold: Int): Unit = {
    val (belowThreshold, totalCount) = data.foldLeft((0, 0)) { case ((belowThreshold, totalCount), data) =>
      val newBelowThreshold = if (data.powerProduction < Threshold) belowThreshold + 1 else belowThreshold
      (newBelowThreshold, totalCount + 1)
    }
    println(s"Out of $totalCount values scanned in the past 24 hours, $belowThreshold values were under the acceptable threshold of $Threshold.")
  }
}
