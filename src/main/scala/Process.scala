import java.time.LocalDateTime
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
    val choiceResult = readIntFromStdIn("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice2Result = readIntFromStdIn("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\nPlease enter your choice: ")
    val choice3Result = readIntFromStdIn("Sort By:\n1. Time\n2. Production Value\nPlease enter your choice: ")
    val choice4Result = readIntFromStdIn("Ascending or Descending:\n1. Ascending\n2. Descending\nPlease enter your choice: ")

    val currentTime = LocalDateTime.now().minusHours(24)

    val (startTime, endTime) = choice2Result.flatMap {
      case 1 => Success((currentTime.minusHours(1), currentTime))
      case 2 => Success((currentTime.minusDays(1), currentTime))
      case 3 => Success((currentTime.minusDays(7), currentTime))
      case 4 => Success((currentTime.minusDays(30), currentTime))
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

  def viewAnalysis(hydroData: List[List[String]], solarData: List[List[String]], windData: List[List[String]]): Unit = {
    print("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")

    val choiceResult = readIntFromStdIn("Please enter your choice: ")

    choiceResult match {
      case Success(choice) =>
        choice match {
          case 1 =>
            Analysis.mean(hydroData)
            Analysis.median(hydroData)
            Analysis.mode(hydroData)
            Analysis.range(hydroData)
            Analysis.midrange(hydroData)
          case 2 =>
            Analysis.mean(solarData)
            Analysis.median(solarData)
            Analysis.mode(solarData)
            Analysis.range(solarData)
            Analysis.midrange(solarData)
          case 3 =>
            Analysis.mean(windData)
            Analysis.median(windData)
            Analysis.mode(windData)
            Analysis.range(windData)
            Analysis.midrange(windData)
          case _ =>
            println("Invalid choice. Please try again.")
        }
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
    }
  }

  def checkData(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData]): Unit = {
    print("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")

    val choiceResult = readIntFromStdIn("Please enter your choice: ")

    choiceResult match {
      case Success(choice) =>
        val currentTime = LocalDateTime.now().minusHours(24)
        val startTime = currentTime.minusHours(24)
        val endTime = currentTime

        choice match {
          case 1 =>
            val data = Process.filterDataByTimePeriod(dataHydro, startTime, endTime)
            processDataForCheckData(data)
          case 2 =>
            val data = Process.filterDataByTimePeriod(dataSolar, startTime, endTime)
            processDataForCheckData(data)
          case 3 =>
            val data = Process.filterDataByTimePeriod(dataWind, startTime, endTime)
            processDataForCheckData(data)
          case _ =>
            println("Invalid choice. Please try again.")
        }
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
    }
  }

  private def processDataForCheckData(data: Seq[RenewableData]): Unit = {
    val (below1000Count, totalCount) = data.foldLeft((0, 0)) { case ((below1000Count, totalCount), data) =>
      val newBelow1000Count = if (data.powerProduction < 1000) below1000Count + 1 else below1000Count
      (newBelow1000Count, totalCount + 1)
    }
    println(s"Out of $totalCount values, it went under 1000 $below1000Count times.")
  }
}
