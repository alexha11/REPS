import Main.powerPlants
import com.github.tototoshi.csv.CSVReader

import java.io.File
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
  
  // Added a method to convert data from List[List[String]] to Seq[RenewableData]
  def createRenewableData(data: Seq[Array[String]]): Seq[RenewableData] = {
    data.map { line =>
      RenewableData(
        LocalDateTime.parse(line(0), dateTimeFormatter),
        LocalDateTime.parse(line(1), dateTimeFormatter),
        line(2).toDouble
      )
    }
  }
  
  // Added a method that can filter data by a specific time period
  private def filterDataByTimePeriod(data: Seq[RenewableData], startTime: LocalDateTime, endTime: LocalDateTime): Seq[RenewableData] = {
    data.filter(d => d.startTime.isAfter(startTime) && d.endTime.isBefore(endTime))
  }
  
  def viewData(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData]): Unit = {
    val currentTime = LocalDateTime.now().minusHours(24)
    
    val choiceResult = readIntFromStdIn("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice2Result = readIntFromStdIn("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Search for a date\nPlease enter your choice: ")
   
    // Added a new choice to filter by a specific date
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
    
    // Added a new choice to filter by a specific plant
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
    
    // Added a new choice to sort by a specific field
    val sortedFilteredData = choice3Result.flatMap {
      case 1 => Success(filteredData.sortBy(_.startTime))
      case 2 => Success(filteredData.sortBy(_.powerProduction))
      case _ => Failure(new IllegalArgumentException("Invalid sort choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }
    
    // Added a new choice to sort by ascending or descending
    val sortedFilteredDataOrder = choice4Result.flatMap {
      case 1 => Success(sortedFilteredData)
      case 2 => Success(sortedFilteredData.reverse)
      case _ => Failure(new IllegalArgumentException("Invalid sort choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }
    // Added a new choice to display the data in a specific format
    sortedFilteredDataOrder.foreach { data =>
      val start = data.startTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
      println(s"Time: $start  |  Value: ${data.powerProduction}")
    }
  }
  
  // Added a new method to read a date from the standard input
  private def readDateFromStdIn(): LocalDate = {
    println("Enter the date (yyyy-MM-dd): ")
    val input = scala.io.StdIn.readLine()
    LocalDate.parse(input, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
  }
  
  // Added a new method to display the analysis of the data
  def viewAnalysis(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData]): Unit = {
    val choiceResult = readIntFromStdIn("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    val choice2Result = readIntFromStdIn("Filter By:\n1. Last hour\n2. Last day\n3. Last week\n4. Last month\n5. Search for a date\nPlease enter your choice: ")
    val currentTime = LocalDateTime.now().minusHours(24)
   
    // Added a new choice to filter by a specific date
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
    
    // Added a new choice to filter by a specific plant
    val filteredData = choiceResult.flatMap {
      case 1 => Success(filterDataByTimePeriod(dataHydro, startTime, endTime))
      case 2 => Success(filterDataByTimePeriod(dataSolar, startTime, endTime))
      case 3 => Success(filterDataByTimePeriod(dataWind, startTime, endTime))
      case _ => Failure(new IllegalArgumentException("Invalid plant choice"))
    }.getOrElse {
      println("Invalid choice. Please try again.")
      return
    }
    displaySortedData(filteredData.map(data => List(data.startTime.toString, data.endTime.toString, data.powerProduction.toString)).toList)
  }
  
  // Added a new method to display the analysis of the data
  private def displaySortedData(sortedData: List[List[String]]): Unit = {
    if (sortedData.isEmpty) {
      println("No data to display.")
    } else {
      println("Data: Anaylsis\n")
      val mean = Analysis.mean(sortedData)
      val median = Analysis.median(sortedData)
      val mode = Analysis.mode(sortedData)
      val range = Analysis.range(sortedData)
      val midrange = Analysis.midrange(sortedData)
      val minimum = Analysis.minimum(sortedData)
    }
  }
  
  // Control the power plant's renewable energy sources
  def controlPlant(powerPlants: List[PowerPlant]) = {
    val choiceResult = readIntFromStdIn("Control the power plant's renewable energy sources:\n1. Shut down a plant\n2. Restart a plant\nPlease enter your choice: ")
    choiceResult match {
      case Success(1) =>
        shutDownPowerPlant(powerPlants)
      case Success(2) =>
        restartPowerPlant(powerPlants)
      case Failure(_) =>
        println("Invalid choice. Please try again.")
    }
    
    // Shutdown a power plant
    def shutDownPowerPlant(powerPlants: List[PowerPlant]): Unit = {
      val choiceResult = Process.readIntFromStdIn("Power Plants:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter the power plant to shut down: ")
      
      choiceResult match {
        case Success(choice) if choice >= 1 && choice <= 3 =>
          val selectedPowerPlant = powerPlants(choice - 1)
          if (!selectedPowerPlant.shutdown) {
            selectedPowerPlant.shutdown = true
            println(s"${selectedPowerPlant.name} power plant has been shut down.")
          } else {
            println(s"${selectedPowerPlant.name} power plant is already shut down.")
          }
        case Success(_) =>
          println("Invalid power plant choice.")
        case Failure(_) =>
          println("Invalid input. Please enter a valid power plant choice.")
      }
    }
    
    // Restart a power plant
    def restartPowerPlant(powerPlant: List[PowerPlant]): Unit = {
      val choiceResult = Process.readIntFromStdIn("Power Plants:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter the power plant to restart: ")
      
      choiceResult match {
        case Success(choice) if choice >= 1 && choice <= 3 =>
          val selectedPowerPlant = powerPlants(choice - 1)
          if (selectedPowerPlant.shutdown) {
            selectedPowerPlant.shutdown = false
            println(s"${selectedPowerPlant.name} power plant has been restarted.")
          } else {
            println(s"${selectedPowerPlant.name} power plant is already opened.")
          }
        case Success(_) =>
          println("Invalid power plant choice.")
        case Failure(_) =>
          println("Invalid input. Please enter a valid power plant choice.")
      }
    }
  }
  
  // Check the data of the renewable energy sources in the power plant
  def checkData(dataHydro: Seq[RenewableData], dataSolar: Seq[RenewableData], dataWind: Seq[RenewableData], powerPlants: List[PowerPlant]): Unit = {
    print("Plant:\n1. Hydro\n2. Solar\n3. Wind\nPlease enter your choice: ")
    
    val choiceResult = readIntFromStdIn("Please enter your choice: ")
    
    choiceResult match {
      case Success(choice) =>
        val currentTime = LocalDateTime.now().minusHours(24)
        val startTime = currentTime.minusHours(24)
        val endTime = currentTime
        
        // Checking if the equipment is malfunctioning and the power plant is shut down
        choice match {
          case 1 =>
            val powerPlant = powerPlants.find(_.name == "Hydro")
            powerPlant.foreach { plant =>
              if (plant.shutdown) {
                println("Warning: Hydro power plant is shut down.")
              } else {
                if (!isEquipmentMalfunction("hydro.csv")) {
                  println("Warning: Hydro power plant is malfunctioning.")
                }
                else {
                  println("All the equipment of Hydro power plant is working properly.")
                }
              }
            }
            
            val data = filterDataByTimePeriod(dataHydro, startTime, endTime)
            processDataForCheckData(data, 1500)
          case 2 =>
            val powerPlant = powerPlants.find(_.name == "Solar")
            powerPlant.foreach { plant =>
              if (plant.shutdown) {
                println("Warning: Solar power plant is shut down.")
              } else {
                if (!isEquipmentMalfunction("solar.csv")) {
                  println("Warning: Solar power plant is malfunctioning.")
                }
                else {
                  println("All the equipment of Solar power plant is working properly.")
                }
              }
            }
            
            val data = filterDataByTimePeriod(dataSolar, startTime, endTime)
            processDataForCheckData(data, 250)
          case 3 =>
            val powerPlant = powerPlants.find(_.name == "Wind")
            powerPlant.foreach { plant =>
              if (plant.shutdown) {
                println("Warning: Wind power plant is shut down.")
              } else {
                if (!isEquipmentMalfunction("wind.csv")) {
                  println("Warning: Wind power plant is malfunctioning.")
                }
                else {
                  println("All the equipment of Wind power plant is working properly.")
                }
              }
            }
            
            val data = filterDataByTimePeriod(dataWind, startTime, endTime)
            processDataForCheckData(data, 1500)
          case _ =>
            println("Invalid choice. Please try again.")
        }
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
    }
  }
  
  //Checks if the data is below the threshold and prints the result
  private def processDataForCheckData(data: Seq[RenewableData], Threshold: Int): Unit = {
    val (belowThreshold, totalCount) = data.foldLeft((0, 0)) { case ((belowThreshold, totalCount), data) =>
      val newBelowThreshold = if (data.powerProduction < Threshold) belowThreshold + 1 else belowThreshold
      (newBelowThreshold, totalCount + 1)
    }
    println(s"Out of $totalCount values scanned in the past 24 hours, $belowThreshold values were under the acceptable threshold of $Threshold.")
  }
  
  // Checks if the equipment is malfunctioning
  private def isEquipmentMalfunction(fileName: String): Boolean = {
    val file = new File(fileName)
    if (file.exists()) {
      val reader = CSVReader.open(file)
      val data = reader.all()
      reader.close()
      if (data.nonEmpty) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }
}