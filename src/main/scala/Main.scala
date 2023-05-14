import Process.readIntFromStdIn

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.util.{Failure, Success}

case class RenewableData(startTime: LocalDateTime, endTime: LocalDateTime, powerProduction: Double)

case class PowerPlant(name: String, var shutdown: Boolean = false)

object Main extends App {
  private val (hydroData, solarData, windData) = DataReader.readData()
  
  // Create a list of power plants for shutdown
  private val powerPlants = List(
    PowerPlant("Hydro", shutdown = false),
    PowerPlant("Solar", shutdown = false),
    PowerPlant("Wind", shutdown = false)
  )

  // Drop the first row of data (column names)
  val (dataHydro, dataSolar, dataWind) = (
      Process.createRenewableData(hydroData.drop(1).map(_.toArray)),
      Process.createRenewableData(solarData.drop(1).map(_.toArray)),
      Process.createRenewableData(windData.drop(1).map(_.toArray))
  )
  @tailrec
  private def runMenu(): Unit = {
    println("Menu:")
    println("1. View Data")
    println("2. View Data Analysis")
    println("3. Check Errors in the past 24 hours")
    println("4. Control a plant")
    println("5. Exit")

    val choiceResult = Process.readIntFromStdIn("Please enter your choice: ")
    
    choiceResult match {
      case Success(choice) =>
        choice match {
          case 1 =>
            Process.viewData(dataHydro, dataSolar, dataWind)
          case 2 =>
            Process.viewAnalysis(dataHydro, dataSolar, dataWind)
          case 3 =>
            Process.checkData(dataHydro, dataSolar, dataWind, powerPlants)
          case 4 =>
            Process.controlPlant(powerPlants)
          case 5 =>
            println("Goodbye!")
          case _ =>
            println("Invalid choice. Please try again.")
        }
        println()
      
        if (choice != 5) // Updated condition to exit the loop if the choice is 5 (Exit)
          runMenu()
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
        runMenu()
    }
  }
  runMenu()
  
}