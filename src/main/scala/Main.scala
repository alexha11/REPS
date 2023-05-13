import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.util.{Failure, Success}

case class RenewableData(startTime: LocalDateTime, endTime: LocalDateTime, powerProduction: Double)
case class PowerPlant(name: String, var shutdown: Boolean)

object Main extends App {
  private val (hydroData, solarData, windData) = DataReader.readData()

  private val powerPlants = List(
    PowerPlant("Hydro", shutdown = false),
    PowerPlant("Solar", shutdown = false),
    PowerPlant("Wind", shutdown = false)
  )

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
    println("4. Shutdown a plant")
    println("5. Exit")

    val choiceResult = Process.readIntFromStdIn("Please enter your choice: ")

    choiceResult match {
      case Success(choice) =>
        choice match {
          case 1 =>
            Process.viewData(dataHydro, dataSolar, dataWind)
          case 2 =>
            Process.viewAnalysis(hydroData.drop(1).map(_.toList), solarData.drop(1).map(_.toList), windData.drop(1).map(_.toList))
          case 3 =>
            Process.checkData(dataHydro, dataSolar, dataWind, powerPlants)
          case 4 =>
            shutDownPowerPlant()
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

  private def shutDownPowerPlant(): Unit = {
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
}
