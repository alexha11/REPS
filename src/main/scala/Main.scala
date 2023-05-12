import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.util.{Failure, Success}

case class RenewableData(startTime: LocalDateTime, endTime: LocalDateTime, powerProduction: Double)

object Main extends App {
  private val (hydroData, solarData, windData) = DataReader.readData()

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
    println("3. Check Data in the past 24 hours")
    println("4. Exit")

    val choiceResult = Process.readIntFromStdIn("Please enter your choice: ")

    choiceResult match {
      case Success(choice) =>
        choice match {
          case 1 =>
            Process.viewData(dataHydro, dataSolar, dataWind)
          case 2 =>
            Process.viewAnalysis(hydroData.drop(1).map(_.toList), solarData.drop(1).map(_.toList), windData.drop(1).map(_.toList))
          case 3 =>
            Process.checkData(dataHydro, dataSolar, dataWind)
          case 4 =>
            println("Goodbye!")
          case _ =>
            println("Invalid choice. Please try again.")
        }
        println()

        if (choice != 4)
          runMenu()
      case Failure(_) =>
        println("Invalid input. Please enter a valid choice.")
        runMenu()
    }
  }
  runMenu()
}
