import com.github.tototoshi.csv._
import java.io.File

object Main extends App {
  val HydroCSV = CSVReader.open(new File("./src/main/scala/Hydro.csv"))
  val SolarCSV = CSVReader.open(new File("./src/main/scala/Solar.csv"))
  val WindCSV = CSVReader.open(new File("./src/main/scala/Wind.csv"))
  val HydroData = HydroCSV.all()
  val SolarData = SolarCSV.all()
  val WindData = WindCSV.all()

  var running = true

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
        println("Hydro Data:")
        HydroData.foreach(println)
      case 2 =>
        println("Solar Data:")
        SolarData.foreach(println)
      case 3 =>
        println("Wind Data:")
        WindData.foreach(println)
      case 4 =>
        running = false
        println("Goodbye!")
      case _ =>
        println("Invalid choice. Please try again.")
    }
    println()
  }
}
