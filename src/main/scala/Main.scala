import com.github.tototoshi.csv._
import java.io.File

object Main extends App {
  val HydroCSV = CSVReader.open(new File("./src/main/scala/Hydro.csv"))
  val SolarCSV = CSVReader.open(new File("./src/main/scala/Solar.csv"))
  val WindCSV = CSVReader.open(new File("./src/main/scala/Wind.csv"))
  val HydroData = HydroCSV.all()
  val SolarData = SolarCSV.all()
  val WindData = WindCSV.all()
  println("Hello, World!")
}