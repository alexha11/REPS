import com.github.tototoshi.csv._
import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.sys.process._

object DataReader {
  // Download the CSV file from the Fingrid API
  private val today = LocalDate.now().format(DateTimeFormatter.ISO_DATE)
  private val windURL = s"https://api.fingrid.fi/v1/variable/75/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time=" + today + "T00%3A00%3A00%2B02%3A00"
  private val windCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$windURL' -o Wind.csv"
  private val solarURL = s"https://api.fingrid.fi/v1/variable/248/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time=" + today + "T00%3A00%3A00%2B02%3A00"
  private val solarCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$solarURL' -o Solar.csv"
  private val hydroURL = s"https://api.fingrid.fi/v1/variable/191/events/csv?start_time=2023-02-01T00%3A00%3A00%2B02%3A00&end_time=" + today + "T00%3A00%3A00%2B02%3A00"
  private val hydroCMD = s"curl -X GET --header 'Accept: text/csv' --header 'x-api-key: cGYPhIzJmk19e3jk7smz2a7vZfEmoL9U36nM2l8T' '$hydroURL' -o Hydro.csv"
  
  private val windExitCode = windCMD.!
  private val solarExitCode = solarCMD.!
  private val hydroExitCode = hydroCMD.!
  
  if (windExitCode == 0 || solarExitCode == 0 || hydroExitCode == 0) {
    println("CSV file downloaded and stored.")
  } else {
    println("Failed to download the CSV file.")
  }
  
  private val HydroCSV = CSVReader.open(new File("Hydro.csv"))
  private val SolarCSV = CSVReader.open(new File("Solar.csv"))
  private val WindCSV = CSVReader.open(new File("Wind.csv"))
  private val HydroData = HydroCSV.all()
  private val SolarData = SolarCSV.all()
  private val WindData = WindCSV.all()
  println("Data loaded successfully.")
  
  // Updated the return type to return a tuple of three lists
  def readData(): (List[List[String]], List[List[String]], List[List[String]]) = {
    (HydroData, SolarData, WindData)
  }
}