import scala.io.StdIn

object StudentRecordsApp extends App {
  
  // Function to read student information
  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student name:")
    val name = StdIn.readLine()

    println("Enter marks obtained:")
    val marks = StdIn.readInt()

    println("Enter total possible marks:")
    val totalMarks = StdIn.readInt()

    val percentage = calculatePercentage(marks, totalMarks)
    val grade = assignGrade(percentage)

    (name, marks, totalMarks, percentage, grade)
  }

  // Function to calculate percentage
  def calculatePercentage(marks: Int, totalMarks: Int): Double = {
    (marks.toDouble / totalMarks.toDouble) * 100
  }

  // Function to assign grade based on percentage
  def assignGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Marks: $totalMarks")
    println(s"Percentage: $percentage%")
    println(s"Grade: $grade")
  }

  // Function to validate input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || totalMarks < 0) {
      (false, Some("Marks and total marks must be positive integers."))
    } else if (marks > totalMarks) {
      (false, Some("Marks cannot exceed total possible marks."))
    } else {
      (true, None)
    }
  }

  // Function to get student info with retry until valid data is provided
  def getInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var name = ""
    var marks = 0
    var totalMarks = 0

    while (!isValid) {
      println("Enter student name:")
      name = StdIn.readLine()

      println("Enter marks obtained:")
      marks = StdIn.readInt()

      println("Enter total possible marks:")
      totalMarks = StdIn.readInt()

      val validation = validateInput(name, marks, totalMarks)
      isValid = validation._1

      if (!isValid) {
        println(s"Invalid input: ${validation._2.get}")
      }
    }

    val percentage = calculatePercentage(marks, totalMarks)
    val grade = assignGrade(percentage)

    (name, marks, totalMarks, percentage, grade)
  }

  // Main logic to demonstrate the application
  val studentRecord = getInfoWithRetry()
  printStudentRecord(studentRecord)
}
