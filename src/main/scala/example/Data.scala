package example


import scala.collection.mutable.ArrayBuffer

object Data {
  var train: ArrayBuffer[TrainExample] = new ArrayBuffer[TrainExample]()
  val test: ArrayBuffer[TestExample] = new ArrayBuffer[TestExample]()

  def readAll(): Unit = {
    readTestSet()
    readTrainSet()
  }

  def readTrainSet(): ArrayBuffer[TrainExample] = {
    val file = getClass.getResource("/train.csv").getFile
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines
    lines.next()
    while (lines.hasNext) {
      val singleLine = lines.next
      var lineArray: Array[String] = singleLine.split(",")
      val textID = lineArray(0)
      var text: String = lineArray(1)
      var selected_text: String = lineArray(2)
      var sentiment: String = lineArray(lineArray.length - 1)
      if (singleLine.contains("\"")) {
        val quoteSplitted = singleLine.split("\"")
        text = quoteSplitted(1)
        if (singleLine.count(_ == '\"') == 4) {
          selected_text = quoteSplitted(3)
        } else {
          val selectedTextWithoutQuote = quoteSplitted(2)
          val restArray = selectedTextWithoutQuote.split(",")
          selected_text = restArray(1)
        }
      }
      train += TrainExample(textID, text, selected_text, sentiment)
    }
    source.close
    train
  }

  def readTestSet(): ArrayBuffer[TestExample] = {
    val file = getClass.getResource("/test.csv").getFile
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines
    lines.next
    while (lines.hasNext) {
      val singleLine = lines.next
      val line: Array[String] = singleLine.split(",")
      val textID = line(0)
      var text: String = line(1)
      var sentiment: String = line(line.length - 1)
      if (singleLine.contains("\"")) {
        text = singleLine.split("\"")(1)
      }
      test += TestExample(textID, text, sentiment)
    }
    source.close
    test
  }

  def checkDataAndFix(): Unit = {
    println("Size of train set: " + train.size)

    train.foreach(x => {
      if (isEmpty(x.textID)) {
        println("one of textID is empty!!!")
      } else if (isEmpty(x.text)) {
        println(s"${
          x.textID
        } has empty text!!")
      } else if (isEmpty(x.selected_text)) {
        println(s"${
          x.textID
        } has empty selected_text!!")
      } else if (isEmpty(x.sentiment)) {
        println(s"${
          x.textID
        } has empty sentiment!!")
      }
    })
    Data.train = train.filter(p=> !isEmpty(p.text))
    println("Dropping one row with null!")
    println("Size of train set: " + train.size)


    test.foreach(x => {
      if (isEmpty(x.textID)) {
        println("one of textID is empty!!!")
      } else if (isEmpty(x.text)) {
        println(s"${
          x.textID
        } has empty text!!")
      } else if (isEmpty(x.sentiment)) {
        println(s"${
          x.textID
        } has empty sentiment!!")
      }
    })
    println("Size of test set: " + test.size)
  }

  private def isEmpty(x: String) = Option(x).forall(_.trim.isEmpty)
}


case class TrainExample(textID: String, text: String, selected_text: String, sentiment: String)

case class TestExample(textID: String, text: String, sentiment: String)
