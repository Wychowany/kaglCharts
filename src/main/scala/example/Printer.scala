package example

import vegas._
import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer


object Printer {
  def printAll(): Unit = {
    printDistributionOfSentimentInTrainSet()
    printDistributionOfSentimentInTestSet()
    printDistributionOfTweetLength()
    printDistributionOfSelectedTextLength()
    printNumberOfUniqueWordsInTweets()
    printNumberOfUniqueWordsInSelectedText()
    printBigrams()
  }

  case class Bigram(first: String, second: String)

  def printBigrams(): Unit = {
    val big = Data.train.filter(p => p.sentiment == "positive")
      .flatMap(p => {
        val bigrams = new ArrayBuffer[Bigram]()
        val listOfWords = p.selected_text.split(" ").filter(p=> !isEmpty(p))
        for (i <- listOfWords.indices) {
          if (i != 0) {
            bigrams.append(Bigram(listOfWords(i - 1), listOfWords(i)))
          }
        }
        bigrams
      }).groupBy(p1 => p1).map(p => (p._1, p._2.length)).toSeq.sortBy(p => p._2)(Ordering.Int.reverse).take(10).sortBy(p => p._2)(Ordering.Int.reverse)

    val plot = Vegas("Train set - sentiment distribution", width = 600.toDouble, height = 600.toDouble).
      withData(
        big.map(p => Map("bigram" -> p._1.first.concat(" ").concat(p._1.second),
          "occurrence" -> p._2))
      )
      .encodeX("bigram", Nom)
      .encodeY("occurrence", Quant)
      .mark(Bar)
    val plotContent: String = plot.html.pageHTML()
    new PrintWriter("bigrams.html") {
      write(plotContent);
      close()
    }
  }

  def printNumberOfUniqueWordsInSelectedText(): Unit = {

    val dataForPlot = Data.train.groupBy(p => p.sentiment)
      .map(p => (p._1, p._2.groupBy(p => p.selected_text.split(" ").distinct.length)
        .map(p => (p._1, p._2.length))))
    val buffer = new ArrayBuffer[Tuple3[String, Int, Int]]()
    dataForPlot.foreach(p => {
      p._2.foreach(k => {
        buffer.append((p._1, k._1, k._2))
      })
    })
    val plot = Vegas("Sample Multi Series Line Chart", width = 600.toDouble, height = 600.toDouble)
      .mark(Line)
      .encodeX("unique words in selected_text", Quant)
      .encodeY("tweets", Quant)
      .withData(
        buffer.map(p =>
          Map("unique words in selected_text" -> p._2,
            "tweets" -> p._3,
            "symbol" -> p._1))
      )
      .encodeColor(
        field = "symbol",
        dataType = Nominal,
        legend = Legend(orient = "left", title = "Sentiment"))
      .encodeDetailFields(Field(field = "symbol", dataType = Nominal))

    new PrintWriter("uniqueWordsInTweet.html") {
      write(plot.html.pageHTML());
      close()
    }
  }

  def printNumberOfUniqueWordsInTweets(): Unit = {

    val dataForPlot = Data.train.groupBy(p => p.sentiment)
      .map(p => (p._1, p._2.groupBy(p => p.text.split(" ").distinct.length)
        .map(p => (p._1, p._2.length))))
    val buffer = new ArrayBuffer[Tuple3[String, Int, Int]]()
    dataForPlot.foreach(p => {
      p._2.foreach(k => {
        buffer.append((p._1, k._1, k._2))
      })
    })
    val plot = Vegas("Sample Multi Series Line Chart", width = 600.toDouble, height = 600.toDouble)
      .mark(Line)
      .encodeX("unique words", Quant)
      .encodeY("tweets", Quant)
      .withData(
        buffer.map(p =>
          Map("unique words" -> p._2,
            "tweets" -> p._3,
            "symbol" -> p._1))
      )
      .encodeColor(
        field = "symbol",
        dataType = Nominal,
        legend = Legend(orient = "left", title = "Sentiment"))
      .encodeDetailFields(Field(field = "symbol", dataType = Nominal))

    new PrintWriter("uniqueWordsInTweet.html") {
      write(plot.html.pageHTML());
      close()
    }
  }


  def printDistributionOfSentimentInTrainSet(): Unit = {
    val grouped = Data.train.groupBy(example => example.sentiment).map(e => (e._1, e._2.length))
    val plot = Vegas("Train set - sentiment distribution", width = 600.toDouble, height = 600.toDouble).
      withData(
        grouped.map(p => Map("sentiment" -> p._1, "occurrence" -> p._2)).toSeq
      )
      .encodeX("sentiment", Nom)
      .encodeY("occurrence", Quant)
      .mark(Bar)
    val plotContent: String = plot.html.pageHTML()
    new PrintWriter("trainSetDistribution.html") {
      write(plotContent);
      close()
    }
  }

  def printDistributionOfSentimentInTestSet(): Unit = {
    val grouped = Data.test.groupBy(example => example.sentiment).map(e => (e._1, e._2.length))
    Data.test.filter(p => p.sentiment == "sentiment")
    val plot = Vegas("Test set - sentiment distribution", width = 600.toDouble, height = 600.toDouble)
      .withData(
        grouped.map(p => Map("sentiment" -> p._1, "occurrence" -> p._2)).toSeq
      )
      .encodeX("sentiment", Nom)
      .encodeY("occurrence", Quant)
      .mark(Bar)
    val plotContent: String = plot.html.pageHTML()
    new PrintWriter("testSetDistribution.html") {
      write(plotContent);
      close()
    }
  }

  def printDistributionOfTweetLength(): Unit = {
    val dataForPlot = Data.train.groupBy(p => p.sentiment)
      .map(p => (p._1, p._2.groupBy(p => p.text.length)
        .map(p => (p._1, p._2.length))))
    val buffer = new ArrayBuffer[Tuple3[String, Int, Int]]()
    dataForPlot.foreach(p => {
      p._2.foreach(k => {
        buffer.append((p._1, k._1, k._2))
      })
    })
    val plot = Vegas("Sample Multi Series Line Chart", width = 600.toDouble, height = 600.toDouble)
      .mark(Line)
      .encodeX("characters", Quant)
      .encodeY("tweets", Quant)
      .withData(
        buffer.map(p =>
          Map("characters" -> p._2,
            "tweets" -> p._3,
            "symbol" -> p._1))
      )
      .encodeColor(
        field = "symbol",
        dataType = Nominal,
        legend = Legend(orient = "left", title = "Sentiment"))
      .encodeDetailFields(Field(field = "symbol", dataType = Nominal))

    new PrintWriter("tweetLength.html") {
      write(plot.html.pageHTML());
      close()
    }
  }

  def printDistributionOfSelectedTextLength(): Unit = {
    val dataForPlot = Data.train.groupBy(p => p.sentiment)
      .map(p => (p._1, p._2.groupBy(p => p.selected_text.length)
        .map(p => (p._1, p._2.length))))
    val buffer = new ArrayBuffer[Tuple3[String, Int, Int]]()
    dataForPlot.foreach(p => {
      p._2.foreach(k => {
        buffer.append((p._1, k._1, k._2))
      })
    })
    val plot = Vegas("Sample Multi Series Line Chart", width = 600.toDouble, height = 600.toDouble)
      .mark(Line)
      .encodeX("characters in selected text", Quant)
      .encodeY("tweets", Quant)
      .withData(
        buffer.map(p =>
          Map("characters in selected text" -> p._2,
            "tweets" -> p._3,
            "symbol" -> p._1))
      )
      .encodeColor(
        field = "symbol",
        dataType = Nominal,
        legend = Legend(orient = "left", title = "Sentiment"))
      .encodeDetailFields(Field(field = "symbol", dataType = Nominal))

    new PrintWriter("selectedTextLength.html") {
      write(plot.html.pageHTML());
      close()
    }
  }

  private def isEmpty(x: String) = Option(x).forall(_.trim.isEmpty)
}
