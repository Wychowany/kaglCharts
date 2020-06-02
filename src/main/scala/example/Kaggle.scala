package example

object Kaggle extends App {
  Data.readAll()
  Data.checkDataAndFix()
  Printer.printAll()
}