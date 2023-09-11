package SokobanGame

import SokobanGame.Sokoban.{Board, playerPos, printBoard}

import scala.::

class Menu {
  type DescriptionAndFunction = (String, () => Unit)

  val map = new Map();
  val game = new Game();
  val solver = new Solver();
  val mapModification = new MapModification();
  val mapValidation = new MapValidation();
  def chooseAndExecuteFunction(options: List[DescriptionAndFunction], getUserInput: () => Int, output: String => Unit): Unit = {
    options.zipWithIndex.foreach { case ((desc, _), idx) =>
      output(s"${idx + 1}. $desc")
    }

    val userInput = getUserInput()

    if (userInput > 0 && userInput <= options.size) {
      options(userInput - 1)._2()
    } else {
      output("Pogresan izbor!")
    }
  }

  val actualUserInput: () => Int = () => scala.io.StdIn.readInt()

  val actualOutput: String => Unit = println

  def exit(): Unit = { System.exit(0)}


  def generateMapsChoose(): List[DescriptionAndFunction] = {
    val fileNames = map.listMaps("C:\\games\\maps").toList
    var list = fileNames.map(name => (name, () => map.loadMap(name)))
    list = list :+ new DescriptionAndFunction("Izlaz", () => exit());
    list;
  }

  def generateMapsActions(board: Board): List[DescriptionAndFunction] = {
    val play = new DescriptionAndFunction("Igraj", () => game.Play(board));
    val playFromFile = new DescriptionAndFunction("Igraj iz fajla", () => game.PlayFromFile(board));
    val solve = new DescriptionAndFunction("Reši mapu", () => solver.solve(board));
    val modify = new DescriptionAndFunction("Izmeni mapu", () => chooseAndExecuteFunction(generateMapModificationActions(board), actualUserInput, actualOutput));
    val back = new DescriptionAndFunction("Nazad", () => ());

    List(play, playFromFile, solve, modify, back)
  }

  def generateMapModificationActions(board: Board): List[DescriptionAndFunction] = {
    val removeFirstRow = new DescriptionAndFunction("Obriši prvi red", () => removeFirstRowFromMap(board));
    val removeLastRow = new DescriptionAndFunction("Obriši poslednji red", () => removeLastRowFromMap(board));
    val removeFirstColumn = new DescriptionAndFunction("Obriši prvu kolonu", () => removeFirstColumnFromMap(board));
    val removeLastColumn = new DescriptionAndFunction("Obriši poslednju kolonu", () => removeLastColumnFromMap(board));
    val extendRow = new DescriptionAndFunction("Dodaj red", () => extendRowFromMap(board));
    val extendFirstColumn = new DescriptionAndFunction("Dodaj prvu kolonu", () => extendFirstColumnFromMap(board));
    val extendLastColumn = new DescriptionAndFunction("Dodaj poslednju kolonu", () => extendLastColumnFromMap(board));
    val replaceTileAtPosition = new DescriptionAndFunction("Zameni tip polja", () => replaceTileAtPositionFromMap(board));
    val invertBoard = new DescriptionAndFunction("Invertuj tablu", () => invertBoardFromMap(board));
    val filterBoard = new DescriptionAndFunction("Filtriraj tablu", () => filterBoardFromMap(board));
    val isValid = new DescriptionAndFunction("Da li je tabla validna?", () => isMapValid(board));
    val back = new DescriptionAndFunction("Nazad", () => chooseAndExecuteFunction(generateMapsActions(board), actualUserInput, actualOutput));

    List(removeFirstRow, removeLastRow, removeFirstColumn, removeLastColumn, extendRow, extendFirstColumn, extendLastColumn, replaceTileAtPosition, invertBoard, filterBoard, isValid, back)
  }

  def removeFirstRowFromMap(board: Board): Unit = {
    val newBoard = mapModification.removeFirstRow(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def removeLastRowFromMap(board: Board): Unit = {
    val newBoard = mapModification.removeLastRow(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def removeFirstColumnFromMap(board: Board): Unit = {
    val newBoard = mapModification.removeFirstColumn(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def removeLastColumnFromMap(board: Board): Unit = {
    val newBoard = mapModification.removeLastColumn(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def extendRowFromMap(board: Board): Unit = {
    val newBoard = mapModification.extendRow(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def extendFirstColumnFromMap(board: Board): Unit = {
    val newBoard = mapModification.extendFirstColumn(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def extendLastColumnFromMap(board: Board): Unit = {
    val newBoard = mapModification.extendLastColumn(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def replaceTileAtPositionFromMap(board: Board): Unit = {
    val newBoard = mapModification.replaceTileAtPosition(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def invertBoardFromMap(board: Board): Unit = {
    val newBoard = mapModification.invertBoard(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def filterBoardFromMap(board: Board): Unit = {
    val newBoard = mapModification.filterBoard(board);
    printBoard(newBoard);
    map.SetNewBoard(newBoard);
    chooseAndExecuteFunction(generateMapModificationActions(newBoard), actualUserInput, actualOutput)
  }

  def isMapValid(board: Board): Unit = {
    var isValid = mapValidation.isValidMap(board);
    if(isValid) {
      println("Mapa je validna.");
    }
    else{
      println("Mapa nije validna.");
    }
    chooseAndExecuteFunction(generateMapModificationActions(board), actualUserInput, actualOutput)
  }

  def getLevel(): String = {
     map.getLevel()
  }
}
