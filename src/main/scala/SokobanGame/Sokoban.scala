import scala.collection.mutable.Stack
package SokobanGame {

  import java.io.{File, PrintWriter}
  import scala.io.Source

  sealed trait Tile

  case object Wall extends Tile

  case object Space extends Tile

  case object Box extends Tile

  case object Target extends Tile

  case object Player extends Tile

  case class Point(x: Int, y: Int) {
    def +(that: Point) = Point(this.x + that.x, this.y + that.y)
  }

  object Sokoban extends App {
    type Board = Array[Array[Tile]];



    val menu = new Menu();
    val mapModification = new MapModification();
    val solver = new Solver();
    val game = new Game();

    var playerPos: Point = _
    var boxes: Set[Point] = Set.empty
    var targets: Set[Point] = Set.empty
    var moveStack: Stack[(Point, Set[Point])] = Stack.empty

    def loadLevel(level: String): Board = {
      val rows = level.split("\n")
      for (i <- rows.indices) {
        rows(i) = rows(i).replaceAll("\\s+", "")
      }
      val board = Array.ofDim[Tile](rows.length, rows(0).length)

      for ((row, y) <- rows.zipWithIndex; (ch, x) <- row.zipWithIndex) {
        board(y)(x) = ch match {
          case '#' => Wall
          case '-' => Space
          case '.' => {
            targets += Point(x, y)
            Target
          }
          case 'x' => {
            boxes += Point(x, y)
            Box
          }
          case 's' => {
            playerPos = Point(x, y)
            Player
          }
          case '\r' => {
            Space
          }
        }
      }
      board
    }

    def printBoard(board: Board): Unit = {
      for (y <- board.indices; x <- board(y).indices) {
        if (Point(x, y) == playerPos) print('s')
        else if (boxes.contains(Point(x, y))) print('x')
        else board(y)(x) match {
          case Wall => print('#')
          case Space => print('-')
          case Target => print('.')
          case Box => print('x')
          case Player => print('s')
          case _ => ()
        }
        if (x == board(y).length - 1) println()
      }
    }

    var moves = Map(
      'u' -> Point(0, -1),
      'd' -> Point(0, 1),
      'l' -> Point(-1, 0),
      'r' -> Point(1, 0)
    )

    val level = {
      """-#####---
        |-#---####
        |-#---#--#
        |-##-----#
        |###-###-#
        |#sx.#-#-#
        |#---#-###
        |#---#----
        |#####----""".stripMargin

      while (true) {
        val generateMapsChooses = menu.generateMapsChoose()
        println("Izaberite mapu:")
        menu.chooseAndExecuteFunction(generateMapsChooses, menu.actualUserInput, menu.actualOutput)
        println("Izabrana mapa:")
        var board = loadLevel(menu.getLevel())
        printBoard(board)

        val generateMapsActions = menu.generateMapsActions(board);
        menu.chooseAndExecuteFunction(generateMapsActions, menu.actualUserInput, menu.actualOutput)

      }

    }
  }
}



