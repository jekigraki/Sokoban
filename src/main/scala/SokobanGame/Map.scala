import java.io.File
import scala.io.Source

package SokobanGame {

  import SokobanGame.Sokoban.Board

  class Map {
    var level = "";
    def listMaps(folderPath: String): Array[String] = {
      val directory = new File(folderPath)
      if (directory.exists() && directory.isDirectory) {
        directory.listFiles.filter(_.isFile).map(_.getName)
      } else {
        Array.empty[String]
      }
    }

    def readFileAsString(filename: String): String = {
      Source.fromFile(filename).mkString
    }

    def getLevel(): String = {
      return level;
    }

    def SetNewBoard(board: Board): Unit = {
      val numRows = board.length
      val numCols = board(0).length
      level = "";
      for (y <- 0 until numRows) {
        for (x <- 0 until numCols) {
          level += board(y)(x)
        }
        level += '\n';
      }
    }

    def loadMap(fileName: String): Unit = {
      level =  readFileAsString("C:\\games\\maps\\" + fileName).stripMargin
    }


  }
}
