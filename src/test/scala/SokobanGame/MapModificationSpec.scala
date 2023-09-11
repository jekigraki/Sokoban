package SokobanGame


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SokobanGame.Sokoban.{Board}
import SokobanGame.Tile


class MapModificationSpec extends AnyFlatSpec with Matchers {



  "MapModification" should "correctly remove the first row" in {
    val board: Board = Array(
      Array[Tile](Wall, Wall),
      Array[Tile](Space, Player)
    )
    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);

    val newBoard = mapModification.removeFirstRow(board)

    newBoard.length shouldBe 1
    newBoard(0)(0) shouldBe Space
    newBoard(0)(1) shouldBe Player
  }



  it should "correctly remove the last row" in {
    val board: Board = Array(
      Array(Wall, Wall),
      Array(Space, Player)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.removeLastRow(board)

    newBoard.length shouldBe 1
    newBoard(0)(0) shouldBe Wall
    newBoard(0)(1) shouldBe Wall
  }



  it should "correctly remove the first column" in {
    val board: Board = Array(
      Array(Wall, Wall),
      Array(Space, Player)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.removeFirstColumn(board)

    newBoard(0).length shouldBe 1
    newBoard(0)(0) shouldBe Wall
    newBoard(1)(0) shouldBe Player
  }

  it should "correctly add the first column" in {
    val board: Board = Array(
      Array(Wall, Wall),
      Array(Space, Player)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.extendFirstColumn(board, Option("##"))

    newBoard(0).length shouldBe 3
    newBoard(0)(0) shouldBe Wall
    newBoard(0)(1) shouldBe Wall
  }

  it should "correctly add the last column" in {
    val board: Board = Array(
      Array(Wall, Wall),
      Array(Space, Player)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.extendLastColumn(board, Option("##"))

    newBoard(0).length shouldBe 3
    newBoard(0)(2) shouldBe Wall
    newBoard(1)(2) shouldBe Wall
  }

  it should "correctly invert the map" in {
    val board: Board = Array(
      Array(Box, Target),
      Array(Space, Space),
      Array(Space, Space),
      Array(Space, Space)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.invertBoard(board)

    newBoard(0)(0) shouldBe Target
    newBoard(0)(1) shouldBe Box
  }

  it should "correctly invert the map without targets" in {
    val board: Board = Array(
      Array(Box, Space),
      Array(Space, Space),
      Array(Space, Space),
      Array(Space, Space)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.invertBoard(board)

    newBoard(0)(0) shouldBe Target
  }

  it should "correctly invert the map without boxes" in {
    val board: Board = Array(
      Array(Target, Space),
      Array(Space, Space),
      Array(Space, Space),
      Array(Space, Space)
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.invertBoard(board)

    newBoard(0)(0) shouldBe Box
  }


  it should "correctly invert the map without boxes and targets" in {
    val board: Board = Array(
      Array(Space, Space),
    )

    val mapModification = new MapModification()
    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    mapModification.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    mapModification.SetTargets(targets);

    val player = Point(1, 2);
    mapModification.SetPlayerPosition(player);
    val newBoard = mapModification.invertBoard(board)

    newBoard(0)(0) shouldBe Space
    newBoard(0)(1) shouldBe Space
  }
}