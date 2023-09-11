package SokobanGame



import SokobanGame.Sokoban.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.:+


class GameTest extends AnyFlatSpec with Matchers {

  def mockBoardWithBoxInFrontOfPlayer(): Board = {
    Array(
      Array[Tile](Wall, Wall, Wall, Wall),
      Array[Tile](Wall, Space, Player , Wall),
      Array[Tile](Wall, Box, Space, Wall),
      Array[Tile](Wall, Target, Space, Wall),
      Array[Tile](Wall, Wall, Wall, Wall)
    )
  }

  def mockBoardWithBoxBeforeWall(): Board = {
    Array(
      Array[Tile](Wall, Wall, Wall, Wall),
      Array[Tile](Wall, Player, Space, Wall),
      Array[Tile](Wall, Space, Box, Wall),
      Array[Tile](Wall, Space, Target, Wall),
      Array[Tile](Wall, Wall, Wall, Wall)
    )
  }

  def mockBoardWithAllBoxesOnTargets(): Board = {
    Array(
      Array(Wall,   Wall,     Wall,     Wall,    Wall),
      Array(Wall,   Space,    Space,   Space,   Wall),
      Array(Wall,   Player,   Box,      Target,   Wall),
      Array(Wall,   Space,    Space,   Space,   Wall),
      Array(Wall,   Wall,     Wall,     Wall,    Wall)   ) }

  val game = new Game

  "Play" should "move box on right" in {
    val mockBoard = mockBoardWithAllBoxesOnTargets()

    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2,2);
    game.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(3, 2);
    game.SetTargets(targets);

    val player = Point(1, 2);
    game.SetPlayerPosition(player);

    game.SetMoves();
    game.ResetStack();

    game.Play(mockBoard, Option("r"))
  }



  it should "move the box when the player pushes it down in corner" in {
    val mockBoard = mockBoardWithBoxInFrontOfPlayer()

    var boxes: Set[Point] = Set.empty;;
    boxes += Point(1, 2);
    game.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(1, 3);
    game.SetTargets(targets);

    val player = Point(1, 1);
    game.SetPlayerPosition(player);

    game.SetMoves();
    game.ResetStack();

    game.Play(mockBoard, Option("d"))
  }



  it should "move the box when the player pushes it down" in {
    val mockBoard = mockBoardWithBoxBeforeWall()

    var boxes: Set[Point] = Set.empty;;
    boxes += Point(2, 2);
    game.SetBoxes(boxes);

    var targets: Set[Point] = Set.empty;;
    targets += Point(2, 3);
    game.SetTargets(targets);

    val player = Point(2, 1);
    game.SetPlayerPosition(player);

    game.SetMoves();
    game.ResetStack();

    game.Play(mockBoard, Option("d"))
  }
}