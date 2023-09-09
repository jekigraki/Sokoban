package SokobanGame

import SokobanGame.Sokoban.{Board, playerPos}

  class MapModification {
    def removeFirstRow(board: Board): Board = {
      if (board.length > 1) {
        for (i <- 0 until board.length - 1) {
          board(i) = board(i + 1)
        }
      }
      board
    }

    def removeLastRow(board: Board): Board = {
      if (board.length > 1) {
        board.dropRight(1)
      }
      board
    }

    def removeFirstColumn(board: Board): Board = {
      if (board(0).length > 1) {
        for (i <- board.indices) {
          board(i) = board(i).tail
        }
      }
      board
    }

    def removeLastColumn(board: Board): Board = {
      if (board(0).length > 1) {
        for (i <- board.indices) {
          board(i) = board(i).dropRight(1)
        }
      }
      board
    }

    def extendRow(): Unit = {

    }

    def extendFirstRow(board: Board, row: Array[Tile]): Board = {
      val newRow = Array.fill(board(0).length)(Space)
      val newBoard = Array.ofDim[Tile](board.length, board(0).length);
      newBoard(0) ++= newRow;
      for (i <- board.indices) {
        newBoard(i + 1) = board(i - 1)
      }
      playerPos = playerPos.copy(y = playerPos.y + 1)
      board
    }

    def extendLastRow(board: Board): Board = {
      val newRow = Array.fill(board(0).length)(Space)
      board(board.length + 1) ++= newRow
      board
    }

    def extendFirstColumn(board: Board): Board = {
      for (i <- board.indices) {
        board(i) = Space +: board(i)
      }
      playerPos = playerPos.copy(x = playerPos.x + 1)
      board
    }

    def extendLastColumn(board: Board): Board = {
      for (i <- board.indices) {
        board(i) = board(i) :+ Space
      }
      board
    }
  }