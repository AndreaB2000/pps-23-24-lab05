package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics

import scala.jdk.javaapi.OptionConverters
import util.Sequences.Sequence
import util.Sequences
import util.Optionals.Optional.Just
import scala.compiletime.ops.double
import scala.util.Random

enum CellState:
  case MINE,
  ZERO,
  ONE,
  TWO,
  THREE,
  FOUR,
  FIVE,
  SIX,
  SEVEN,
  EIGHT

object CellState:
  def getVal(n: Int): CellState = n match
    case 0 => ZERO
    case 1 => ONE
    case 2 => TWO
    case 3 => THREE
    case 4 => FOUR
    case 5 => FIVE 
    case 6 => SIX
    case 7 => SEVEN
    case 8 => EIGHT
    case _ => MINE

trait Cell:
  def x: Int
  def y: Int
  def state: CellState
  def state_=(state: CellState): Unit
  def isAdjacent(cell: Cell): Boolean

object Cell:
  def apply(x: Int, y: Int, state: =>CellState): Cell =
    new CellImpl(x, y, state)
  def unapply(cell: Cell): (Int, Int, CellState) =
    (cell.x, cell.y, cell.state)
  case class CellImpl(val x: Int, val y: Int, var state: CellState) extends Cell:
    override def isAdjacent(cell: Cell): Boolean = cell match
      case CellImpl(x, y, _) => cell != this && Math.abs(cell.x - x) <= 1 && Math.abs(cell.y - y) <= 1 

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var board: Sequence[Cell] = Sequence()
  private var placedMines = 0

  for i: Int <- 0 until size do
    for j: Int <- 0 until size do
      board = Sequence.Cons(Cell(i, j, CellState.ZERO), board)
  
  while placedMines < mines do
    val x: Int = Random.nextInt(size)
    val y: Int = Random.nextInt(size)
    if board.find(c => c.x == x && c.y == y && c.state == CellState.MINE).isEmpty then
      placedMines += 1
      board.find(c => c.x == x && c.y == y) match
        case Just(c) => c.state = CellState.MINE
        case _       =>

  

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter

  def won = false
