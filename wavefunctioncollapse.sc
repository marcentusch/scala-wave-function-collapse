import scala.util.Random.shuffle

val height = 8
val width = 32

case class Tile(
    char: String = " ",
    y: Int,
    x: Int,
    possibleStates: Set[String] = Set("┤", "├", "┴", "┬"),
    isCollapsed: Boolean = false
) {
  val entropy = possibleStates.size
}

val grid: Array[Array[Tile]] =
  val grid = Array.ofDim[Tile](height, width)
  for
    row <- 0 until height
    col <- 0 until width
  yield grid(row)(col) = Tile(x = col, y = row)
  grid

var nonCollapsedTiles: List[Tile] = shuffle(grid.toList.flatten)

def syncNonCollapsedTiles() =
  nonCollapsedTiles = shuffle(grid.flatten.filterNot(_.isCollapsed).toList)

def getTileAt(x: Int, y: Int, grid: Array[Array[Tile]]): Option[Tile] =
  if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length)
    Some(grid(x)(y))
  else None

def getPossibleStates(collapsedTile: Tile, direction: String): Set[String] =
  (collapsedTile.char, direction) match
    case (" ", "up")    => Set("┴")
    case (" ", "right") => Set("├")
    case (" ", "down")  => Set("┬")
    case (" ", "left")  => Set("┤")
    case ("┤", "up")    => Set("├", "┤", "┬")
    case ("┤", "right") => Set("├", " ")
    case ("┤", "down")  => Set("├", "┤", "┴")
    case ("┤", "left")  => Set("┬", "┴", "┬")
    case ("├", "up")    => Set("├", "┤", "┬")
    case ("├", "right") => Set("┬", "┤", "┴")
    case ("├", "down")  => Set("├", "┤", "┴")
    case ("├", "left")  => Set("┤", " ")
    case ("┴", "up")    => Set("├", "┤", "┬")
    case ("┴", "right") => Set("┬", "┤", "┴")
    case ("┴", "down")  => Set("┬", " ")
    case ("┴", "left")  => Set("├", "┬", "┴")
    case ("┬", "up")    => Set("┴", " ")
    case ("┬", "right") => Set("┬", "┤", "┴")
    case ("┬", "down")  => Set("├", "┤", "┴")
    case ("┬", "left")  => Set("├", "┴", "┬")

def collapseTile(tile: Tile) =
  val randomState = shuffle(tile.possibleStates.toList).head
  updateGridTile(tile.copy(char = randomState, isCollapsed = true))

def updateGridTile(tile: Tile) =
  grid(tile.y)(tile.x) = tile

def updateNeighborEntropy(tile: Tile): Unit =
  updateNeighborTileEntropy(grid(tile.y)(tile.x), "up")
  updateNeighborTileEntropy(grid(tile.y)(tile.x), "right")
  updateNeighborTileEntropy(grid(tile.y)(tile.x), "down")
  updateNeighborTileEntropy(grid(tile.y)(tile.x), "left")

def updateNeighborTileEntropy(
    collapsedTile: Tile,
    direction: String
) =
  val (dx, dy) = direction match
    case "up"    => (0, -1)
    case "right" => (1, 0)
    case "down"  => (0, 1)
    case "left"  => (-1, 0)

  val neighborTile =
    getTileAt(collapsedTile.y + dy, collapsedTile.x + dx, grid)

  neighborTile.map(tile =>
    updateGridTile(
      tile.copy(possibleStates = getPossibleStates(collapsedTile, direction))
    )
  )

def getNextTileToCollapse() =
  nonCollapsedTiles.minByOption(_.entropy).get

def renderGridAsString(grid: Array[Array[Tile]]): String =
  grid.map(_.map(_.char).mkString).mkString("\n")

// This is the actual algorithm
while (!nonCollapsedTiles.isEmpty)
  val tileToCollapse = getNextTileToCollapse()
  collapseTile(tileToCollapse)
  updateNeighborEntropy(tileToCollapse)
  syncNonCollapsedTiles()
  Thread.sleep(100)
  println(renderGridAsString(grid))

/*

To run this code: `brew install scala-cli` and then `scala-cli wavefunctioncollapse.sc`

 * Notes
- Github repo: https://github.com/mxgmn/WaveFunctionCollapse
- WFC example https://oskarstalberg.com/game/wave/wave.html
- Wave function collapse poetry generator https://github.com/mewo2/oisin

3D WFC: https://oskarstalberg.com/Townscaper/#GSB0RARueC6Snc9E0lO5B

Bad North uses this algorithm to generate 3D islands: https://youtu.be/RqD6qJZlW90?si=kkCn29QdRX54GuZu

Creator of Bad North also has a cool talk about how they implemented it: https://www.youtube.com/watch?v=0bcZb-SsnrA

Wave function collapse but with tiles just being a certain color and grid is each pixel on the screen: https://github.com/gignac-cha/gignac-cha.github.io/tree/main/wave-function-collapse


 * Improvements
- Could look at entropy of whole grid after each collapse to maybe get better result with less loose ends
- Each possible state could have different probabilities
- Endless possibilities for different rules, 3D grids, long lists of possible states, etc.
- Example characters that could be added as possibleStates: ┼  └ ─ ┐ ┌ ┘ │

 */
