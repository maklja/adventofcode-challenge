import scala.io.Source
import scala.util.Using

case class Game(id: Int, cubePicks: Map[String, Int])

def parseGames(gameInput: String) = {
    val s"Game $gameStrId:$gameData" = gameInput : @unchecked

    val gameId = gameStrId.toInt
    val cubePicks = gameData.split(";").flatMap {  
        _.split(",").map(cube => {
            val Array(n, color) = cube.trim.split(" ")
            (n.trim.toInt, color.trim)
        }) 
    }.foldLeft(Map[String, Int]())((map, cube)=> {
        val curCubeVal = map.getOrElse(cube._2, -1)
        if (curCubeVal < cube._1) 
            map.+ (cube._2 -> cube._1) 
        else 
            map
    })

    Game(gameId, cubePicks)
}

@main def day2Solution: Unit = {
    val possibleBagContent = Map(("blue" -> 14), ("green" -> 13), ("red" -> 12))
    Using.Manager { use =>

        val inputBufferedSource = use(Source.fromResource("day2/input.txt"))
        val gameIdsSum = inputBufferedSource.getLines()
                        .map(parseGames)
                        .filter(game => possibleBagContent.keys.forall(color => {
                            val bagColorCount = possibleBagContent.getOrElse(color, 0)
                            val gameColorCount = game.cubePicks.getOrElse(color, 0)

                            gameColorCount <= bagColorCount
                        }))
                        .map { _.id }
                        .sum

        println(f"Game ids sum: ${gameIdsSum}")
        
    }
}