import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12Challenge:

  @main def day12Main(): Unit =
    Using.Manager { use =>

      val springsData =
        use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
      println(springsData)
    }

end Day12Challenge
