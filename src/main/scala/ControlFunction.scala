/** This bot is heavily based on the Scalatron reference bot.
  * It uses a modified attractiveness scoring system of the reference bot.
  * Each bot(both master and mini) spawns new bot if the number of friendly bots in its view is below a threshold
  * Minibots have three moods: Gathering(default), Aggressive and Defensive.
  * Gathering: bot scans its view and move towards resource that no other minibot is closer to
  * Aggressive: bot explodes if the damage points from exploding exceeds
  *                  the bot's energy and the value of potential resources
  * Defensive: bot explodes if an enemy bot is right on its doorstep
  */
object ControlFunction {

  var globals = Map[String, String]()
  val botStrategy = BotStrategy
  val miniBotStrategy = MiniBotStrategy

  def response(input: String): String = {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" =>
        globals = params
        ""
      case "React" =>
        react(params)
      case _ => "" // OK
    }
  }

  def react(params: Map[String, String] ) = {
    val gen = params("generation").toInt
    if( gen == 0 ) {
      val bot = new BotImpl(globals ++ params)
      forMaster(bot)(botStrategy)
      bot.toString
    } else {
      val bot = new MiniBotImpl(globals ++ params)
      forSlave(bot)(miniBotStrategy)
      bot.toString
    }
  }


  def forMaster(bot: Bot)(strategy: Strategy){
    strategy.react(bot)
  }


  def forSlave(bot: MiniBot)(strategy: Strategy) {
    strategy.react(bot)
  }

}

/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
  /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def apply(command: String): (String, Map[String, String]) = {
    /** "key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      (segments(0), if(segments.length>=2) segments(1) else "")
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}