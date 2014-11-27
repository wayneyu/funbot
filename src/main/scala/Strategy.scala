/**
 * Created by wayneyu on 11/26/14.
 */
trait Strategy{
  def react(bot: Bot): Bot
}

object BotStrategy extends Strategy {

  def react(bot: Bot) = reactAsGatherer(bot)

  def reactAsGatherer(bot: Bot): Bot = {

    val directionValue = bot.viewAnalyzer.directionValue
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // determine move direction
    directionValue(lastDirection) += 10 // try to break ties by favoring the last direction
    directionValue(XY.fromDirection45(lastDirection).negate.toDirection45) -= 10 // unfavor going backward

    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val bestDirection = XY.fromDirection45(bestDirection45)

    // spawn if surrounding has few mini bots
    if (bot.energy > 100 && bot.viewAnalyzer.S.size < 10) {
      val spawnDirection = bestDirection.negate
      bot.spawn(spawnDirection, "mood" -> "Gathering")
      directionValue(spawnDirection.toDirection45) -= 10
    }

    bot.move(bestDirection)
    bot.set("lastDirection" -> bestDirection.toDirection45)
  }

}

object MiniBotStrategy extends Strategy {

  def react(bot: Bot) = {
    bot match {
      case b: MiniBot => b.mood match {
        case Aggressive => reactAggressively(b)
        case Defensive => reactDefensively(b)
        case Gathering => reactAsGatherer(b)
      }
      case b: Bot => b
    }
  }

  def reactAsGatherer(bot: MiniBot): Bot = {

    val directionValue = bot.viewAnalyzer.directionValue
    val apocalypse = bot.inputAsIntOrElse("apocalypse",5000)

    // determine movement direction
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)
    directionValue(lastDirection) += 10 // try to break ties by favoring the last direction
    directionValue(XY.fromDirection45(lastDirection).negate.toDirection45) -= 10 // unfavor going backward

    // back to master if master is nearby by favoring direction of master
    if ( bot.energy > 500 && bot.offsetToMaster.stepCount <= 5 ||
      (apocalypse - 200 - bot.time < bot.offsetToMaster.stepCount))
      directionValue(bot.offsetToMaster.toDirection45) += 500

    // favor the food that no other friendly slave is closer too
    val siblings = bot.viewAnalyzer.S
    val fluppets = bot.viewAnalyzer.B
    if ( fluppets.nonEmpty && siblings.nonEmpty ) {
      for (xy <- fluppets) {
        val closestSiblingToFluppet = siblings.map(_.distanceTo(xy)).sorted.head
        if (closestSiblingToFluppet < xy.length) {
          // remove the point for the fluppet
          directionValue(xy.toDirection45) -= {
            val steps = xy.stepCount
            if(steps == 1) 600
            else if(steps == 2) 400
            else (430 - steps * 15).max(10)
          }
        } else {
          directionValue(xy.toDirection45) += 500
        }
      }
    }
    val zugars = bot.viewAnalyzer.P
    if ( zugars.nonEmpty && siblings.nonEmpty ) {
      for (xy <- zugars) {
        val closestSiblingToZugars = siblings.map(_.distanceTo(xy)).sorted.head
        if (closestSiblingToZugars < xy.length) {
          // remove the point for the zugar
          directionValue(xy.toDirection45) -= {
            val steps = xy.stepCount
            if(steps == 1) 500
            else if(steps == 2) 300
            else (320 - steps * 10).max(10)
          }
        } else {
          directionValue(xy.toDirection45) += 500
        }
      }
    }

    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val bestDirection = XY.fromDirection45(bestDirection45)

    // spawn new gatherer if the surrounding lacks minibots
    if ( siblings.size < 5 )
      bot.spawn(bestDirection.negate, "mood" -> "Gathering")

    bot.move(bestDirection)
    bot.set("lastDirection" -> bestDirection.toDirection45)

    // react to enemy bots and beasts if they are too close
    val enemybots = bot.viewAnalyzer.m ++ bot.viewAnalyzer.s
    val beasts = bot.viewAnalyzer.b
    enemybots ++ beasts match {
      case x :: xs =>
        if ( x.length < 10 ) reactAggressively(bot)
        else bot
      case Nil => bot
    }
    enemybots match {
      case x :: xs =>
        if ( x.stepCount < 2 ) reactDefensively(bot) // rather explode than get annihilated
        else bot
      case Nil => bot
    }


  }

  def reactAggressively(bot: MiniBot): Bot = {

    val analyzer = bot.viewAnalyzer
    val resourceValue = analyzer.resourceValue
    val directionValue = bot.viewAnalyzer.directionValue
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val bestDirection = XY.fromDirection45(bestDirection45)

    val blastRadius = 5
    val damageByExploding =
      (for {d <- analyzer.b} yield bot.calcExplodeDamage(blastRadius, d.length, 150)) ++
        (for {d <- analyzer.s} yield bot.calcExplodeDamage(blastRadius, d.length, 200)) ++
        (for {d <- analyzer.m} yield bot.calcExplodeDamage(blastRadius, d.length, 500))

    // explode if damage by exploding exceeds bot's energy and value of potential resource
    if (damageByExploding.sum > (bot.energy + resourceValue)) {
      if ( analyzer.m.nonEmpty && analyzer.m.head.stepCount > 1) //get closer to enemy master
        bot.move(analyzer.m.head)
      else {
        bot.spawn(bestDirection.negate, "mood" -> "Gathering")
        bot.explode(blastRadius)
      }
    } else {
      bot.set("mood" -> "Gathering")
    }
  }


  def reactDefensively(bot: MiniBot): Bot = {
    bot.spawn(XY(1,0), "energy" -> bot.energy, "mood" -> "Gathering")
    bot.explode(2)
  }

}