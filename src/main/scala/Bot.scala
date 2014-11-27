/**
 * Created by wayneyu on 11/26/14.
 */
trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String
  def inputAsIntOrElse(key: String, fallback: Int): Int
  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
  def view: View
  def energy: Int
  def time: Int
  def generation: Int
  def viewAnalyzer: ViewAnalyzer

  // outputs
  def move(delta: XY) : Bot
  def say(text: String) : Bot
  def status(text: String) : Bot
  def spawn(offset: XY, params: (String,Any)*) : Bot
  def set(params: (String,Any)*) : Bot
  def log(text: String) : Bot
}

trait MiniBot extends Bot {
  // inputs
  def offsetToMaster: XY

  def calcExplodeDamage(blastRadius: Int, distFromCenter: Double, maxDamage: Int) = {
    val blastArea = blastRadius*blastRadius*3.141
    val energyPerArea = energy/blastArea
    val damageAtCenter = energyPerArea*200
    val damage = damageAtCenter*(if (distFromCenter >= blastRadius) 0 else (1-distFromCenter/blastRadius))
    if (damage > maxDamage) maxDamage.toDouble else damage
  }

  def mood: Mood

  // outputs
  def explode(blastRadius: Int) : Bot
}

class BotImpl(inputParams: Map[String, String]) extends Bot {
  // input
  def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  val view = View(inputParams("view"))
  val energy = inputParams("energy").toInt
  val time = inputParams("time").toInt
  val generation = inputParams("generation").toInt
  val viewAnalyzer: ViewAnalyzer = viewAnalyzerMaster(view)

  // output
  private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
  private var commands = ""                           // holds all other commands
  private var debugOutput = ""                        // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }

  /** Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if(stateParams.nonEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(", ",", ")")
    }
    if(debugOutput.nonEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String,Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}


class MiniBotImpl(inputParams: Map[String, String]) extends BotImpl(inputParams: Map[String, String]) with MiniBot{

  def offsetToMaster = inputAsXYOrElse("master", XY.Zero)
  override val viewAnalyzer = viewAnalyzerSlave(view)
  val mood = inputOrElse("mood","Gathering") match {
    case "Gathering" => Gathering
    case "Aggressive" => Aggressive
    case "Defensive" => Defensive
    case s: String => Gathering
  }
  val blastRadius = 2
  def damageByExploding = ( for {
    d <- viewAnalyzer.s
  } yield calcExplodeDamage(blastRadius, d.length, 150)).sum

  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")

}