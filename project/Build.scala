import sbt._
import Keys._

object FunBotBuild extends Build {

  lazy val root = Project(
    id = "funbot",
    base = file("."),
    settings = botSettings)

  def botSettings: Seq[Setting[_]] = Seq(
    version := "0.1-SNAPSHOT",
    organization := "com.wayneyu.scalatron",

    scalaVersion := "2.9.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimise", "-explaintypes"),

    libraryDependencies ++= Seq(),

    // default parameters for scalatron play goal
    scalatronDir := file("/home/wayneyu/scala/Scalatron"),
    headless := false,
    steps := 5000,
    maxSlaves := 5000,

    play <<= (scalatronDir, name, javaOptions, Keys.`package` in Compile, headless, steps, maxSlaves) map {
      (base, name, javaOptions, botJar, headless, steps, maxSlaves) =>
        require(base exists, "The setting '%s' must point to the base directory of an existing Scalatron installation.".format(scalatronDir.key.label))
        IO delete (base / "bots" / name)
        IO copyFile (botJar, base / "bots" / name / "ScalatronBot.jar")

        val headlessOpts = if (headless) Seq("-maxfps", "1000", "-headless", "yes") else Seq()

        Process("java" +: (javaOptions ++ Seq("-Ddebug=true", "-jar", "Scalatron.jar", "-browser", "no",
          "-x", "100", "-y", "100", "-steps", steps.toString, "-maxslaves", maxSlaves.toString) ++ headlessOpts), base / "bin").!

    },

    installBot <<= (scalatronDir, name, javaOptions, Keys.`package` in Compile, headless, steps, maxSlaves) map {
      (base, name, javaOptions, botJar, headless, steps, maxSlaves) =>
        require(base exists, "The setting '%s' must point to the base directory of an existing Scalatron installation.".format(scalatronDir.key.label))
        IO delete (base / "bots" / name)
        IO copyFile (botJar, base / "bots" / name / "ScalatronBot.jar")

        val headlessOpts = if (headless) Seq("-maxfps", "1000", "-headless", "yes") else Seq()
    }
  )

  val headless = SettingKey[Boolean]("headless", "whether the frontend should be started, or if we should just run in headless mode")
  val maxSlaves = SettingKey[Int]("maxSlaves", "maximum number of mini-bots to allow per scalatron master")
  val steps = SettingKey[Int]("steps", "number of simulation steps to make before the apocalypse")
  val scalatronDir = SettingKey[File]("scalatron-dir", "base directory of an existing Scalatron installation")

  val play = TaskKey[Unit]("play", "recompiles, packages and installs the bot, then starts Scalatron")
  val installBot = TaskKey[Unit]("installBot", "recompiles, packages and installs the bot")
}