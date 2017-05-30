import org.nlogo.headless.HeadlessWorkspace

object runPicSaintLoup {

  def main(args: Array[String]) {

    val workspace = HeadlessWorkspace.newInstance
    workspace.open(
      "models/MeatModel-V5.nlogo")

    workspace.command("random-seed 42")
    workspace.command("setup")
    workspace.command("repeat 10 [go]")
    workspace.command("save-csv")
    workspace.dispose()
  }
}