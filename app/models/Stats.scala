package models

import concurrent.stm.Ref

/**
 * Based on SpeedOmeter from play2 sample projects
 */
object Stats {

  val cpu = new models.CPU()

  def getAll(): String = getRequestsPerSecond + " " + getHeap + " " + getCPU + " " + getThreads

  def getRequestsPerSecond = Stats.getSpeed +":rps"
  def getHeap = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024*1024) + ":memory"
  def getCPU = (cpu.getCpuUsage()*1000).round / 10.0 + ":cpu"
  def getThreads = Thread.activeCount()+":thrds"

  val unit = 100

  private val counter = Ref((0,(0,java.lang.System.currentTimeMillis())))

  def countRequest() = {
    val current = java.lang.System.currentTimeMillis()
    counter.single.transform {
      case (precedent,(count,millis)) if current > millis + unit => (0, (1,current))
      case (precedent,(count,millis)) if current > millis + (unit/2) => (count, (1, current))
      case (precedent,(count,millis))  => (precedent,(count + 1, millis))
    }
  }

  def getSpeed = {
    val current = java.lang.System.currentTimeMillis()
    val (precedent,(count,millis)) = counter.single()
    val since = current-millis
    if(since <= unit) ((count + precedent) * 1000) / (since + unit/2)
    else 0
  }

}
