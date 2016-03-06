package rt

import java.util.Scanner
import java.lang.System

private fun Scanner.nextVector() = Vec3(nextDouble(), -nextDouble(), nextDouble())
private fun Scanner.nextColor() = Vec3(nextInt().toDouble(), nextInt().toDouble(), nextInt().toDouble())

fun main(args: Array<String>) {
  val s = Scanner(System.`in`!!)

  val width = s.nextInt()
  val height = s.nextInt() 
  val cameraDepth = s.nextDouble()
  val lightSource = s.nextVector()
  val count = s.nextInt()

  var scene = mutableListOf<Shape>()
  for (i in 1..count) {
    when (s.next()) {
      "S" -> scene.add(Sphere(s.nextVector(), s.nextDouble(), s.nextColor()))
      "P" -> scene.add(Plane(s.nextVector(), s.nextVector(), s.nextColor()))
    }
  }

  val ren = render(width, height, cameraDepth, lightSource, scene)

  println("P3 ${2*width} ${2*height} 255")
  for (c in ren) {
    println("${c.x.toInt()} ${c.y.toInt()} ${c.z.toInt()}")
  }
}
