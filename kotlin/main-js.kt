package rt

import jquery.*
import org.w3c.dom.CanvasRenderingContext2D
import org.w3c.dom.HTMLCanvasElement
import kotlin.browser.document
import kotlin.browser.window

val canvas = initCanvas()
fun initCanvas(): HTMLCanvasElement {
  val canvas = document.createElement("canvas") as HTMLCanvasElement
  val context = canvas.getContext("2d") as CanvasRenderingContext2D
  context.canvas.width  = window.get("innerWidth");
  context.canvas.height = window.get("innerHeight");
  document.body!!.appendChild(canvas)
  return canvas
}
val context: CanvasRenderingContext2D
  get() = canvas.getContext("2d") as CanvasRenderingContext2D

val width = 800
val height = 600
val scaleFactor = initScaleFactor()
fun initScaleFactor(): Double {
  val widthScale = canvas.width.toDouble() / 2 / width.toDouble() 
  val heightScale = canvas.height.toDouble() / 2 / height.toDouble() 
  return if (widthScale < heightScale) widthScale else heightScale
}

private fun scale(c: Double) = c * scaleFactor
private fun Vec3.scale() = Vec3(scale(x), -scale(y), scale(z))

fun main(args: Array<String>) {
  // TODO: load from a JSON definition
  val scene = listOf<Shape>(
    Sphere(Vec3(0.0, -200.0, 400.0).scale(), scale(300.0), Vec3(250.0, 128.0, 128.0)),
    Sphere(Vec3(50.0, 200.0, -200.0).scale(), scale(50.0), Vec3(0.0, 250.0, 128.0)),
    Plane(Vec3(0.0, -200.0, 0.0).scale(), Vec3(y=1.0), Vec3(250.0, 250.0, 250.0)))

  val canvasWidth = scale(width.toDouble()).toInt() / 2 * 2
  val canvasHeight = scale(height.toDouble()).toInt() / 2 * 2
  var ren = render(canvasWidth / 2, canvasHeight / 2, scale(1000.0), 
                   Vec3(0.0, 500.0, -400.0).scale(), scene)

  for (y in 0..canvasHeight-1) {
    for (x in 0..canvasWidth-1) {
      val color = ren[2*y*canvasWidth + x]
      context.fillStyle = "rgb(${color.x},${color.y},${color.z})"
      context.fillRect(x.toDouble() - 0.5, y.toDouble() - 0.5, x.toDouble() + 0.5, y.toDouble() + 0.5)
    }
  }
}
