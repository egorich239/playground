package rt

data class Vec3(val x: Double = 0.0, val y: Double = 0.0, val z: Double = 0.0) {
  val norm: Double
    get() = MathSqrt(x*x + y*y + z*z)

  operator fun div(d: Double) = Vec3(x/d, y/d, z/d)

  operator fun unaryMinus() = Vec3(-x, -y, -z)
  operator fun plus(o: Vec3) = Vec3(x + o.x, y + o.y, z + o.z)
  operator fun minus(o: Vec3) = this + (-o)

  fun Normalize() = this / norm

  infix fun CrossProduct(o: Vec3) = Vec3(
    y * o.z - o.y * z,
    z * o.x - o.z * x,
    x * o.y - o.x * y)

  infix fun DotProduct(o: Vec3) = x*o.x + y*o.y + z*o.z
}
operator fun Double.times(v: Vec3) = Vec3(this*v.x, this*v.y, this*v.z)

data class Ray(val origin: Vec3, val dir: Vec3)

interface Shape {
  fun color(p: Vec3): Vec3
  fun n(p: Vec3): Vec3
  fun RayIntersect(ray: Ray): Double
}

data class Sphere(val center: Vec3, val r: Double, val color: Vec3) : Shape {
  override fun color(p: Vec3) = color
  override fun n(p: Vec3) = (p - center).Normalize()
  override fun RayIntersect(ray: Ray): Double {
    val a = center - ray.origin
    val p = a DotProduct ray.dir
    val d2 = (a DotProduct a) - p*p
    val r2 = r*r
    if (d2 > r2) return Double.NEGATIVE_INFINITY

    val delta = MathSqrt(r2 - d2)
    return if (p - delta > 0) p - delta else p + delta
  }
}

const val EPS = 1e-6

data class Plane(val center: Vec3, val n: Vec3, val color: Vec3) : Shape {
  private val n_ = n.Normalize()
  private val x_ = GetP(n_)
  private val y_ = n_ CrossProduct x_

  override fun color(p: Vec3): Vec3 {
    val dir = p - center
    val xCoord = (dir DotProduct x_).toLong()
    val yCoord = (dir DotProduct y_).toLong()
    val xMod = if (xCoord > 0) xCoord % 100 - 50 else 50 + xCoord % 100
    val yMod = if (yCoord > 0) yCoord % 100 - 50 else 50 + yCoord % 100
    val pr = xMod * yMod
    return if (pr < 0) color else 0.5*color
  }

  override fun n(p: Vec3) = n_

  override fun RayIntersect(ray: Ray): Double {
    val p1 = ray.dir DotProduct n_
    val p2 = (ray.origin - center) DotProduct n_
    return (
      if (MathAbs(p2) < EPS) 0.0
      else if (MathAbs(p1) < EPS) Double.POSITIVE_INFINITY
      else -p2/p1)
  }

  companion object {
    private fun GetP(n: Vec3) = 
      if (MathAbs(n.x) < EPS) Vec3(1.0, 0.0, 0.0)
      else if (MathAbs(n.y) < EPS) Vec3(0.0, 1.0, 0.0)
      else if (MathAbs(n.z) < EPS) Vec3(0.0, 0.0, 1.0)
      else Vec3(-n.y, n.x, 0.0).Normalize()
  }
}

const val BACKGROUND_RADIATION = 0.3

fun trace(ray: Ray, scene: List<Shape>, lightSource: Vec3): Vec3 {
  var index = 0
  var distance = Double.POSITIVE_INFINITY
  for (t in 0..scene.size-1) {
    val sd = scene[t].RayIntersect(ray)
    if (0 <= sd && sd < distance) {
      distance = sd
      index = t
    }
  }

  if (distance.isInfinite()) return Vec3(0.0, 0.0, 0.0)

  val rPoint = ray.origin + distance * (1.0 - EPS) * ray.dir
  val lightVec = lightSource - rPoint
  val lightDistance = lightVec.norm
  val lightDir = lightVec.Normalize()

  distance = Double.POSITIVE_INFINITY
  for (t in 0..scene.size-1) {
    val sd = scene[t].RayIntersect(Ray(rPoint, lightDir))
    if (sd >= 0.0 && sd < lightDistance) {
      distance = sd
      break
    }
  }

  val bgColor = BACKGROUND_RADIATION * scene[index].color(rPoint)
  if (!distance.isInfinite()) return bgColor

  return bgColor + (1.0 - BACKGROUND_RADIATION) * MathAbs(lightDir DotProduct scene[index].n(rPoint)) * scene[index].color(rPoint)
}

fun render(width: Int, height: Int, cameraDepth : Double, lightSource: Vec3, scene: List<Shape>): Array<Vec3> {
  var result = Array<Vec3>(4 * width * height) { Vec3() }
  val camera = Vec3(0.0, 0.0, -cameraDepth)
  for (x in -width..width-1) {
    for (y in -height..height-1) {
      val rayDir = (Vec3(x.toDouble(), y.toDouble(), 0.0) - camera).Normalize()
      val color = trace(Ray(camera, rayDir), scene, lightSource)
      result[(y + height) * 2 * width + (x + width)] = color
    }
  }
  return result
}
