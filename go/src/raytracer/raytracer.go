package main

import (
	"fmt"
	"math"
	"strings"
)

const kEps float64 = 1e-6
const kBackgroundRadiation = 0.3

type Vec3 struct {
	x, y, z float64
}

func (v *Vec3) norm() float64 { return math.Sqrt(v.x*v.x + v.y*v.y + v.z*v.z) }

func Add(a, b *Vec3) *Vec3          { return &Vec3{a.x + b.x, a.y + b.y, a.z + b.z} }
func Sub(a, b *Vec3) *Vec3          { return &Vec3{a.x - b.x, a.y - b.y, a.z - b.z} }
func Neg(a *Vec3) *Vec3             { return &Vec3{-a.x, -a.y, -a.z} }
func Mult(a float64, b *Vec3) *Vec3 { return &Vec3{a * b.x, a * b.y, a * b.z} }
func Div(a *Vec3, b float64) *Vec3  { return &Vec3{a.x / b, a.y / b, a.z / b} }
func DotProduct(a, b *Vec3) float64 { return a.x*b.x + a.y*b.y + a.z*b.z }
func CrossProduct(a, b *Vec3) *Vec3 {
	return &Vec3{
		a.y*b.z - b.y*a.z,
		a.z*b.x - b.z*a.x,
		a.x*b.y - b.x*a.y}
}
func Normalize(a *Vec3) *Vec3 { return Div(a, a.norm()) }

type Ray struct {
	origin, dir Vec3
}

type Shape interface {
	color(p *Vec3) *Vec3
	n(p *Vec3) *Vec3
	RayIntersect(r *Ray) float64
}

type Sphere struct {
	center Vec3
	radius float64
	color_ Vec3
}

func (s *Sphere) color(p *Vec3) *Vec3 { return &s.color_ }
func (s *Sphere) n(p *Vec3) *Vec3     { return Normalize(Sub(p, &s.center)) }
func (s *Sphere) RayIntersect(r *Ray) float64 {
	a := Sub(&s.center, &r.origin)
	p := DotProduct(a, &r.dir)

	d2 := DotProduct(a, a) - p*p
	r2 := s.radius * s.radius

	if d2 > r2 {
		return math.Inf(-1)
	}

	delta := math.Sqrt(r2 - d2)
	if p-delta > 0 {
		return p - delta
	}
	return p + delta
}

type Plane struct {
	center_    Vec3
	n_, x_, y_ Vec3
	color_     Vec3
}

func MakePlane(center, n, color Vec3) *Plane {
	n_ := Normalize(&n)
	var x_ Vec3
	if math.Abs(n_.x) < kEps {
		x_ = Vec3{1, 0, 0}
	} else if math.Abs(n_.y) < kEps {
		x_ = Vec3{0, 1, 0}
	} else if math.Abs(n_.z) < kEps {
		x_ = Vec3{0, 0, 1}
	} else {
		x_ = *Normalize(&Vec3{-n_.y, n.x, 0})
	}
	y_ := *CrossProduct(n_, &x_)
	return &Plane{center, *n_, x_, y_, color}
}

func (s *Plane) color(p *Vec3) *Vec3 {
	dir := Sub(p, &s.center_)
	x_coord := int(DotProduct(dir, &s.x_))
	y_coord := int(DotProduct(dir, &s.y_))
	x_mod := (x_coord%100+100)%100 - 50
	y_mod := (y_coord%100+100)%100 - 50
	if x_mod*y_mod < 0 {
		return &s.color_
	}
	return Mult(0.5, &s.color_)
}

func (s *Plane) n(p *Vec3) *Vec3 { return &s.n_ }

func (s *Plane) RayIntersect(r *Ray) float64 {
	p1 := DotProduct(&r.dir, &s.n_)
	p2 := DotProduct(Sub(&r.origin, &s.center_), &s.n_)
	if math.Abs(p2) < kEps {
		return 0
	}
	if math.Abs(p1) < kEps {
		return math.Inf(1)
	}
	return -p2 / p1
}

func trace(r *Ray, scene []Shape, l *Vec3) *Vec3 {
	index := 0
	distance := math.Inf(1)

	for t, shape := range scene {
		if shape == nil {
			continue
		}
		sd := shape.RayIntersect(r)
		if sd < 0 {
			continue
		}
		if sd < distance {
			distance = sd
			index = t
		}
	}

	if math.IsInf(distance, 0) {
		return &Vec3{}
	}

	r_point := Add(&r.origin, Mult(distance*(1.0-kEps), &r.dir))
	light_vec := Sub(l, r_point)
	light_distance := light_vec.norm()
	light_dir := Normalize(light_vec)

	distance = math.Inf(1)
	for _, shape := range scene {
		if shape == nil {
			continue
		}
		sd := shape.RayIntersect(&Ray{*r_point, *light_dir})
		if sd < 0 || sd > light_distance {
			continue
		}
		if sd < distance {
			distance = sd
			break
		}
	}

	bgColor := Mult(kBackgroundRadiation, scene[index].color(r_point))
	if !math.IsInf(distance, 0) {
		return bgColor
	}

	return Add(
		bgColor,
		Mult((1-kBackgroundRadiation)*
			math.Abs(DotProduct(light_dir, scene[index].n(r_point))),
			scene[index].color(r_point)))
}

func render(width, height int, camera_depth float64, l *Vec3, scene []Shape) *[]Vec3 {
	result := make([]Vec3, 4*width*height)

	camera := &Vec3{0, 0, -camera_depth}
	for x := -width; x < width; x++ {
		for y := -height; y < height; y++ {
			ray_dir := Normalize(Sub(&Vec3{float64(x), float64(y), 0}, camera))
			color := trace(&Ray{*camera, *ray_dir}, scene, l)
			result[(y+height)*2*width+(x+width)] = *color
		}
	}
	return &result
}

func read_vector() *Vec3 {
	var x, y, z float64
	fmt.Scan(&x, &y, &z)
	return &Vec3{x, -y, z}
}

func read_color() *Vec3 {
	var x, y, z int
	fmt.Scan(&x, &y, &z)
	return &Vec3{float64(x), float64(y), float64(z)}
}

func main() {
	width, height := 0, 0
	var camera_depth float64 = 0
	fmt.Scan(&width, &height, &camera_depth)

	light_source := read_vector()

	count := 0
	fmt.Scan(&count)

	scene := make([]Shape, count)
	for i := 0; i < count; i++ {
		var key string
		fmt.Scan(&key)

		switch strings.Trim(key, " ") {
		case "S":
			center := read_vector()
			var radius float64
			fmt.Scan(&radius)
			color := read_color()
			scene[i] = &Sphere{*center, radius, *color}
		case "P":
			center := read_vector()
			n := read_vector()
			color := read_color()
			scene[i] = MakePlane(*center, *n, *color)
		}
	}

	ren := render(width, height, camera_depth, light_source, scene)

	fmt.Print("P3 ", 2*width, " ", 2*height, " 255\n")
	for _, c := range *ren {
		fmt.Print(int(c.x), " ", int(c.y), " ", int(c.z), "\n")
	}
}
