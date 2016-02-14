#include <array>
#include <cmath>
#include <iostream>
#include <limits>
#include <memory>
#include <utility>
#include <vector>

template <typename T, typename... Args>
std::unique_ptr<T> MakeUnique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args&&>(args)...));
}

class Vec3 {
 public:
  Vec3() : coords_{0, 0, 0} {}
  explicit Vec3(std::array<double, 3> coords) : coords_(coords) {}
  Vec3(double x, double y, double z) : coords_{x, y, z} {}
  Vec3(const Vec3&) = default;
  Vec3& operator=(const Vec3&) = default;
  Vec3(Vec3&&) = default;
  Vec3& operator=(Vec3&&) = default;

  double x() const { return coords_[0]; }
  double y() const { return coords_[1]; }
  double z() const { return coords_[2]; }

  const std::array<double, 3>& coords() const { return coords_; }

  double norm() const { return std::sqrt(x() * x() + y() * y() + z() * z()); }

  friend Vec3 operator/(const Vec3& v, double d) {
    return {v.x() / d, v.y() / d, v.z() / d};
  }

  friend Vec3 operator*(double d, const Vec3& v) {
    return {v.x() * d, v.y() * d, v.z() * d};
  }

  friend Vec3 operator+(const Vec3& lhs, const Vec3& rhs) {
    return {lhs.x() + rhs.x(), lhs.y() + rhs.y(), lhs.z() + rhs.z()};
  }

  friend Vec3 operator-(const Vec3& l) { return {-l.x(), -l.y(), -l.z()}; }

  friend Vec3 operator-(const Vec3& lhs, const Vec3& rhs) {
    return lhs + (-rhs);
  }

  friend Vec3 Normalize(const Vec3& v) { return v / v.norm(); }

  friend Vec3 CrossProduct(const Vec3& lhs, const Vec3& rhs) {
    return {lhs.y() * rhs.z() - rhs.y() * lhs.z(),
            lhs.z() * rhs.x() - rhs.z() * lhs.x(),
            lhs.x() * rhs.y() - rhs.x() * lhs.y()};
  }

  friend double DotProduct(const Vec3& lhs, const Vec3& rhs) {
    return lhs.x() * rhs.x() + lhs.y() * rhs.y() + lhs.z() * rhs.z();
  }

 private:
  std::array<double, 3> coords_;
};

class Ray {
  public:
    Ray(Vec3 origin, Vec3 dir) : origin_(origin), dir_(dir) {}
    const Vec3& origin() const { return origin_; }
    const Vec3& dir() const { return dir_; }
    
private:
    Vec3 origin_;
    Vec3 dir_;
};

class Shape {
 public:
  Shape() = default;
  virtual ~Shape() {}

  virtual Vec3 color(const Vec3& p) const = 0;
  virtual Vec3 n(const Vec3& p) const = 0;

  virtual double RayIntersect(const Ray& ray) const = 0;

 private:
  Shape(const Shape&) = delete;
  Shape& operator=(const Shape&) = delete;

  Shape(Shape&&) = delete;
  Shape& operator=(Shape&&) = delete;
};

constexpr double kInf = std::numeric_limits<double>::infinity();
constexpr double kEps = 1e-6;

class Sphere : public Shape {
 public:
  explicit Sphere(const Vec3& center, double radius, const Vec3& color)
      : center_(center), radius_(radius), color_(color) {}

  const Vec3& center() const { return center_; }
  double radius() const { return radius_; }

  Vec3 color(const Vec3&) const override { return color_; }
  Vec3 n(const Vec3& point) const override {
    return Normalize(point - center());
  }

  double RayIntersect(const Ray& ray) const override {
    const Vec3 a = center() - ray.origin();
    const double p = DotProduct(a, ray.dir());

    const double d2 = DotProduct(a, a) - p * p;
    const double r2 = radius() * radius();
    if (d2 > r2) return -kInf;

    const double delta = std::sqrt(r2 - d2);
    return p - delta > 0 ? p - delta : p + delta;
  }

 private:
  Vec3 center_;
  double radius_;
  Vec3 color_;
};

class Plane : public Shape {
  public:
    explicit Plane(const Vec3& center, const Vec3& n, const Vec3& color)
    : center_(center), n_(Normalize(n)), x_(GetP(n_)), y_(CrossProduct(n_, x_)), color_(color) {
    }

    Vec3 color(const Vec3& p) const override {
      const Vec3 dir = p - center_;
      const int x_coord = DotProduct(dir, x_);
      const int y_coord = DotProduct(dir, y_);
      const int x_mod = x_coord > 0 ? x_coord % 100 - 50 : 50 + x_coord % 100;
      const int y_mod = y_coord > 0 ? y_coord % 100 - 50 : 50 + y_coord % 100;
      const int pr = x_mod * y_mod;
      return pr < 0 ? color_ : 0.5*color_;
    }

    Vec3 n(const Vec3&) const override { return n_; }

    double RayIntersect(const Ray& ray) const override {
      const double p1 = DotProduct(ray.dir(), n_);
      const double p2 = DotProduct(ray.origin() - center_, n_);
      if (std::abs(p2) < kEps) return 0;
      if (std::abs(p1) < kEps) return kInf;
      return -p2/p1;
    }

  private:
    static Vec3 GetP(const Vec3& n) {
      for (size_t t = 0; t < 3; ++t) {
        if (std::abs(n.coords()[t]) < kEps) {
          std::array<double, 3> r{0, 0, 0};
          r[t] = 1;
          return Vec3(r);
        }
      }
      return Normalize(Vec3{-n.y(), n.x(), 0});
    }

    Vec3 center_;
    Vec3 n_;
    Vec3 x_;
    Vec3 y_;
    Vec3 color_;
};


const double kBackgroundRadiation = 0.3;

Vec3 trace(const Ray& ray,
           const std::vector<std::unique_ptr<Shape>>& scene, const Vec3& light_source) {
  size_t index = 0;
  double distance = +kInf;
  for (size_t t = 0; t < scene.size(); ++t) {
    const auto sd = scene[t]->RayIntersect(ray);
    if (sd < 0) continue;
    if (sd < distance) {
      distance = sd;
      index = t;
    }
  }

  if (distance == kInf || distance == -kInf) {
    return {0, 0, 0};  // black
  }

  const Vec3 r_point = ray.origin() + (distance * (1 - kEps)) * ray.dir();
  const Vec3 light_vec = light_source - r_point;
  const double light_distance = light_vec.norm();
  const Vec3 light_dir = Normalize(light_vec);

  distance = +kInf;
  for (size_t t = 0; t < scene.size(); ++t) {
    const auto sd = scene[t]->RayIntersect({r_point, light_dir});
    if (sd < 0 || sd > light_distance) continue;
    if (sd < distance) {
      distance = sd;
      break;
    }
  }
  const Vec3 bgColor = kBackgroundRadiation * scene[index]->color(r_point);
  if (distance != kInf) {
    return bgColor;
  }

  return bgColor +
         (1 - kBackgroundRadiation) * std::abs(DotProduct(light_dir, 
               scene[index]->n(r_point))) *
             scene[index]->color(r_point);
}

std::vector<Vec3> render(int width, int height, double camera_depth,
                         const Vec3& light_source,
                         const std::vector<std::unique_ptr<Shape>>& scene) {
  std::vector<Vec3> result(4 * width * height);

  const Vec3 camera(0, 0, -camera_depth);
  for (int x = -width; x < width; ++x) {
    for (int y = -height; y < height; ++y) {
      const Vec3 ray_dir = Normalize(Vec3(x, y, 0) - camera);
      const Vec3 color = trace({camera, ray_dir}, scene, light_source);
      result[(y + height) * 2 * width + (x + width)] = color;
    }
  }

  return result;
}

int main() {
  int count;
  std::vector<std::unique_ptr<Shape>> scene;

  auto read_vector = []() {
    double x, y, z;
    std::cin >> x >> y >> z;
    return Vec3(x, -y, z);
  };

  auto read_color = []() {
    int x, y, z;
    std::cin >> x >> y >> z;
    return Vec3(x, y, z);
  };

  int width = 0, height = 0;
  double camera_depth;
  std::cin >> width >> height;
  std::cin >> camera_depth;

  const Vec3 light_source = read_vector();

  std::cin >> count;
  for (int i = 0; i < count; ++i) {
    char key;
    std::cin >> key;
    switch (key) {
      case 'S': {
        double x, y, z;
        double r, g, b;
        double R;
        std::cin >> x >> y >> z >> R >> r >> g >> b;
        scene.push_back(MakeUnique<Sphere>(Vec3(x, -y, z), R, Vec3{r, g, b}));
        break;
      }
      case 'P': {
        const Vec3 center = read_vector();
        const Vec3 n = read_vector();
        const Vec3 color = read_color();
        scene.push_back(MakeUnique<Plane>(center, n, color));
        break;
      }
    }
  }

  const auto ren = render(width, height, camera_depth, light_source, scene);

  std::cout << "P3 " << 2 * width << " " << 2 * height << " 255\n";
  for (const auto& c : ren) {
    std::cout << (int)c.x() << " " << (int)c.y() << " " << (int)c.z() << "\n";
  }

  return 0;
}
