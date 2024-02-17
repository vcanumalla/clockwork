pub mod type_utils {
    use std::ops::{Add, Div, Mul, Neg, Sub};
    pub struct Vec3 {
        e: [f64; 3],
    }

    pub type Point = Vec3;
    pub type Color = Vec3;

    impl Vec3 {
        pub fn new() -> Vec3 {
            Vec3 { e : [0.0, 0.0, 0.0] }
        }
        pub fn newv(e0: f64, e1 : f64, e2 : f64) -> Vec3 {
            Vec3 {e : [e0, e1, e2]}
        }
        
        pub fn x(&self) -> f64 {
            self.e[0]
        }

        pub fn y(&self) -> f64 {
            self.e[1]
        }

        pub fn z(&self) -> f64 {
            self.e[2]
        }
        
        pub fn length_squared(&self) -> f64 {
            (self.e[0] * self.e[0]) + (self.e[1] * self.e[1]) + (self.e[2] * self.e[2])
        }
        pub fn length(&self) -> f64 {
            self.length_squared().sqrt()
        }
        pub fn dot(self, rhs: Vec3) -> f64 {
            // (self * rhs).e.iter().sum()
            (self.e[0] * rhs.e[0]) + (self.e[1] * rhs.e[1]) + (self.e[2] * rhs.e[2])
        }
    }

    impl Neg for Vec3 {
        type Output = Vec3;

        fn neg(self) -> Vec3 {
            Vec3 { e: [-self.e[0], -self.e[1], -self.e[2]] }
        }
    }
    impl Add for Vec3 {
        type Output = Vec3;

        fn add(self, rhs: Vec3) -> Vec3 {
            Vec3 { e: [self.e[0] + rhs.e[0], self.e[1] + rhs.e[1], self.e[2] + rhs.e[2]] }
        }
    }

    impl Mul<f64> for Vec3 {
        type Output = Vec3;

        fn mul(self, rhs: f64) -> Vec3 {
            Vec3 { e: [self.e[0] * rhs, self.e[1] * rhs, self.e[2] * rhs] }
        }
    }

    impl Div<f64> for Vec3 {
        type Output = Vec3;

        fn div(self, rhs: f64) -> Vec3 {
            self * (1.0 / rhs)
        }
    }
    impl Sub for Vec3 {
        type Output = Vec3;
        fn sub(self, rhs: Vec3) -> Vec3 {
            return self + -rhs;
        }

    }
    impl Mul<Vec3> for Vec3 {
        type Output = Vec3;
        fn mul(self, rhs: Vec3) -> Vec3 {
            Vec3 {e : [self.e[0] * rhs.e[0], self.e[1] * rhs.e[1], self.e[2] * rhs.e[2]]}
        }
    }
    impl Color {
        pub fn write_color(self) {

            println!("{0} {1} {2}", self.x() as i32, self.y() as i32, self.z() as i32);
        }
    }

    pub struct Ray {
        e : (Point, Vec3),
    }

    impl Ray {
        fn origin(self) -> Point {
            self.e.0
        }

        fn direction(self) -> Vec3 {
            self.e.1
        }

        fn at(self, t : f64) -> Point {
            return self.e.0 + (self.e.1  *t)
        }
    }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_vector_addition() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = type_utils::Vec3::newv(4.0, 5.0, 6.0);
    let v3 = v1 + v2;
    assert_eq!(v3.x(), 5.0);
    assert_eq!(v3.y(), 7.0);
    assert_eq!(v3.z(), 9.0);
  }
  #[test]
  fn test_negation() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = -v1;
    assert_eq!(v2.x(), -1.0);
    assert_eq!(v2.y(), -2.0);
    assert_eq!(v2.z(), -3.0);
  }
  #[test]
  fn test_division() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = v1 / 2.0;
    assert_eq!(v2.x(), 0.5);
    assert_eq!(v2.y(), 1.0);
    assert_eq!(v2.z(), 1.5);
  }
  #[test]
  fn test_scalar_multiplication() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = v1 * 2.0;
    assert_eq!(v2.x(), 2.0);
    assert_eq!(v2.y(), 4.0);
    assert_eq!(v2.z(), 6.0);
  }
  #[test]
  fn test_vector_multiplication() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = type_utils::Vec3::newv(4.0, 5.0, 6.0);
    let v3 = v1 * v2;
    assert_eq!(v3.x(), 4.0);
    assert_eq!(v3.y(), 10.0);
    assert_eq!(v3.z(), 18.0);
  }
  #[test]
  fn test_vector_subtraction() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = type_utils::Vec3::newv(4.0, 5.0, 6.0);
    let v3 = v1 - v2;
    assert_eq!(v3.x(), -3.0);
    assert_eq!(v3.y(), -3.0);
    assert_eq!(v3.z(), -3.0);
  }
  #[test]
  fn test_dot_product() {
    let v1 = type_utils::Vec3::newv(1.0, 2.0, 3.0);
    let v2 = type_utils::Vec3::newv(4.0, 5.0, 6.0);
    let dot = v1.dot(v2);
    assert_eq!(dot, 32.0);
  }

}