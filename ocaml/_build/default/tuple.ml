open OUnit2
module Tuple = struct
let epsilon = 0.00001
type tuple = {
  x: float;
  y: float;
  z: float;
  w: float;
}
(* constructor for a point *)
let point x y z = {x = x; y = y; z = z; w = 1.0}
let vector x y z = {x = x; y = y; z = z; w = 0.0}

let make_tup tupp = match tupp with
  | (x, y, z, w) -> {x = x; y = y; z = z; w = w}
let is_point p = match p with
  | {x = _; y = _; z = _; w = 1.0} -> true
  | _ -> false

let is_vector v = match v with
  | {x = _; y = _; z = _; w = 0.0} -> true
  | _ -> false
let add t1 t2 = 
  if is_point t1 && is_point t2 then
    raise (Invalid_argument "cannot add two points")
  else
    {x = t1.x +. t2.x; y = t1.y +. t2.y; z = t1.z +. t2.z; w = t1.w +. t2.w}

let sub t1 t2 =
  if is_vector t1 && is_point t2 then
    raise (Invalid_argument "cannot subtract a point from a vector")
  else
    {x = t1.x -. t2.x; y = t1.y -. t2.y; z = t1.z -. t2.z; w = t1.w -. t2.w}

let negate t = 
  if is_vector t then 
    {x = -.t.x; y = -.t.y; z = -.t.z; w = -.t.w}
  else 
    raise (Invalid_argument "cannot negate a point")

let mult t s =
  {x = t.x *. s; y = t.y *. s; z = t.z *. s; w = t.w *. s}

let div t s =
  {x = t.x /. s; y = t.y /. s; z = t.z /. s; w = t.w /. s}

let mag t =
  sqrt (t.x *. t.x +. t.y *. t.y +. t.z *. t.z +. t.w *. t.w)

let flteq f1 f2 =
  abs_float (f1 -. f2) < epsilon
  
let equals t1 t2 =
  flteq t1.x t2.x && flteq t1.y t2.y && flteq t1.z t2.z && flteq t1.w t2.w

let norm t =
  let divisor = mag t in
  {x = t.x /. divisor; y = t.y /. divisor; 
   z = t.z /. divisor; w = t.w /. divisor}
end


let x = Tuple.point 4.3 (-4.2) 3.1
let tests = "test suite for tuple" >::: [
  "test-is_point" >:: (fun _ -> assert_equal true (Tuple.is_point (Tuple.point 4.3 (-4.2) 4.1)));

  "test-is_vector" >:: (fun _ -> assert_equal false (Tuple.is_point (Tuple.vector 4.3 (-4.2) 3.1)));

  "test-add" >:: (fun _ -> assert_equal true (Tuple.equals 
    (Tuple.make_tup (1.0, 1.0, 6.0, 1.0)) 
    (Tuple.add (Tuple.point 3.0 (-2.0) 5.0) (Tuple.vector (-2.0) 3.0 1.0))));

  "test-sub" >:: (fun _ -> assert_equal true (Tuple.equals 
    (Tuple.vector (-2.0) (-4.0) (-6.0)) 
    (Tuple.sub (Tuple.point 3.0 2.0 1.0) (Tuple.point 5.0 6.0 7.0))));

  "test-sub2" >:: (fun _ -> assert_equal true (Tuple.equals 
    (Tuple.point (-2.0) (-4.0) (-6.0)) 
    (Tuple.sub (Tuple.point 3.0 2.0 1.0) (Tuple.vector 5.0 6.0 7.0))));

  "test-sub3" >:: (fun _ -> assert_equal true (Tuple.equals 
    (Tuple.vector (-2.0) (-4.0) (-6.0)) 
    (Tuple.sub (Tuple.vector 3.0 2.0 1.0) (Tuple.vector 5.0 6.0 7.0))));
  
  "test-subzero" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.vector (-1.0) (-2.0) (-3.0))
    (Tuple.sub (Tuple.vector 0.0 0.0 0.0) (Tuple.vector 1.0 2.0 3.0))));
  "test-negate" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.make_tup (1.0, -2.0, 3.0, 0.0))
    (Tuple.negate (Tuple.make_tup (-1.0, 2.0, -3.0, 0.0)))));

  "test-mult" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.make_tup (3.5, -7.0, 10.5, -14.0))
    (Tuple.mult (Tuple.make_tup (1.0, -2.0, 3.0, -4.0)) 3.5)));
  
  "test-mult-frac" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.make_tup (0.5, -1.0, 1.5, -2.0))
    (Tuple.mult (Tuple.make_tup (1.0, -2.0, 3.0, -4.0)) 0.5)));
  
  "test-div" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.make_tup (0.5, -1.0, 1.5, -2.0))
    (Tuple.div (Tuple.make_tup (1.0, -2.0, 3.0, -4.0)) 2.0)));
  
  "test-mag1" >:: (fun _ -> assert_equal true (Tuple.flteq
    (Tuple.mag (Tuple.make_tup (1.0, 0.0, 0.0, 0.0)))
    1.0));
  
  "test-mag2" >:: (fun _ -> assert_equal true (Tuple.flteq
    (Tuple.mag (Tuple.make_tup (0.0, 1.0, 0.0, 0.0)))
    1.0));

  "test-mag3" >:: (fun _ -> assert_equal true (Tuple.flteq
    (Tuple.mag (Tuple.make_tup (0.0, 0.0, 1.0, 0.0)))
    1.0));
  
  "test-mag4" >:: (fun _ -> assert_equal true (Tuple.flteq
    (Tuple.mag (Tuple.vector 1.0 2.0 3.0))
    (sqrt 14.0)));

  "test-norm" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.vector 1.0 0.0 0.0)
    (let v = Tuple.vector 4.0 0.0 0.0 in Tuple.norm v)));
  "test-norm2" >:: (fun _ -> assert_equal true (Tuple.equals
    (Tuple.vector 0.26726 0.53452 0.80178)  
    (let v = Tuple.vector 1.0 2.0 3.0 in Tuple.norm v)))
] 

let _ = run_test_tt_main tests