(** Ex 6.1 *)
type figure =
  | Point
  | Circle of int
  | Rectangle of int * int
  | Square of int
;;

type loc_fig = {x : int; y : int; fig : figure};;
(*
長方形 Rectangle(h, v) に対して、頂点を(x,y),(x+h,y),(x,y+v),(x+h,y+v)
正方形 Square a に対して、頂点を(x,y),(x+a,y),(x,y+a),(x+a,y+a)
円 Circle r に対して、中心を(x,y)
とする。
 *)

let square x = x * x;;

let overlap f1 f2 = 
  let make_point (x, y) = {x = x; y = y; fig = Point} in
  let square_to_rectangle {x = sx; y = sy; fig = Square a} =
    {x = sx; y = sy; fig = Rectangle (a, a)} in
  let circle_point ({x = cx; y = cy; fig = Circle r}, p) =
    let d = square(cx - p.x) + square(cy - p.y) in d <= r in
  let rectangle_point ({x = rx; y = ry; fig = Rectangle (a, b)}, p) =
    rx <= p.x && p.x <= rx + a && ry <= p.y && p.y <= ry + b in
  let circle_circle ({x = cx1; y = cy1; fig = Circle r1},
		     {x = cx2; y = cy2; fig = Circle r2}) =
    let d = square (cx1 - cx2) + square (cy1 - cy2) in
    if r1 - r2 > 0 then r1 - r2 < d && d < r1 + r2
    else r2 - r1 < d && d < r1 + r2 in
  let circle_rectangle ({x = cx; y = cy; fig = Circle r},
			{x = rx; y = ry; fig = Rectangle (a, b)}) =
    circle_point ({x = cx; y = cy; fig = Circle r}, make_point(rx, ry))
    || circle_point ({x = cx; y = cy; fig = Circle r}, make_point(rx + a, ry))
    || circle_point ({x = cx; y = cy; fig = Circle r}, make_point(rx, ry + b))
    || circle_point ({x = cx; y = cy; fig = Circle r}, make_point(rx + a, ry + b))
    || rectangle_point ({x = rx; y = ry; fig = Rectangle (a, b)}, 
			make_point(cx - r, cy))
    || rectangle_point ({x = rx; y = ry; fig = Rectangle (a, b)}, 
			make_point(cx + r, cy))
    || rectangle_point ({x = rx; y = ry; fig = Rectangle (a, b)}, 
			make_point(cx, cy - r))
    || rectangle_point ({x = rx; y = ry; fig = Rectangle (a, b)}, 
			make_point(cx, cy + r)) in
  let rectangle_rectangle ({x = rx1; y = ry1; fig = Rectangle (a1, b1)},
			   {x = rx2; y = ry2; fig = Rectangle (a2, b2)}) =
    rectangle_point ({x = rx1; y = ry1; fig = Rectangle (a1, b1)}, 
		    make_point (rx2, ry2))
    || rectangle_point ({x = rx1; y = ry1; fig = Rectangle (a1, b1)}, 
		       make_point (rx2 + a2, ry2))
    || rectangle_point ({x = rx1; y = ry1; fig = Rectangle (a1, b1)}, 
		       make_point (rx2, ry2 + b2))
    || rectangle_point ({x = rx1; y = ry1; fig = Rectangle (a1, b1)}, 
		       make_point (rx2 + a1, ry2 + b2))
    || rectangle_point ({x = rx2; y = ry2; fig = Rectangle (a2, b2)}, 
		    make_point (rx1, ry1))
    || rectangle_point ({x = rx2; y = ry2; fig = Rectangle (a2, b2)}, 
		       make_point (rx1 + a1, ry1))
    || rectangle_point ({x = rx2; y = ry2; fig = Rectangle (a2, b2)}, 
		       make_point (rx2, ry2 + b2))
    || rectangle_point ({x = rx2; y = ry2; fig = Rectangle (a2, b2)}, 
		       make_point (rx1 + a1, ry1 + b1)) in
  match (f1.fig, f2.fig) with
  | (Point, Point) -> f1.x = f2.x && f1.y = f2.y
  | (Point, Circle r) -> circle_point (f2, f1)
  | (Circle r, Point) -> circle_point (f1, f2)
  | (Point, Rectangle (a, b)) -> rectangle_point (f2, f1)
  | (Rectangle (a, b), Point) -> rectangle_point (f1, f2)
  | (Point, Square a) -> rectangle_point ((square_to_rectangle f2), f1)
  | (Square a, Point) -> rectangle_point ((square_to_rectangle f1), f2)
  | (Circle r1, Circle r2) -> circle_circle (f1, f2)
  | (Circle r, Rectangle (a, b)) -> circle_rectangle (f1, f2)
  | (Rectangle (a, b), Circle r) -> circle_rectangle (f2, f1)
  | (Circle r, Square a) -> circle_rectangle (f1, (square_to_rectangle f2))
  | (Square a, Circle r) -> circle_rectangle (f2, (square_to_rectangle f1))
  | (Rectangle (a1, b1), Rectangle (a2, b2)) -> rectangle_rectangle (f1, f2)
  | (Rectangle (a, b), Square a1) -> rectangle_rectangle (f1, (square_to_rectangle f2))
  | (Square a1, Rectangle (a, b)) -> rectangle_rectangle ((square_to_rectangle f1), f2)
  | (Square a1, Square a2) -> rectangle_rectangle ((square_to_rectangle f1), 
					     (square_to_rectangle f2))
;;
