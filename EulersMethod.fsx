#load "../packages/FsLab.1.0.2/FsLab.fsx"

// Euler's Method
// common variables
let xa = 0.0;; // initial value of x 
let xb = 1.0;; // final value of x
let y = 1.0;;  // initial value of y
let y1 = 1.0;;  // initial value of first derivative of y'
let n = 100000.0; // number of iteration
let h = (xb - xa) / n;; 
let x = [xa..h..xb];;  

// First Order ODE
let f1 x y:float = cos x + sin y;;  // utility function

let rec euler1 f l y0 =
    match l with
    | x::rest -> euler1 f rest (y0 + (h*(f x y0)))
    | _  -> y0;;

euler1 f1 x y;;

// Second Order ODE
let f2 x y y1:float = (sin y*x) - y1;;  // utility function

let rec euler2 f l y0 y01=
    match l with
    | x::rest -> euler2 f rest (y0 + (h*y01)) (y1+(h*(f x y0 y01)))
    | _  -> y0;;

euler2 f2 x y y1;;
