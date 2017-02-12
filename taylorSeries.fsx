#load "../packages/FsLab.1.0.2/FsLab.fsx"

// http://people.math.sc.edu/girardi/m142/handouts/10sTaylorPolySeries.pdf
// http://fsharpnews.blogspot.hk/2012/06/taylor-series-expansion-of-cosine-that.html
//let cosine n x =
//  let rec loop i q t c =
//   if i=n then c else
//      loop (i + 1) (q + 10 + 8*i) (-t * x * x / float q) (c + t)
//  loop 0 2 1.0 0.0


// Sine
let x = 40.0;;
// let n = (x*3.0) ;;
let n = 100.0 ;;

let factorial n = [1.0..n] |> List.reduce (*);;
let term n x = (-1.0**n)*(((x ** (2.0 * n + 1.0))) / (factorial(2.0 * n + 1.0)));;
let points = [ for i in 0.0..n-1.0 -> term i x ];;

let remove_nan = List.filter (System.Double.IsNaN >> not)

List.sum (remove_nan points);;

sin x;;

