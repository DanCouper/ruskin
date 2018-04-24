type radians = float;

type degrees = float;

let degreesToRadians = (d: degrees) : radians => d *. (Js.Math._PI /. 180.);

let radiansToDegrees = (r: radians) : degrees => r *. (180. /. Js.Math._PI);

let clamp = (x, lower, upper) => min(upper, max(lower, x));
