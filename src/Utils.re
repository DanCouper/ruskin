type radians = float;

type degrees = float;

let degreesToRadians = (d: degrees) : radians => d *. (Js.Math._PI /. 180.);

let radiansToDegrees = (r: radians) : degrees => r *. (180. /. Js.Math._PI);

let clamp = (x, lower, upper) => min(upper, max(lower, x));

let random = () => {
  let u1 = ref(0.0);
  let u2 = ref(0.0);
  while (u1^ <= min_float) {
    u1 := Random.float(1.0);
    u2 := Random.float(1.0);
  };
  sqrt((-2.0) *. log(u1^)) *. cos(Js.Math._PI *. 2. *. u2^);
};

/**
 * Marsaglia polar method for generating gaussian noise. Translation
 * of the [Java] code from https://en.wikipedia.org/wiki/Marsaglia_polar_method.
 * Uses an object to make retention of an internal state easier.
 * REVIEW convert it to not use an object: aim is to produce a sequence,
 * so producing two values and either unfolding, or folding across an
 * array/list of length {N} would suffice.
 * TODO the end goal is generation of a sequence. That should allow defining
 * whether it ascend, descends, rises in the middle etc. But the actual values
 * should follow gaussian ditribution to ensure the result looks natural: they
 * should _tend_ to ascend/descend/etc in a linear/etc manner, rather than
 * rigidly do so.
 */
let polarRand = {
  val hasSpare = ref(false);
  val spare = ref(0.);
  /* TODO an initWithSeed method to allow testing. */
  pub gen = (mean, stdDev) =>
    hasSpare^ ?
      {
        hasSpare := false;
        spare^ *. stdDev +. mean;
      } :
      {
        let rec genRand = (u, v, s, mu, sigma) =>
          s^ >= 1. || s^ == 0. ?
            {
              u := Random.float(1.0) *. 2. -. 1.;
              v := Random.float(1.0) *. 2. -. 1.;
              s := u^ *. u^ +. v^ *. v^;
              genRand(u, v, s, mu, sigma);
            } :
            {
              let mul = sqrt((-2.0) *. log(s^) /. s^);
              spare := v^ *. mul;
              hasSpare := true;
              mu +. sigma *. u^ *. mul;
            };
        genRand(ref(0.), ref(0.), ref(0.), mean, stdDev);
      }
};

module type GAUSSIAN_RAND = {
  let gen: (float, float) => float;
};

module GaussianRand : GAUSSIAN_RAND = {
  /* The randomisation function is cyclic, generating two values.
   * The `spare` value is held as internal state. */
  let spare = ref(0.);
  let hasSpare = ref(false);

  let rec internalGenRand = (u, v, s, mu, sigma) =>
    s^ >= 1. || s^ == 0. ?
      {
        u := Random.float(1.0) *. 2. -. 1.;
        v := Random.float(1.0) *. 2. -. 1.;
        s := u^ *. u^ +. v^ *. v^;
        internalGenRand(u, v, s, mu, sigma);
      } :
      {
        let mul = sqrt((-2.0) *. log(s^) /. s^);
        spare := v^ *. mul;
        hasSpare := true;
        mu +. sigma *. u^ *. mul;
      };

  let gen = (mean, stdDev) =>
    hasSpare^ ?
      {
        hasSpare := false;
        spare^ *. stdDev +. mean;
      } :
      {
        internalGenRand(ref(0.), ref(0.), ref(0.), mean, stdDev);
      }

};