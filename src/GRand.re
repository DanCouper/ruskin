/* The randomisation function is cyclic, generating two values.
 * The `spare` value is held as internal state. */
let spare = ref(0.);
let hasSpare = ref(false);

/**
 * Marsaglia polar method for generating gaussian noise. Translation
 * of the [Java] code from https://en.wikipedia.org/wiki/Marsaglia_polar_method.
 * REVIEW aim is to produce a sequence, so unfold, or fold across an
 * array/list of length `GaussianRand.gen(N, stdDev)`.
 * TODO the end goal is generation of a sequence. That should allow defining
 * whether it ascend, descends, rises in the middle etc. But the actual values
 * should follow gaussian ditribution to ensure the result looks natural: they
 * should _tend_ to ascend/descend/etc in a linear/etc manner, rather than
 * rigidly do so.
 */
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
