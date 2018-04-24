// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE

import * as Random from "bs-platform/lib/es6/random.js";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as CamlinternalOO from "bs-platform/lib/es6/camlinternalOO.js";

var shared = ["gen"];

function degreesToRadians(d) {
  return d * (Math.PI / 180);
}

function radiansToDegrees(r) {
  return r * (180 / Math.PI);
}

function clamp(x, lower, upper) {
  return Caml_obj.caml_min(upper, Caml_obj.caml_max(lower, x));
}

function random() {
  var u1 = 0.0;
  var u2 = 0.0;
  while(u1 <= Number.MIN_VALUE) {
    u1 = Random.$$float(1.0);
    u2 = Random.$$float(1.0);
  };
  return Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(Math.PI * 2 * u2);
}

var $$class = CamlinternalOO.create_table(shared);

var ids = CamlinternalOO.new_methods_variables($$class, shared, [
      "hasSpare",
      "spare"
    ]);

var gen = ids[0];

var hasSpare = ids[1];

var spare = ids[2];

CamlinternalOO.set_method($$class, gen, (function (self$1, mean, stdDev) {
        var match = self$1[hasSpare][0];
        if (match !== 0) {
          self$1[hasSpare][0] = /* false */0;
          return self$1[spare][0] * stdDev + mean;
        } else {
          var u = [0];
          var v = [0];
          var s = [0];
          var mu = mean;
          var sigma = stdDev;
          while(true) {
            var match$1 = +(s[0] >= 1 || s[0] === 0);
            if (match$1 !== 0) {
              u[0] = Random.$$float(1.0) * 2 - 1;
              v[0] = Random.$$float(1.0) * 2 - 1;
              s[0] = u[0] * u[0] + v[0] * v[0];
              continue ;
              
            } else {
              var mul = Math.sqrt(-2.0 * Math.log(s[0]) / s[0]);
              self$1[spare][0] = v[0] * mul;
              self$1[hasSpare][0] = /* true */1;
              return mu + sigma * u[0] * mul;
            }
          };
        }
      }));

function obj_init() {
  var self = CamlinternalOO.create_object_opt(0, $$class);
  self[hasSpare] = [/* false */0];
  self[spare] = [0];
  return self;
}

CamlinternalOO.init_class($$class);

var polarRand = obj_init(0);

var spare$1 = [0];

var hasSpare$1 = [/* false */0];

function gen$1(mean, stdDev) {
  var match = hasSpare$1[0];
  if (match !== 0) {
    hasSpare$1[0] = /* false */0;
    return spare$1[0] * stdDev + mean;
  } else {
    var u = [0];
    var v = [0];
    var s = [0];
    var mu = mean;
    var sigma = stdDev;
    while(true) {
      var match$1 = +(s[0] >= 1 || s[0] === 0);
      if (match$1 !== 0) {
        u[0] = Random.$$float(1.0) * 2 - 1;
        v[0] = Random.$$float(1.0) * 2 - 1;
        s[0] = u[0] * u[0] + v[0] * v[0];
        continue ;
        
      } else {
        var mul = Math.sqrt(-2.0 * Math.log(s[0]) / s[0]);
        spare$1[0] = v[0] * mul;
        hasSpare$1[0] = /* true */1;
        return mu + sigma * u[0] * mul;
      }
    };
  }
}

var GaussianRand = /* module */[/* gen */gen$1];

export {
  degreesToRadians ,
  radiansToDegrees ,
  clamp ,
  random ,
  polarRand ,
  GaussianRand ,
  
}
/* class Not a pure module */