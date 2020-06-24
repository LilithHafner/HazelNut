// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Guesser$MyNewProject = require("./Guesser.bs.js");
var Refiner$MyNewProject = require("./Refiner.bs.js");
var Unevaluator$MyNewProject = require("./unevaluator.bs.js");

function updateHoleContext_h(delta, gs) {
  if (!gs) {
    return delta;
  }
  var match = gs[0];
  var xs = updateHoleContext_h(delta, gs[1]);
  return /* :: */[
          /* tuple */[
            match[1],
            /* tuple */[
              match[0],
              match[2]
            ]
          ],
          xs
        ];
}

function updateHoleContext(delta, h, gs) {
  return List.filter((function (param) {
                  return h !== param[0];
                }))(updateHoleContext_h(delta, gs));
}

function updateUnfilledHoles(gs) {
  if (!gs) {
    return /* [] */0;
  }
  var match = gs[0];
  return /* :: */[
          /* tuple */[
            match[1],
            match[3]
          ],
          updateUnfilledHoles(gs[1])
        ];
}

function optionPred(x) {
  return x !== undefined;
}

function guessAndCheck_h(delta, gamma, typ, exs, _i) {
  while(true) {
    var i = _i;
    if (i > 5) {
      return Pervasives.failwith("Guessing timed out");
    }
    var es = Guesser$MyNewProject.guess(delta, gamma, typ, i);
    var checked = List.filter((function (e) {
              return optionPred(Unevaluator$MyNewProject.constrainExp(e, exs));
            }))(es);
    if (checked) {
      return checked[0];
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function guessAndCheck(delta, gamma, typ, exs) {
  return guessAndCheck_h(delta, gamma, typ, exs, 1);
}

function fill(delta, holeFillings, gamma, h, typ, exs) {
  if (Refiner$MyNewProject.refinable(typ)) {
    var match = Refiner$MyNewProject.refine(gamma, typ, exs);
    var gs = match[1];
    var f_000 = /* tuple */[
      h,
      match[0]
    ];
    var f = /* :: */[
      f_000,
      holeFillings
    ];
    var delta$prime = updateHoleContext(delta, h, gs);
    var u = updateUnfilledHoles(gs);
    var k = /* tuple */[
      u,
      f
    ];
    return /* tuple */[
            k,
            delta$prime
          ];
  }
  var e = guessAndCheck(delta, gamma, typ, exs);
  var f_000$1 = /* tuple */[
    h,
    e
  ];
  var f$1 = /* :: */[
    f_000$1,
    holeFillings
  ];
  var delta$prime$1 = List.filter((function (param) {
            return h !== param[0];
          }))(delta);
  var k$1 = /* tuple */[
    /* [] */0,
    f$1
  ];
  return /* tuple */[
          k$1,
          delta$prime$1
        ];
}

exports.updateHoleContext_h = updateHoleContext_h;
exports.updateHoleContext = updateHoleContext;
exports.updateUnfilledHoles = updateUnfilledHoles;
exports.optionPred = optionPred;
exports.guessAndCheck_h = guessAndCheck_h;
exports.guessAndCheck = guessAndCheck;
exports.fill = fill;
/* No side effect */
