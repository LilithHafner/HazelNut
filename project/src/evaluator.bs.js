// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Tools$MyNewProject = require("./Tools.bs.js");
var Printer$MyNewProject = require("./Printer.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function $$eval(__env, _e) {
  while(true) {
    var e = _e;
    var _env = __env;
    if (typeof e === "number") {
      if (e === /* Nil */0) {
        return /* Rnil */0;
      } else {
        return /* Runit */1;
      }
    }
    switch (e.tag | 0) {
      case /* Int */0 :
          return /* Rint */Block.__(0, [e[0]]);
      case /* Float */1 :
          return /* Rfloat */Block.__(1, [e[0]]);
      case /* Bool */2 :
          return /* Rbool */Block.__(2, [e[0]]);
      case /* Cons */3 :
          return /* Rcons */Block.__(3, [
                    $$eval(_env, e[0]),
                    $$eval(_env, e[1])
                  ]);
      case /* Function */4 :
          return /* Rfunc */Block.__(4, [
                    e[0],
                    e[1],
                    e[2],
                    e[3],
                    _env
                  ]);
      case /* Application */5 :
          var r1 = $$eval(_env, e[0]);
          var r2 = $$eval(_env, e[1]);
          if (typeof r1 === "number") {
            return /* Rapp */Block.__(5, [
                      r1,
                      r2
                    ]);
          }
          if (r1.tag !== /* Rfunc */4) {
            return /* Rapp */Block.__(5, [
                      r1,
                      r2
                    ]);
          }
          _e = r1[3];
          __env = /* :: */[
            /* tuple */[
              r1[0],
              r1
            ],
            /* :: */[
              /* tuple */[
                r1[1],
                r2
              ],
              r1[4]
            ]
          ];
          continue ;
      case /* Hole */6 :
          return /* Rhole */Block.__(6, [
                    e[0],
                    _env
                  ]);
      case /* Var */7 :
          return Tools$MyNewProject.lookup(e[0], _env);
      case /* Pair */8 :
          return /* Rpair */Block.__(7, [
                    $$eval(_env, e[0]),
                    $$eval(_env, e[1])
                  ]);
      case /* Fst */9 :
          return /* Rfst */Block.__(8, [$$eval(_env, e[0])]);
      case /* Snd */10 :
          return /* Rsnd */Block.__(9, [$$eval(_env, e[0])]);
      case /* Ctor */11 :
          return /* Rctor */Block.__(10, [
                    e[0],
                    e[1],
                    $$eval(_env, e[2])
                  ]);
      case /* Case */12 :
          var e1 = e[0];
          console.log("evaluator case");
          console.log(Printer$MyNewProject.string_of_exp(e1));
          console.log(Printer$MyNewProject.string_of_res($$eval(_env, e1)));
          var match = $$eval(_env, e1);
          if (typeof match === "number") {
            return Pervasives.failwith("Type error: expected a constructor within case");
          }
          if (match.tag !== /* Rctor */10) {
            return Pervasives.failwith("Type error: expected a constructor within case");
          }
          var match$1 = Tools$MyNewProject.lookup(match[0], e[1]);
          _e = match$1[1];
          __env = Pervasives.$at(getPatEnv(match$1[0], match[2]), _env);
          continue ;
      
    }
  };
}

function evalAndFill(env, e, f) {
  return fillRes($$eval(env, e), f);
}

function fillEnv(env, f) {
  return List.map((function (param) {
                return /* tuple */[
                        param[0],
                        fillRes(param[1], f)
                      ];
              }), env);
}

function fillRes(r, f) {
  if (typeof r === "number") {
    return r;
  }
  switch (r.tag | 0) {
    case /* Rfunc */4 :
        return /* Rfunc */Block.__(4, [
                  r[0],
                  r[1],
                  r[2],
                  fillExp(r[3], f),
                  fillEnv(r[4], f)
                ]);
    case /* Rapp */5 :
        return /* Rapp */Block.__(5, [
                  fillRes(r[0], f),
                  fillRes(r[1], f)
                ]);
    case /* Rhole */6 :
        var env = r[1];
        var x = r[0];
        try {
          return evalAndFill(env, fillExp(Tools$MyNewProject.lookup(x, f), f), f);
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            return /* Rhole */Block.__(6, [
                      x,
                      fillEnv(env, f)
                    ]);
          }
          throw exn;
        }
    case /* Rpair */7 :
        return /* Rpair */Block.__(7, [
                  fillRes(r[0], f),
                  fillRes(r[1], f)
                ]);
    case /* Rfst */8 :
        return /* Rfst */Block.__(8, [fillRes(r[0], f)]);
    case /* Rsnd */9 :
        return /* Rsnd */Block.__(9, [fillRes(r[0], f)]);
    case /* Rctor */10 :
        return /* Rctor */Block.__(10, [
                  r[0],
                  r[1],
                  fillRes(r[2], f)
                ]);
    case /* Rictor */11 :
        return /* Rictor */Block.__(11, [
                  r[0],
                  r[1],
                  fillRes(r[2], f)
                ]);
    case /* Rcase */12 :
        return /* Rcase */Block.__(12, [
                  fillRes(r[0], f),
                  List.map((function (param) {
                          var match = param[1];
                          return /* tuple */[
                                  param[0],
                                  /* tuple */[
                                    match[0],
                                    fillExp(match[1], f)
                                  ]
                                ];
                        }), r[1]),
                  fillEnv(r[2], f)
                ]);
    default:
      return r;
  }
}

function fillExp(exp, f) {
  if (typeof exp === "number") {
    if (exp !== /* Nil */0) {
      return /* Unit */1;
    }
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "evaluator.re",
            81,
            26
          ]
        ];
  } else {
    switch (exp.tag | 0) {
      case /* Int */0 :
          return /* Int */Block.__(0, [exp[0]]);
      case /* Float */1 :
          return /* Float */Block.__(1, [exp[0]]);
      case /* Bool */2 :
          return /* Bool */Block.__(2, [exp[0]]);
      case /* Cons */3 :
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "evaluator.re",
                  81,
                  26
                ]
              ];
      case /* Function */4 :
          return /* Function */Block.__(4, [
                    exp[0],
                    exp[1],
                    exp[2],
                    fillExp(exp[3], f)
                  ]);
      case /* Application */5 :
          return /* Application */Block.__(5, [
                    fillExp(exp[0], f),
                    fillExp(exp[1], f)
                  ]);
      case /* Hole */6 :
          var x = exp[0];
          try {
            return fillExp(Tools$MyNewProject.lookup(x, f), f);
          }
          catch (exn){
            if (exn === Caml_builtin_exceptions.not_found) {
              return /* Hole */Block.__(6, [x]);
            }
            throw exn;
          }
      case /* Var */7 :
          return /* Var */Block.__(7, [exp[0]]);
      case /* Pair */8 :
          return /* Pair */Block.__(8, [
                    fillExp(exp[0], f),
                    fillExp(exp[1], f)
                  ]);
      case /* Fst */9 :
          return /* Fst */Block.__(9, [fillExp(exp[0], f)]);
      case /* Snd */10 :
          return /* Snd */Block.__(10, [fillExp(exp[0], f)]);
      case /* Ctor */11 :
          return /* Ctor */Block.__(11, [
                    exp[0],
                    exp[1],
                    fillExp(exp[2], f)
                  ]);
      case /* Case */12 :
          return /* Case */Block.__(12, [
                    fillExp(exp[0], f),
                    List.map((function (param) {
                            var match = param[1];
                            return /* tuple */[
                                    param[0],
                                    /* tuple */[
                                      match[0],
                                      fillExp(match[1], f)
                                    ]
                                  ];
                          }), exp[1])
                  ]);
      
    }
  }
}

function getPatEnv(pat, r) {
  if (pat.tag) {
    if (typeof r === "number" || r.tag !== /* Rpair */7) {
      return Pervasives.failwith("Result does not match constructor pattern");
    } else {
      return Pervasives.$at(getPatEnv(pat[0], r[0]), getPatEnv(pat[1], r[1]));
    }
  } else {
    return /* :: */[
            /* tuple */[
              pat[0],
              r
            ],
            /* [] */0
          ];
  }
}

exports.$$eval = $$eval;
exports.evalAndFill = evalAndFill;
exports.fillEnv = fillEnv;
exports.fillRes = fillRes;
exports.fillExp = fillExp;
exports.getPatEnv = getPatEnv;
/* No side effect */
