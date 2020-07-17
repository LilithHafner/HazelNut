// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Tools$MyNewProject = require("./Tools.bs.js");
var Types$MyNewProject = require("./Types.bs.js");
var Typing$MyNewProject = require("./Typing.bs.js");
var Printer$MyNewProject = require("./Printer.bs.js");
var Evaluator$MyNewProject = require("./evaluator.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function unevaluate(delta, _res, _ex) {
  while(true) {
    var ex = _ex;
    var res = _res;
    if (typeof ex === "number") {
      if (ex === /* Top */0) {
        return /* tuple */[
                /* [] */0,
                /* [] */0
              ];
      }
      if (typeof res === "number") {
        if (res === /* Runit */1) {
          return /* tuple */[
                  /* [] */0,
                  /* [] */0
                ];
        } else {
          return ;
        }
      }
      switch (res.tag | 0) {
        case /* Rapp */5 :
        case /* Rhole */6 :
        case /* Rfst */8 :
        case /* Rsnd */9 :
        case /* Rictor */11 :
        case /* Rcase */12 :
            break;
        default:
          return ;
      }
    } else {
      switch (ex.tag | 0) {
        case /* Eint */0 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rint */0 :
                  if (ex[0] === res[0]) {
                    return /* tuple */[
                            /* [] */0,
                            /* [] */0
                          ];
                  } else {
                    return ;
                  }
              case /* Rapp */5 :
              case /* Rhole */6 :
              case /* Rfst */8 :
              case /* Rsnd */9 :
              case /* Rictor */11 :
              case /* Rcase */12 :
                  break;
              default:
                return ;
            }
            break;
        case /* Ebool */1 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rbool */2 :
                  if (ex[0] === res[0]) {
                    return /* tuple */[
                            /* [] */0,
                            /* [] */0
                          ];
                  } else {
                    return ;
                  }
              case /* Rapp */5 :
              case /* Rhole */6 :
              case /* Rfst */8 :
              case /* Rsnd */9 :
              case /* Rictor */11 :
              case /* Rcase */12 :
                  break;
              default:
                return ;
            }
            break;
        case /* Epair */2 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rpair */7 :
                  var match = unevaluate(delta, res[0], ex[0]);
                  var match$1 = unevaluate(delta, res[1], ex[1]);
                  if (match !== undefined && match$1 !== undefined) {
                    return merge(match, match$1);
                  } else {
                    return ;
                  }
              case /* Rapp */5 :
              case /* Rhole */6 :
              case /* Rfst */8 :
              case /* Rsnd */9 :
              case /* Rictor */11 :
              case /* Rcase */12 :
                  break;
              default:
                return ;
            }
            break;
        case /* Efunc */3 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rfunc */4 :
                  var env$prime_000 = /* tuple */[
                    res[0],
                    res
                  ];
                  var env$prime_001 = /* :: */[
                    /* tuple */[
                      res[1],
                      Types$MyNewProject.valToRes(ex[0])
                    ],
                    res[4]
                  ];
                  var env$prime = /* :: */[
                    env$prime_000,
                    env$prime_001
                  ];
                  var exs_000 = /* tuple */[
                    env$prime,
                    ex[1]
                  ];
                  var exs = /* :: */[
                    exs_000,
                    /* [] */0
                  ];
                  return constrainExp(delta, res[3], exs);
              case /* Rapp */5 :
              case /* Rhole */6 :
              case /* Rfst */8 :
              case /* Rsnd */9 :
              case /* Rictor */11 :
              case /* Rcase */12 :
                  break;
              default:
                return ;
            }
            break;
        case /* Ector */4 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rctor */10 :
                  if (ex[0] !== res[0]) {
                    return ;
                  }
                  _ex = ex[2];
                  _res = res[2];
                  continue ;
              case /* Rapp */5 :
              case /* Rhole */6 :
              case /* Rfst */8 :
              case /* Rsnd */9 :
              case /* Rictor */11 :
              case /* Rcase */12 :
                  break;
              default:
                return ;
            }
            break;
        
      }
    }
    if (typeof res !== "number") {
      switch (res.tag | 0) {
        case /* Rapp */5 :
            var r2 = res[1];
            if (!Types$MyNewProject.castable(r2)) {
              return ;
            }
            var v = Types$MyNewProject.resToVal(r2);
            if (v !== undefined) {
              _ex = /* Efunc */Block.__(3, [
                  v,
                  ex
                ]);
              _res = res[0];
              continue ;
            }
            throw [
                  Caml_builtin_exceptions.match_failure,
                  /* tuple */[
                    "unevaluator.re",
                    51,
                    20
                  ]
                ];
        case /* Rhole */6 :
            return /* tuple */[
                    /* :: */[
                      /* tuple */[
                        res[0],
                        /* :: */[
                          /* tuple */[
                            res[1],
                            ex
                          ],
                          /* [] */0
                        ]
                      ],
                      /* [] */0
                    ],
                    /* [] */0
                  ];
        case /* Rfst */8 :
            _ex = /* Epair */Block.__(2, [
                ex,
                /* Top */0
              ]);
            _res = res[0];
            continue ;
        case /* Rsnd */9 :
            _ex = /* Epair */Block.__(2, [
                /* Top */0,
                ex
              ]);
            _res = res[0];
            continue ;
        case /* Rictor */11 :
            _ex = /* Ector */Block.__(4, [
                res[0],
                res[1],
                ex
              ]);
            _res = res[2];
            continue ;
        case /* Rcase */12 :
            var env = res[2];
            var r$prime = res[0];
            var cons = List.filter(optionPred)(List.map((function(ex,r$prime,env){
                    return function (param) {
                      var match = param[1];
                      var p = match[0];
                      var ctor_id = param[0];
                      var t = Typing$MyNewProject.getResType(delta, r$prime);
                      if (typeof t === "number") {
                        throw [
                              Caml_builtin_exceptions.match_failure,
                              /* tuple */[
                                "unevaluator.re",
                                71,
                                24
                              ]
                            ];
                      }
                      if (t.tag === /* D */3) {
                        var t$1 = t[0];
                        var k1 = unevaluate(delta, r$prime, /* Ector */Block.__(4, [
                                ctor_id,
                                t$1,
                                /* Top */0
                              ]));
                        var patBinds = List.map((function (x) {
                                return getPatRes(x, p, /* Rictor */Block.__(11, [
                                              ctor_id,
                                              t$1,
                                              r$prime
                                            ]));
                              }), getPatIds(p));
                        var k2 = constrainExp(delta, match[1], /* :: */[
                              /* tuple */[
                                Pervasives.$at(patBinds, env),
                                ex
                              ],
                              /* [] */0
                            ]);
                        var x = mergeCons(k1, k2);
                        if (x !== undefined) {
                          return x;
                        } else {
                          return ;
                        }
                      }
                      throw [
                            Caml_builtin_exceptions.match_failure,
                            /* tuple */[
                              "unevaluator.re",
                              71,
                              24
                            ]
                          ];
                    }
                    }(ex,r$prime,env)), res[1]));
            if (cons) {
              return cons[0];
            } else {
              return ;
            }
        
      }
    }
    
  };
}

function getPatIds(p) {
  if (p.tag) {
    return Pervasives.$at(getPatIds(p[0]), getPatIds(p[1]));
  } else {
    return /* :: */[
            p[0],
            /* [] */0
          ];
  }
}

function getPatRes(id, p, r) {
  var x = getPatRes_h(id, p, r);
  if (x !== undefined) {
    return /* tuple */[
            id,
            x
          ];
  } else {
    return Pervasives.failwith("Id wasn't found in pattern");
  }
}

function getPatRes_h(id, p, r) {
  if (!p.tag) {
    if (p[0] === id) {
      return r;
    } else {
      return ;
    }
  }
  var match = getPatRes_h(id, p[0], r);
  var match$1 = getPatRes_h(id, p[1], r);
  if (match !== undefined) {
    if (match$1 !== undefined) {
      return Pervasives.failwith("The same variable id is bound in two places in the same pattern");
    } else {
      return /* Rfst */Block.__(8, [match]);
    }
  } else if (match$1 !== undefined) {
    return /* Rsnd */Block.__(9, [match$1]);
  } else {
    return Pervasives.failwith("Id not found in pattern");
  }
}

function constrainExp(delta, exp, exs) {
  console.log("Constrain");
  console.log(Printer$MyNewProject.string_of_exp(exp));
  console.log(Printer$MyNewProject.string_of_excons(exs));
  console.log("End");
  if (!exs) {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
  var match = exs[0];
  var match$1 = constrainExp(delta, exp, exs[1]);
  var match$2 = unevaluate(delta, Evaluator$MyNewProject.$$eval(match[0], exp), match[1]);
  if (match$1 !== undefined && match$2 !== undefined) {
    return merge(match$1, match$2);
  }
  
}

function merge(k1, k2) {
  return /* tuple */[
          merge_h(k1[0], k2[0]),
          Pervasives.$at(k1[1], k2[1])
        ];
}

function merge_h(_u1, _u2) {
  while(true) {
    var u2 = _u2;
    var u1 = _u1;
    if (!u1) {
      return u2;
    }
    _u2 = Tools$MyNewProject.add(u1[0], u2);
    _u1 = u1[1];
    continue ;
  };
}

function mergeCons(k1, k2) {
  if (k1 !== undefined && k2 !== undefined) {
    return merge(k1, k2);
  }
  
}

function optionPred(x) {
  return x !== undefined;
}

exports.unevaluate = unevaluate;
exports.getPatIds = getPatIds;
exports.getPatRes = getPatRes;
exports.getPatRes_h = getPatRes_h;
exports.constrainExp = constrainExp;
exports.merge = merge;
exports.merge_h = merge_h;
exports.mergeCons = mergeCons;
exports.optionPred = optionPred;
/* No side effect */
