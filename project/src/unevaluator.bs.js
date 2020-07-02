// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Types$MyNewProject = require("./Types.bs.js");
var Typing$MyNewProject = require("./Typing.bs.js");
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
                    return /* tuple */[
                            List.concat(/* :: */[
                                  match[0],
                                  /* :: */[
                                    match$1[0],
                                    /* [] */0
                                  ]
                                ]),
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
        case /* Efunc */3 :
            if (typeof res === "number") {
              return ;
            }
            switch (res.tag | 0) {
              case /* Rfunc */4 :
                  var env$prime_000 = /* tuple */[
                    res[0],
                    Types$MyNewProject.valToRes(ex[0])
                  ];
                  var env$prime_001 = res[3];
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
                  return constrainExp(delta, res[2], exs);
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
                    45,
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
                      var ctor_id = param[0];
                      var t = Typing$MyNewProject.getResType(delta, r$prime);
                      if (typeof t === "number") {
                        throw [
                              Caml_builtin_exceptions.match_failure,
                              /* tuple */[
                                "unevaluator.re",
                                63,
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
                        var k2 = constrainExp(delta, match[1], /* :: */[
                              /* tuple */[
                                /* :: */[
                                  /* tuple */[
                                    match[0],
                                    /* Rictor */Block.__(11, [
                                        ctor_id,
                                        t$1,
                                        r$prime
                                      ])
                                  ],
                                  env
                                ],
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
                              63,
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

function constrainExp(delta, exp, exs) {
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
    return /* tuple */[
            List.concat(/* :: */[
                  match$1[0],
                  /* :: */[
                    match$2[0],
                    /* [] */0
                  ]
                ]),
            /* [] */0
          ];
  }
  
}

function mergeCons(k1, k2) {
  if (k1 !== undefined && k2 !== undefined) {
    return /* tuple */[
            List.concat(/* :: */[
                  k1[0],
                  /* :: */[
                    k2[0],
                    /* [] */0
                  ]
                ]),
            List.concat(/* :: */[
                  k1[1],
                  /* :: */[
                    k2[1],
                    /* [] */0
                  ]
                ])
          ];
  }
  
}

function optionPred(x) {
  return x !== undefined;
}

exports.unevaluate = unevaluate;
exports.constrainExp = constrainExp;
exports.mergeCons = mergeCons;
exports.optionPred = optionPred;
/* No side effect */
