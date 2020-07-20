// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Tools$MyNewProject = require("./Tools.bs.js");

function intToNum(i) {
  if (i <= 0) {
    return /* Ctor */Block.__(11, [
              0,
              /* Num */1,
              /* Unit */1
            ]);
  } else {
    return /* Ctor */Block.__(11, [
              1,
              /* Num */1,
              intToNum(i - 1 | 0)
            ]);
  }
}

function valToExp(v) {
  if (typeof v === "number") {
    return /* Unit */1;
  }
  switch (v.tag | 0) {
    case /* Vint */0 :
        return /* Int */Block.__(0, [v[0]]);
    case /* Vbool */1 :
        return /* Bool */Block.__(2, [v[0]]);
    case /* Vpair */2 :
        return /* Pair */Block.__(8, [
                  valToExp(v[0]),
                  valToExp(v[1])
                ]);
    case /* Vctor */3 :
        return /* Ctor */Block.__(11, [
                  v[0],
                  v[1],
                  valToExp(v[2])
                ]);
    
  }
}

function valToRes(v) {
  if (typeof v === "number") {
    return /* Runit */1;
  }
  switch (v.tag | 0) {
    case /* Vint */0 :
        return /* Rint */Block.__(0, [v[0]]);
    case /* Vbool */1 :
        return /* Rbool */Block.__(2, [v[0]]);
    case /* Vpair */2 :
        return /* Rpair */Block.__(7, [
                  valToRes(v[0]),
                  valToRes(v[1])
                ]);
    case /* Vctor */3 :
        return /* Rctor */Block.__(10, [
                  v[0],
                  v[1],
                  valToRes(v[2])
                ]);
    
  }
}

function exToExp(ex) {
  if (typeof ex === "number") {
    if (ex === /* Eunit */1) {
      return /* Unit */1;
    } else {
      return ;
    }
  }
  switch (ex.tag | 0) {
    case /* Epair */2 :
        var match = exToExp(ex[0]);
        var match$1 = exToExp(ex[1]);
        if (match !== undefined && match$1 !== undefined) {
          return /* Pair */Block.__(8, [
                    match,
                    match$1
                  ]);
        } else {
          return ;
        }
    case /* Ector */4 :
        var exp = exToExp(ex[2]);
        if (exp !== undefined) {
          return /* Ctor */Block.__(11, [
                    ex[0],
                    ex[1],
                    exp
                  ]);
        } else {
          return ;
        }
    default:
      return ;
  }
}

function resToVal(_res) {
  while(true) {
    var res = _res;
    if (typeof res === "number") {
      if (res === /* Runit */1) {
        return /* Vunit */0;
      } else {
        return ;
      }
    }
    switch (res.tag | 0) {
      case /* Rint */0 :
          return /* Vint */Block.__(0, [res[0]]);
      case /* Rbool */2 :
          return /* Vbool */Block.__(1, [res[0]]);
      case /* Rapp */5 :
          var r1 = res[0];
          if (typeof r1 === "number") {
            return ;
          }
          if (r1.tag !== /* Rfunc */4) {
            return ;
          }
          _res = $$eval(/* :: */[
                /* tuple */[
                  r1[0],
                  r1
                ],
                /* :: */[
                  /* tuple */[
                    r1[1],
                    res[1]
                  ],
                  r1[4]
                ]
              ], r1[3]);
          continue ;
      case /* Rpair */7 :
          var match = resToVal(res[0]);
          var match$1 = resToVal(res[1]);
          if (match !== undefined && match$1 !== undefined) {
            return /* Vpair */Block.__(2, [
                      match,
                      match$1
                    ]);
          } else {
            return ;
          }
      case /* Rctor */10 :
          var v = resToVal(res[2]);
          if (v !== undefined) {
            return /* Vctor */Block.__(3, [
                      res[0],
                      res[1],
                      v
                    ]);
          } else {
            return ;
          }
      default:
        return ;
    }
  };
}

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
          var match = $$eval(_env, e[0]);
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

function castable(res) {
  return resToVal(res) !== undefined;
}

var sigma = /* :: */[
  /* tuple */[
    /* List */0,
    /* :: */[
      /* tuple */[
        0,
        /* Unit_t */2
      ],
      /* :: */[
        /* tuple */[
          1,
          /* Pair_t */Block.__(2, [
              /* D */Block.__(3, [/* Num */1]),
              /* D */Block.__(3, [/* List */0])
            ])
        ],
        /* [] */0
      ]
    ]
  ],
  /* :: */[
    /* tuple */[
      /* Num */1,
      /* :: */[
        /* tuple */[
          0,
          /* Unit_t */2
        ],
        /* :: */[
          /* tuple */[
            1,
            /* D */Block.__(3, [/* Num */1])
          ],
          /* [] */0
        ]
      ]
    ],
    /* :: */[
      /* tuple */[
        /* Bool */2,
        /* :: */[
          /* tuple */[
            0,
            /* Unit_t */2
          ],
          /* :: */[
            /* tuple */[
              1,
              /* Unit_t */2
            ],
            /* [] */0
          ]
        ]
      ],
      /* [] */0
    ]
  ]
];

exports.sigma = sigma;
exports.intToNum = intToNum;
exports.valToExp = valToExp;
exports.valToRes = valToRes;
exports.exToExp = exToExp;
exports.resToVal = resToVal;
exports.castable = castable;
exports.$$eval = $$eval;
exports.getPatEnv = getPatEnv;
/* No side effect */
