// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Evaluator$MyNewProject = require("./evaluator.bs.js");

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
          _res = Evaluator$MyNewProject.$$eval(/* :: */[
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

function castable(res) {
  return resToVal(res) !== undefined;
}

exports.intToNum = intToNum;
exports.valToExp = valToExp;
exports.valToRes = valToRes;
exports.exToExp = exToExp;
exports.resToVal = resToVal;
exports.castable = castable;
/* No side effect */
