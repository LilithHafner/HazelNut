// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");

function make_list(n, f) {
  var out = /* [] */0;
  for(var i = n - 1 | 0; i >= 0; --i){
    out = /* :: */[
      Curry._1(f, i),
      out
    ];
  }
  return out;
}

function explode(str) {
  return make_list(str.length, (function (param) {
                return Caml_string.get(str, param);
              }));
}

function implode(cs) {
  return $$String.init(List.length(cs), (function (param) {
                return List.nth(cs, param);
              }));
}

function parse_token(x) {
  var parse_token_r = function (_x, _y) {
    while(true) {
      var y = _y;
      var x = _x;
      if (!x) {
        return /* tuple */[
                x,
                y
              ];
      }
      var c = x[0];
      var switcher = c - 32 | 0;
      if (switcher > 9 || switcher < 0) {
        if (switcher === 12) {
          return /* tuple */[
                  x[1],
                  y
                ];
        }
        _y = /* :: */[
          c,
          y
        ];
        _x = x[1];
        continue ;
      }
      if (switcher > 8 || switcher < 1) {
        return /* tuple */[
                x[1],
                y
              ];
      }
      _y = /* :: */[
        c,
        y
      ];
      _x = x[1];
      continue ;
    };
  };
  var match = parse_token_r(x, /* [] */0);
  return /* tuple */[
          implode(List.rev(match[1])),
          match[0]
        ];
}

function parse_float(x) {
  var match = parse_token(x);
  return /* tuple */[
          Caml_format.caml_float_of_string(match[0]),
          match[1]
        ];
}

function parse_bool(x) {
  var match = parse_token(x);
  return /* tuple */[
          Pervasives.bool_of_string(match[0]),
          match[1]
        ];
}

function parse_int(x) {
  var match = parse_token(x);
  return /* tuple */[
          Caml_format.caml_int_of_string(match[0]),
          match[1]
        ];
}

function parse_environment(x) {
  if (!x) {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
  var match = x[0];
  var exit = 0;
  if (!(match !== 32 && match !== 44)) {
    exit = 2;
  }
  if (exit === 2) {
    var match$1 = x[1];
    if (match$1 && match$1[0] === 93) {
      return /* tuple */[
              /* [] */0,
              match$1[1]
            ];
    }
    
  }
  var match$2 = parse_int(x);
  var match$3 = parse_res(match$2[1]);
  var match$4 = parse_environment(match$3[1]);
  return /* tuple */[
          /* :: */[
            /* tuple */[
              match$2[0],
              match$3[0]
            ],
            match$4[0]
          ],
          match$4[1]
        ];
}

function parse_res(x) {
  if (x) {
    switch (x[0]) {
      case 92 :
          var match = parse_int(x[1]);
          var match$1 = parse_exp(match[1]);
          var match$2 = parse_environment(match$1[1]);
          return /* tuple */[
                  /* Rfunc */Block.__(4, [
                      match[0],
                      match$1[0],
                      match$2[0]
                    ]),
                  match$2[1]
                ];
      case 97 :
          var match$3 = parse_res(x[1]);
          var match$4 = parse_res(match$3[1]);
          return /* tuple */[
                  /* Rapp */Block.__(5, [
                      match$3[0],
                      match$4[0]
                    ]),
                  match$4[1]
                ];
      case 98 :
          var match$5 = parse_bool(x[1]);
          return /* tuple */[
                  /* Rbool */Block.__(2, [match$5[0]]),
                  match$5[1]
                ];
      case 99 :
          var match$6 = parse_res(x[1]);
          var match$7 = parse_res(match$6[1]);
          return /* tuple */[
                  /* Rcons */Block.__(3, [
                      match$6[0],
                      match$7[0]
                    ]),
                  match$7[1]
                ];
      case 102 :
          var x$1 = x[1];
          var exit = 0;
          if (x$1 && x$1[0] === 115) {
            var match$8 = x$1[1];
            if (match$8 && match$8[0] === 116) {
              var match$9 = parse_res(match$8[1]);
              return /* tuple */[
                      /* Rfst */Block.__(8, [match$9[0]]),
                      match$9[1]
                    ];
            }
            exit = 2;
          } else {
            exit = 2;
          }
          if (exit === 2) {
            var match$10 = parse_float(x$1);
            return /* tuple */[
                    /* Rfloat */Block.__(1, [match$10[0]]),
                    match$10[1]
                  ];
          }
          break;
      case 104 :
          var match$11 = parse_int(x[1]);
          var match$12 = parse_environment(match$11[1]);
          return /* tuple */[
                  /* Rhole */Block.__(6, [
                      match$11[0],
                      match$12[0]
                    ]),
                  match$12[1]
                ];
      case 105 :
          var match$13 = parse_int(x[1]);
          return /* tuple */[
                  /* Rint */Block.__(0, [match$13[0]]),
                  match$13[1]
                ];
      case 110 :
          return /* tuple */[
                  /* Rnil */0,
                  x[1]
                ];
      case 112 :
          var match$14 = parse_res(x[1]);
          var match$15 = parse_res(match$14[1]);
          return /* tuple */[
                  /* Rpair */Block.__(7, [
                      match$14[0],
                      match$15[0]
                    ]),
                  match$15[1]
                ];
      case 115 :
          var match$16 = parse_res(x[1]);
          return /* tuple */[
                  /* Rsnd */Block.__(9, [match$16[0]]),
                  match$16[1]
                ];
      case 93 :
      case 94 :
      case 95 :
      case 96 :
      case 100 :
      case 101 :
      case 103 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 111 :
      case 113 :
      case 114 :
      case 116 :
          break;
      case 117 :
          return /* tuple */[
                  /* Runit */1,
                  x[1]
                ];
      default:
        
    }
  }
  return Pervasives.failwith("Some code generated by parser_generator.py is throwing a parse error:\nWhile parsing a/an res, I got \"" + (implode(x) + "\" which doesn't match any of the expected tags: ['fst', '\\\\', 'i', 'f', 'b', 'c', 'n', 'a', 'h', 'u', 'p', 's']"));
}

function parse_exp(x) {
  if (x) {
    switch (x[0]) {
      case 92 :
          var match = parse_int(x[1]);
          var match$1 = parse_exp(match[1]);
          return /* tuple */[
                  /* Function */Block.__(5, [
                      match[0],
                      match$1[0]
                    ]),
                  match$1[1]
                ];
      case 97 :
          var match$2 = parse_exp(x[1]);
          var match$3 = parse_exp(match$2[1]);
          return /* tuple */[
                  /* Application */Block.__(6, [
                      match$2[0],
                      match$3[0]
                    ]),
                  match$3[1]
                ];
      case 98 :
          var match$4 = parse_bool(x[1]);
          return /* tuple */[
                  /* Bool */Block.__(2, [match$4[0]]),
                  match$4[1]
                ];
      case 99 :
          var match$5 = parse_exp(x[1]);
          var match$6 = parse_exp(match$5[1]);
          return /* tuple */[
                  /* Cons */Block.__(3, [
                      match$5[0],
                      match$6[0]
                    ]),
                  match$6[1]
                ];
      case 102 :
          var x$1 = x[1];
          var exit = 0;
          if (x$1 && x$1[0] === 115) {
            var match$7 = x$1[1];
            if (match$7 && match$7[0] === 116) {
              var match$8 = parse_exp(match$7[1]);
              return /* tuple */[
                      /* Fst */Block.__(10, [match$8[0]]),
                      match$8[1]
                    ];
            }
            exit = 2;
          } else {
            exit = 2;
          }
          if (exit === 2) {
            var match$9 = parse_float(x$1);
            return /* tuple */[
                    /* Float */Block.__(1, [match$9[0]]),
                    match$9[1]
                  ];
          }
          break;
      case 104 :
          var match$10 = parse_int(x[1]);
          return /* tuple */[
                  /* Hole */Block.__(7, [match$10[0]]),
                  match$10[1]
                ];
      case 105 :
          var match$11 = parse_int(x[1]);
          return /* tuple */[
                  /* Int */Block.__(0, [match$11[0]]),
                  match$11[1]
                ];
      case 110 :
          return /* tuple */[
                  /* Nil */0,
                  x[1]
                ];
      case 112 :
          var match$12 = parse_exp(x[1]);
          var match$13 = parse_exp(match$12[1]);
          return /* tuple */[
                  /* Pair */Block.__(9, [
                      match$12[0],
                      match$13[0]
                    ]),
                  match$13[1]
                ];
      case 115 :
          var match$14 = parse_exp(x[1]);
          return /* tuple */[
                  /* Snd */Block.__(11, [match$14[0]]),
                  match$14[1]
                ];
      case 93 :
      case 94 :
      case 95 :
      case 96 :
      case 100 :
      case 101 :
      case 103 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 111 :
      case 113 :
      case 114 :
      case 116 :
          break;
      case 117 :
          return /* tuple */[
                  /* Unit */1,
                  x[1]
                ];
      case 118 :
          var x$2 = x[1];
          var exit$1 = 0;
          if (x$2 && x$2[0] === 97) {
            var match$15 = x$2[1];
            if (match$15 && match$15[0] === 114) {
              var match$16 = parse_int(match$15[1]);
              return /* tuple */[
                      /* Var */Block.__(8, [match$16[0]]),
                      match$16[1]
                    ];
            }
            exit$1 = 2;
          } else {
            exit$1 = 2;
          }
          if (exit$1 === 2) {
            var match$17 = parse_int(x$2);
            return /* tuple */[
                    /* Variable */Block.__(4, [match$17[0]]),
                    match$17[1]
                  ];
          }
          break;
      default:
        
    }
  }
  return Pervasives.failwith("Some code generated by parser_generator.py is throwing a parse error:\nWhile parsing a/an exp, I got \"" + (implode(x) + "\" which doesn't match any of the expected tags: ['var', 'fst', '\\\\', 'i', 'f', 'b', 'c', 'n', 'v', 'a', 'h', 'u', 'p', 's']"));
}

function parse_type_(x) {
  if (x) {
    switch (x[0]) {
      case 97 :
          return /* tuple */[
                  /* Any_t */3,
                  x[1]
                ];
      case 98 :
          return /* tuple */[
                  /* Bool_t */1,
                  x[1]
                ];
      case 99 :
          var match = parse_type_(x[1]);
          var match$1 = parse_type_(match[1]);
          return /* tuple */[
                  /* Cons_t */Block.__(0, [
                      match[0],
                      match$1[0]
                    ]),
                  match$1[1]
                ];
      case 102 :
          var x$1 = x[1];
          var exit = 0;
          if (x$1 && x$1[0] === 97) {
            var match$2 = x$1[1];
            if (match$2 && match$2[0] === 105) {
              var match$3 = match$2[1];
              if (match$3) {
                if (match$3[0] === 108) {
                  return /* tuple */[
                          /* Fail_t */4,
                          match$3[1]
                        ];
                }
                exit = 2;
              } else {
                exit = 2;
              }
            } else {
              exit = 2;
            }
          } else {
            exit = 2;
          }
          if (exit === 2) {
            var match$4 = parse_type_(x$1);
            var match$5 = parse_type_(match$4[1]);
            return /* tuple */[
                    /* Function_t */Block.__(1, [
                        match$4[0],
                        match$5[0]
                      ]),
                    match$5[1]
                  ];
          }
          break;
      case 105 :
          return /* tuple */[
                  /* Int_t */0,
                  x[1]
                ];
      case 112 :
          var match$6 = parse_type_(x[1]);
          var match$7 = parse_type_(match$6[1]);
          return /* tuple */[
                  /* Pair_t */Block.__(2, [
                      match$6[0],
                      match$7[0]
                    ]),
                  match$7[1]
                ];
      case 100 :
      case 101 :
      case 103 :
      case 104 :
      case 106 :
      case 107 :
      case 108 :
      case 109 :
      case 110 :
      case 111 :
      case 113 :
      case 114 :
      case 115 :
      case 116 :
          break;
      case 117 :
          return /* tuple */[
                  /* Unit_t */2,
                  x[1]
                ];
      default:
        
    }
  }
  return Pervasives.failwith("Some code generated by parser_generator.py is throwing a parse error:\nWhile parsing a/an type_, I got \"" + (implode(x) + "\" which doesn't match any of the expected tags: ['fail', 'i', 'b', 'c', 'f', 'u', 'p', 'a']"));
}

function parse_debug_construct(x) {
  if (x) {
    var match = x[0];
    if (match !== 101) {
      if (match === 114) {
        var match$1 = parse_res(x[1]);
        return /* tuple */[
                /* Res */Block.__(2, [match$1[0]]),
                match$1[1]
              ];
      }
      
    } else {
      var x$1 = x[1];
      var exit = 0;
      if (x$1 && x$1[0] === 110) {
        var match$2 = x$1[1];
        if (match$2 && match$2[0] === 118) {
          var match$3 = parse_environment(match$2[1]);
          return /* tuple */[
                  /* Environment */Block.__(1, [match$3[0]]),
                  match$3[1]
                ];
        }
        exit = 2;
      } else {
        exit = 2;
      }
      if (exit === 2) {
        var match$4 = parse_exp(x$1);
        return /* tuple */[
                /* Exp */Block.__(0, [match$4[0]]),
                match$4[1]
              ];
      }
      
    }
  }
  return Pervasives.failwith("Some code generated by parser_generator.py is throwing a parse error:\nWhile parsing a/an debug_construct, I got \"" + (implode(x) + "\" which doesn't match any of the expected tags: ['env', 'e', 'r']"));
}

exports.make_list = make_list;
exports.explode = explode;
exports.implode = implode;
exports.parse_token = parse_token;
exports.parse_exp = parse_exp;
exports.parse_res = parse_res;
exports.parse_type_ = parse_type_;
exports.parse_debug_construct = parse_debug_construct;
exports.parse_environment = parse_environment;
exports.parse_int = parse_int;
exports.parse_bool = parse_bool;
exports.parse_float = parse_float;
/* No side effect */