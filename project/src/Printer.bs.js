// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("bs-platform/lib/js/pervasives.js");

function string_of_adt(d) {
  switch (d) {
    case /* List */0 :
        return "List";
    case /* Num */1 :
        return "Num";
    case /* Bool */2 :
        return "Bool";
    
  }
}

function string_of_type_(t) {
  if (typeof t === "number") {
    switch (t) {
      case /* Int_t */0 :
          return "Int";
      case /* Bool_t */1 :
          return "Bool";
      case /* Unit_t */2 :
          return "Unit";
      case /* Any_t */3 :
          return "Any";
      case /* Fail_t */4 :
          return "Fail";
      
    }
  } else {
    switch (t.tag | 0) {
      case /* Cons_t */0 :
          return "Cons(" + (string_of_type_(t[0]) + ("," + (string_of_type_(t[1]) + ")")));
      case /* Function_t */1 :
          return "(" + (string_of_type_(t[0]) + ("->" + (string_of_type_(t[1]) + ")")));
      case /* Pair_t */2 :
          return "(" + (string_of_type_(t[0]) + (", " + (string_of_type_(t[1]) + ")")));
      case /* D */3 :
          return string_of_adt(t[0]);
      
    }
  }
}

function string_of_exp(e) {
  if (typeof e === "number") {
    if (e === /* Nil */0) {
      return "Nil";
    } else {
      return "()";
    }
  }
  switch (e.tag | 0) {
    case /* Int */0 :
        return String(e[0]);
    case /* Float */1 :
        return e[0].toString();
    case /* Bool */2 :
        return Pervasives.string_of_bool(e[0]);
    case /* Cons */3 :
        return string_of_exp(e[0]) + ("::" + string_of_exp(e[1]));
    case /* Function */4 :
        return "(" + (String(e[0]) + ("): \\" + (String(e[1]) + (": " + (string_of_type_(e[2]) + ("." + string_of_exp(e[3])))))));
    case /* Application */5 :
        return string_of_exp(e[0]) + (" " + string_of_exp(e[1]));
    case /* Hole */6 :
        return "??_" + String(e[0]);
    case /* Var */7 :
        return "var_" + String(e[0]);
    case /* Pair */8 :
        return "(" + (string_of_exp(e[0]) + (", " + (string_of_exp(e[1]) + ")")));
    case /* Fst */9 :
        return "fst(" + (string_of_exp(e[0]) + ")");
    case /* Snd */10 :
        return "snd(" + (string_of_exp(e[0]) + ")");
    case /* Ctor */11 :
        return "C" + (String(e[0]) + (": unimplemented " + string_of_exp(e[2])));
    case /* Case */12 :
        return "case " + (string_of_exp(e[0]) + (" of {" + (string_of_branches(e[1]) + "}")));
    
  }
}

function string_of_branches(b) {
  if (!b) {
    return "";
  }
  var match = b[0];
  var match$1 = match[1];
  return "C" + (String(match[0]) + (" " + (string_of_pat(match$1[0]) + (" -> " + (string_of_exp(match$1[1]) + ("; " + string_of_branches(b[1])))))));
}

function string_of_pat(p) {
  if (p.tag) {
    return "(" + (string_of_pat(p[0]) + (", " + (string_of_pat(p[1]) + ")")));
  } else {
    return String(p[0]);
  }
}

function string_of_unevalcons(c) {
  return "([" + (string_of_unfilled_holes(c[0]) + ("], [" + (string_of_hole_fillings(c[1]) + "])")));
}

function string_of_value(v) {
  if (typeof v === "number") {
    return "()";
  }
  switch (v.tag | 0) {
    case /* Vint */0 :
        return String(v[0]);
    case /* Vbool */1 :
        return Pervasives.string_of_bool(v[0]);
    case /* Vpair */2 :
        return "(" + (string_of_value(v[0]) + (", " + (string_of_value(v[1]) + ")")));
    case /* Vctor */3 :
        return "C" + (String(v[0]) + (": " + (string_of_adt(v[1]) + (" " + string_of_value(v[2])))));
    
  }
}

function string_of_example(ex) {
  if (typeof ex === "number") {
    if (ex === /* Top */0) {
      return "T";
    } else {
      return "()";
    }
  }
  switch (ex.tag | 0) {
    case /* Eint */0 :
        return String(ex[0]);
    case /* Ebool */1 :
        return Pervasives.string_of_bool(ex[0]);
    case /* Epair */2 :
        return "(" + (string_of_example(ex[0]) + (", " + (string_of_example(ex[1]) + ")")));
    case /* Efunc */3 :
        return "{" + (string_of_value(ex[0]) + (" -> " + (string_of_example(ex[1]) + "}")));
    case /* Ector */4 :
        return "C" + (String(ex[0]) + (" " + string_of_example(ex[2])));
    
  }
}

function string_of_unfilled_holes(c) {
  if (!c) {
    return "-";
  }
  var match = c[0];
  return String(match[0]) + ("->[" + (string_of_excons(match[1]) + ("]; " + string_of_unfilled_holes(c[1]))));
}

function string_of_excons(c) {
  if (!c) {
    return "-";
  }
  var match = c[0];
  return "([" + (string_of_env(match[0]) + ("], " + (string_of_example(match[1]) + ("); " + string_of_excons(c[1])))));
}

function string_of_res(r) {
  if (typeof r === "number") {
    if (r === /* Rnil */0) {
      return "Nil";
    } else {
      return "()";
    }
  }
  switch (r.tag | 0) {
    case /* Rint */0 :
        return String(r[0]);
    case /* Rfloat */1 :
        return r[0].toString();
    case /* Rbool */2 :
        return Pervasives.string_of_bool(r[0]);
    case /* Rcons */3 :
        return string_of_res(r[0]) + ("::" + string_of_res(r[1]));
    case /* Rfunc */4 :
        return "[" + (string_of_env(r[4]) + ("](" + (String(r[0]) + ("): \\" + (String(r[1]) + (": " + (string_of_type_(r[2]) + ("." + string_of_exp(r[3])))))))));
    case /* Rapp */5 :
        return string_of_res(r[0]) + (" " + string_of_res(r[1]));
    case /* Rhole */6 :
        return "[" + (string_of_env(r[1]) + ("]??_" + String(r[0])));
    case /* Rpair */7 :
        return "(" + (string_of_res(r[0]) + (", " + (string_of_res(r[1]) + ")")));
    case /* Rfst */8 :
        return "fst(" + (string_of_res(r[0]) + ")");
    case /* Rsnd */9 :
        return "snd(" + (string_of_res(r[0]) + ")");
    case /* Rctor */10 :
        return "C" + (String(r[0]) + (" " + string_of_res(r[2])));
    case /* Rictor */11 :
        return "Ci" + (String(r[0]) + (" " + string_of_res(r[2])));
    case /* Rcase */12 :
        "[" + (string_of_env(r[2]) + "] case ");
        return string_of_res(r[0]) + (" of {" + (string_of_branches(r[1]) + "}"));
    
  }
}

function string_of_env(e) {
  if (!e) {
    return "-";
  }
  var match = e[0];
  return String(match[0]) + ("->" + (string_of_res(match[1]) + ("; " + string_of_env(e[1]))));
}

function string_of_context(e) {
  if (!e) {
    return "-";
  }
  var match = e[0];
  return String(match[0]) + ("->" + (string_of_type_(match[1][0]) + ("; " + string_of_context(e[1]))));
}

function string_of_hole_fillings(c) {
  if (!c) {
    return "-";
  }
  var match = c[0];
  return String(match[0]) + ("->" + (string_of_exp(match[1]) + ("; " + string_of_hole_fillings(c[1]))));
}

function string_of_guess_output(c) {
  if (c) {
    return string_of_exp(c[0]) + ("; " + string_of_guess_output(c[1]));
  } else {
    return "-";
  }
}

function string_of_hole_context(c) {
  if (!c) {
    return "-";
  }
  var match = c[0];
  var match$1 = match[1];
  return string_of_context(match$1[0]) + (": " + (String(match[0]) + ("->" + (string_of_type_(match$1[1]) + ("; " + string_of_hole_context(c[1]))))));
}

function string_of_one_constraint_(c) {
  if (!c) {
    return "-";
  }
  var match = c[0];
  var match$1 = match[1];
  return string_of_env(match[0]) + (": " + (String(match$1[0]) + ("->" + (string_of_example(match$1[1]) + ("; " + string_of_one_constraint_(c[1]))))));
}

function string_of_identifier(prim) {
  return String(prim);
}

function string_of_hole_identifier(prim) {
  return String(prim);
}

function string_of_filler_output(c) {
  return "(" + (string_of_unevalcons(c[0]) + (", " + (string_of_hole_context(c[1]) + ")")));
}

function string_of_constraint_(c) {
  if (c !== undefined) {
    return string_of_unevalcons(c);
  } else {
    return "None";
  }
}

function string_of_solver_output(c) {
  return "(" + (string_of_hole_fillings(c[0]) + (", " + (string_of_hole_context(c[1]) + ")")));
}

function string_of_debug_construct(c) {
  switch (c.tag | 0) {
    case /* Exp */0 :
        return string_of_exp(c[0]);
    case /* Environment */1 :
        return string_of_env(c[0]);
    case /* Res */2 :
        return string_of_res(c[0]);
    case /* Type_ */3 :
        return string_of_type_(c[0]);
    case /* Example */4 :
        return string_of_example(c[0]);
    case /* Constraint_ */5 :
        return string_of_constraint_(c[0]);
    case /* Context */6 :
        return string_of_context(c[0]);
    case /* Hole_Context */7 :
        return string_of_hole_context(c[0]);
    case /* Guess_Output */9 :
        return string_of_guess_output(c[0]);
    case /* Solver_Output */10 :
        return string_of_solver_output(c[0]);
    case /* Filler_Output */11 :
        return string_of_filler_output(c[0]);
    case /* Hole_Fillings */12 :
        return string_of_hole_fillings(c[0]);
    case /* Unfilled_Holes */13 :
        return string_of_unfilled_holes(c[0]);
    case /* DB_Int */8 :
    case /* Hole_Identifier */14 :
        return String(c[0]);
    case /* Excons */15 :
        return string_of_excons(c[0]);
    case /* Unevalcons */16 :
        return string_of_unevalcons(c[0]);
    case /* Branches */17 :
        return string_of_branches(c[0]);
    
  }
}

exports.string_of_debug_construct = string_of_debug_construct;
exports.string_of_exp = string_of_exp;
exports.string_of_res = string_of_res;
exports.string_of_branches = string_of_branches;
exports.string_of_pat = string_of_pat;
exports.string_of_env = string_of_env;
exports.string_of_identifier = string_of_identifier;
exports.string_of_hole_identifier = string_of_hole_identifier;
exports.string_of_type_ = string_of_type_;
exports.string_of_adt = string_of_adt;
exports.string_of_example = string_of_example;
exports.string_of_value = string_of_value;
exports.string_of_one_constraint_ = string_of_one_constraint_;
exports.string_of_constraint_ = string_of_constraint_;
exports.string_of_context = string_of_context;
exports.string_of_hole_context = string_of_hole_context;
exports.string_of_guess_output = string_of_guess_output;
exports.string_of_solver_output = string_of_solver_output;
exports.string_of_filler_output = string_of_filler_output;
exports.string_of_hole_fillings = string_of_hole_fillings;
exports.string_of_unfilled_holes = string_of_unfilled_holes;
exports.string_of_excons = string_of_excons;
exports.string_of_unevalcons = string_of_unevalcons;
/* No side effect */
