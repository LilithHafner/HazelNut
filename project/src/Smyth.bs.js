// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

function type_intersection(t1, t2) {
  if (typeof t1 === "number" && t1 === 2) {
    return t2;
  }
  if (typeof t2 === "number") {
    if (t2 !== 2 && !Caml_obj.caml_equal(t1, t2)) {
      return /* Fail_t */3;
    } else {
      return t1;
    }
  } else if (Caml_obj.caml_equal(t1, t2)) {
    return t1;
  } else {
    return /* Fail_t */3;
  }
}

function example_refined_type_intersection(ert1, ert2) {
  return /* tuple */[
          type_intersection(ert1[0], ert2[0]),
          List.append(ert1[1], ert2[1])
        ];
}

function bidirectional_typecheck(sketch) {
  return sketch;
}

function add(x, y) {
  return x + y | 0;
}

Pervasives.print_int(18);

Pervasives.print_string("\n");

var merge_examples = List.append;

exports.type_intersection = type_intersection;
exports.merge_examples = merge_examples;
exports.example_refined_type_intersection = example_refined_type_intersection;
exports.bidirectional_typecheck = bidirectional_typecheck;
exports.add = add;
/*  Not a pure module */
