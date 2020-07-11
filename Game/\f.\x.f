\f.\x.f f f f x
\f.\x.f f f f f x

incr \a\f\x.fafx
add \a.\b.\f.\x.afbfx

\2\4\3


Lambda(_1, [], Lambda(_3, [], Lambda(_2, [], Application(Application(Application(Application(Application(Variable(_9), Hole(_8)), Hole(_7)), Hole(_6)), Hole(_5)), Variable(_3)))))

\132 

?incr \f.\x.f f f f x
\f.\x.f f f f f x
?incr

\a\f\x.fafx
ru_incr = a
rru_incr = f
rrru_incr = x
2 1 2 3
? 2 2 2 2 3
? f f f f 3
Lambda(ru_incr, [], Lambda(rru_incr, [], Lambda(rrru_incr, [], Application(Application(Application(Application(Application(Variable(_5), Variable(rru_incr)), Variable(rru_incr)), Variable(rru_incr)), Variable(rru_incr)), Variable(rrru_incr)))))


?incr \f.\x.f (f x)
\f.\x.f (f (f x))
?incr

yields:
\ru_incr.\rru_incr.\rrru_incr.rru_incr

and yet
(\ru_incr.\rru_incr.\rrru_incr.rru_incr) \f.\x.f (f x)
\f.\x.f (f (f x))

is unsatisfiable.




let f = ? in {f 3 = F 3 3 3} f

let f = ? in {3 = 3} f

let f = ? in {3 = f} f











