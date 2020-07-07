open Grammar
open Id

let lambda (id:id):exp =
    let id = tag Refinement id in
    Lambda(id, [], Hole id)
let application (id:id):exp = 
    Application(lambda (tag Applicator id), Hole (tag Applicand id))
