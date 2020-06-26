open Parser;

let help = "
Welcome to the REPL in REPL debug expirence!

You are currently in a Reason repl, where you can run
any Reason command. One available reason command is 

    r()

Which enters a sub-repl in Candlenut. This sub-repl has three commands:
    q
        Any command starting with q will exit the
        sub-repl and re-enter the reason repl.
    #env exp eval
        A command starting with # indicates a reconfiguration of the
        repl's behavior. Specify a list of commands in reverse-polish
        Notation. Available commands can be found in repl_source.dat.
    2f3.14 ppi3i4i5
        Anything else is interpreted via the specified commands. The 
        input format is polish notation and whitespace insensitive.

If you quit the repl, you will still have access to the
entire history of computation in Repl.history, which is
mutable, so you'll likely need to tyoe Repl.history^.

DISCLAIMER: everything I just said about a Reason repl
Isn't true yet, you are stuck in the Candlenut repl.";

let h() = 
    Js.log(help)

Js.log("r() enters repl, ? for help in repl, h() for help outside of repl.")
let command = ref("env exp eval");
let r() =
    Readline.readline((inp) => {
        //Js.log(inp);
        //let (construct, _) = parse_debug_construct(explode(inp));
        //Js.log(string_of_debug_construct(construct));
        switch(String.get(inp, 0)) {
        | 'q' => Readline.close()
        | '?' => h()
        | '#' => command := implode(List.tl(explode(inp))); Js.log("Command = \""++command^ ++"\"")
        | _ => Repl.main(inp, command^)
        }
    });
r()

/*

#mod_use "Tools.re";







*/