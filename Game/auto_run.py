from time import time, sleep
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from os import system

directory_to_watch = None#input('What directory should I watch? (default=\'.\')')
if not directory_to_watch:
    directory_to_watch = '.'

script = './write-compile-run'
just_run = 'ocaml {}'
compile_and_run = '(ocamlc {} && ./a.out)'
compile_backtrace_and_run = '(ocamlc -g {} && ./a.out)'
optimized_compile_and_run = '(ocamlopt {} && ./a.out)'
command_format = script#compile_and_run
depends = ['lang.ml', 'parse.ml', 'iseq.ml', 'enumerator.ml', 'union_find.ml']
last_file = ''

width = 65

extention = '.ml'
exclusions = ['Up.ml', 'Up_.ml', 'Parser.ml', 'Lexer.ml']
inclusions = ['Up.mlp', 'Parser.mly', 'Lexer.mll']

def interpret():
    system('ocaml')

class Handler(FileSystemEventHandler):
    def on_any_event(self, event):
        path = event.src_path
        if (path[-len(extention):] == extention and path.split('/')[-1] not in exclusions) or path.split('/')[-1] in inclusions:
            global last_file
            last_file = path
            cmd = command_format.format(' '.join(depends+[path]))
            print('\n'+'{{:-<{}}}'.format(width).format('{}  '.format(cmd)))
            t0 = time()
            system(cmd)
            t1 = time()
            print('{{:-<{}}}'.format(width).format('Time elapsed = {:.2f} seconds  '.format(t1-t0))+'\n')


observer = Observer()
event_handler = Handler()
observer.schedule(event_handler, directory_to_watch, recursive=True)
observer.start()

print('Started.')
manual_configure = False

try:
    depends_add = ['depends add','da ']
    depends_remove = ['depends remove','dr ']
    interpret_ = ['interpret()', 'i']
    trace = ['trace', 'command_format = compile_backtrace_and_run', 'command_format=compile_backtrace_and_run']
    manual = ['manual','m']
    input_disabled = False
    while True:
        if input_disabled:
            sleep(1)
            continue
        inp = input()
        try:
            if inp in manual:
                manual_configure = True
                print('Live manual configuration active!')
                continue

            acted = False
            for da in depends_add:
                if inp[:len(da)] == da:
                    depends.append(inp[len(da):].strip())
                    if command_format == just_run:
                        command_format = compile_and_run
                    acted = True
            for dr in depends_remove:
                if inp[:len(dr)] == dr:
                    depends.remove(inp[len(dr):].strip())
                    acted = True
            if acted:
                continue

            if inp in interpret_:
                if last_file:
                    for i in depends + [last_file]:
                        print('#load "{}.cmo";;'.format('.'.join(i.split('.')[:-1])))
                    name = last_file.split('.')[-2].split('/')[-1]
                    print('open {}{};;'.format(name[0].upper(),name[1:]))
                interpret()
                print('Interpretation Finished.')
                continue

            if inp in trace:
                command_format = compile_backtrace_and_run
                print('Backtrace enabled')
                print('If you havn\'t already, quit, run this, and try again: "export OCAMLRUNPARAM=b"')
                continue

            if inp[:1] == "\\":
                input_disabled = True
                print("auto_run.py no longer accepts input")
                continue

            if manual_configure:
                try:
                    print(repr(eval(inp)))
                except:
                    exec(inp)
                continue

            if inp:
                print('Unknown option.\nYou can disable input capture with \\ or try\n"{} ...", "{} ...", or "{}".'.format(depends_add[0], depends_remove[0], manual[0]))            
                continue

        except Exception as e:
                print(e)

except:
    observer.stop()
    observer.join()
    print("Error")
    raise
