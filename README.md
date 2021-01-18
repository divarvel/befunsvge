# befunsvge

Draw SVG with befunge-93


## build

stack install

## run

befunsvge-exe random-walk.b > random-walk.html

## Instructions

All befunge-93 instructions (except `&` and `~` for now).

There is a special `path` buffer used to fill the `d` attribute of a `path` tag.

- `M`, `m`, `L` and `l` pop `x`, `y` and push the corresponding path command.
- `H`, `h`, `W` and `w` pop a single value and push the corresponding path command (`W` and `w` are 
  turned into `V` and `v` commands, since `v` is already used by befunge)

- `π` flushes the current path buffer in a new `path` tag
- 'κ' pops `r`, `cx`, `cy` and pushes `circle` to the output
- 'ρ' pops `width`, `height`, `x`, `y` and pushes `rect` to the output
- 'ε' pops `rx`, `ry`, `cx`, `cy` and pushes `ellipse` to the output

## Todo

- add support for trig operations (that would mean adding support for doubles on the stack)
- add support for perlin noise (that would also mean adding support for doubles)
- add a step debugger and breakpoints
- add a web UI
