3099*:Mv                               initialize the last walked direction at 3
v      <   > v                         init the iteration count at 0 and make the 
           1                           path start at 81,81
>:"z"`#v_\>?0>+4%:!#v_1-:!#v_1-:!#v_v  stop the loop when the iteration counter reaches ord('z')= 122
       π   3        0      0      5 0  flush the path and quit
       @   > ^      5      5      0 0  then swap the stack to put the last direction front then choose
                    -               5  between 3 possible rotations (0,1,3)*π/2 to avoid going back
                    0               -  take the direction count mod 4 and switch on it. Each branch
                    l      l      l l  checks if it's 0, decrements it if not.
                    $      $      $ $  Each branch then pushes the corresponding move to the path
                    0      1      2 3  buffer and then pushes the direction code again (because of
                    >      >      > v  the decrement, the original value is lost).
^                  ," "          +1\<  When all this is done, swap the stack again and increment
                                       the iteration count.
