### Make Emacs use spaces instead of  tabs:
###     M-x set-variable<RET>
###     indent-tabs-mode<RET> nil

### NOTE: setting this in my .spacemacs file doesn't work but doing it manually
### does, why?

       .data

foo:   .word 88    # REPLACE 88 WITH 666

boo:   .space 128


       .text

main:
       # Execution starts here.

       li $t1, 77

       # INSERT li v0 0xa7

# START SKIP
        add $t0, $t0, 1
        move $t1, t0
# END SKIP

        li $t3, 1               # REPLACE # ADD CODE HERE

        add $t4, $t4, 1         # REPLACE $t4 WITH $t5
