.global _start
_start:

# Set stack pointer
# ITCM size + DTCM size minus one word
li sp, (2 << (12+2)) - 4

# Jump to main
jal main

# Loop forever
j .
