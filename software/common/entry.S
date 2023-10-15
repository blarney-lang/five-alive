.global _start
_start:

# Set stack pointer
# ITCM size + DTCM size minus one word
li sp, (2 << (12+2)) - 4

# Jump to main
jal main

# Send null char to terminate UART stream
wait_uart:
  csrr t0, 0x801
  beq t0, zero, wait_uart
csrw 0x801, 0

# Loop forever
j .
