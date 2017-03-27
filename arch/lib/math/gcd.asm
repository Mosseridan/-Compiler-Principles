/* gcd.asm
 * Compute GCD of two non-sob INTEGERS
 * Returns a non-sob INTEGER to R0
 *
 * Programmer: Guy Hecht, 2017
 */

GCD:
  PUSH(FP);
  MOV(FP , SP);
  INFO;
  SHOW("IN GCD " , FPARG(1));
  SHOW("IN GCD " , FPARG(2));
  SHOW("IN GCD " , FPARG(3));

  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);

  MOV(R1, FPARG(2));
  MOV(R2, FPARG(3));

GCD_LOOP:

  INFO;
  SHOW("IN GCD " , R1)
  SHOW("IN GCD " , R2)

  CMP(R2 , IMM(0));
  JUMP_EQ(GCD_EXIT);
  MOV(R3 , R2);
  MOV(R4 , R1);
  REM(R4 , R2);
  MOV(R2 , R4);
  MOV(R1 , R3);
  JUMP(GCD_LOOP);

GCD_EXIT:
  SHOW("IN GCD - OUT OF LOOP " , R1)
  MOV(R0 , R1)
  SHOW("IN GCD - RET" , R0)
  POP(R4);
  POP(R3);
  POP(R2);;
  POP(R1);
  POP(FP);
  RETURN;
