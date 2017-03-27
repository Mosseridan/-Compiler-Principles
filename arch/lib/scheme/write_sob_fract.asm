/* scheme/write_sob_fract.asm
 * Take a pointer to a Scheme fract object, and
 * prints (to stdout) the character representation
 * of that object.
 *
 * Programmer: Guy Hecht, 2017
 */

 WRITE_SOB_FRACT:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);


  MOV(R1, FPARG(0));
  MOV(R0, INDD(R1 , 1));

  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);

  PUSH('/');
  CALL(PUTCHAR);
  DROP(1);

  MOV(R0 , INDD(R1 , 2));
  PUSH(R0);
  CALL(WRITE_INTEGER);
  DROP(1);

  POP(R1);
  POP(FP);
  RETURN;
