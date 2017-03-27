/* scheme/make_sob_symbol.asm
 * takes a pointer to a representative string and returns the corresponding symbol object in R0
 * Programmer: Guy Hecht, 2017
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(2);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0 , 0), T_SYMBOL);
  MOV(INDD(R0 , 1), FPARG(0));
  POP(FP);
  RETURN;
