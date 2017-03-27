/* scheme/make_sob_fract.asm
 * Takes two numbers, a numinator and a denuminator as arguments, and places the corresponding Scheme object in R0
 *
 * Programmer: Mayer Goldberg, 2010
 */

  MAKE_SOB_FRACT:
   PUSH(FP);
   MOV(FP, SP);
   PUSH(IMM(3));
   CALL(MALLOC);
   DROP(1);
   MOV(IND(R0), T_FRACT);
   MOV(INDD(R0, 1), FPARG(0));
   MOV(INDD(R0, 2), FPARG(1));
   POP(FP);
   RETURN;
