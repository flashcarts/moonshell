
static void ARM7_SelfCheck_Check(void)
{
  EARM7SelfCheck E7SC=IPC6->ARM7SelfCheck;
  if(E7SC==E7SC_OK) return;
  
  switch(E7SC){
//    case E7SC_OK: break;
    case E7SC_StackOverflow_SVC: _consolePrint("Fatal error: ARM7/SVC stack overflow signal received.\n"); break;
    case E7SC_StackOverflow_IRQ: _consolePrint("Fatal error: ARM7/IRQ stack overflow signal received.\n"); break;
    case E7SC_StackOverflow_SYS: _consolePrint("Fatal error: ARM7/SYS stack overflow signal received.\n"); break;
    default: _consolePrintf("Fatal error: ARM7/Unknown(0x%x) stack overflow signal received.\n",E7SC); break;
  }
  
  REG_IME=0;
  ShowLogHalt();
  while(1);
}

