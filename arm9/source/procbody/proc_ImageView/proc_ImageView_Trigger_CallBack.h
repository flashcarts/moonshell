
#include "PlaySE.h"

static void CB_Trigger_ins_PlaySE(ETriggerType TriggerType,u32 Count,bool AddLong)
{
//  if(TriggerType!=ETT_PhoneSwitch) return; // HPSwitch�ȊO�Ȃ�A��
  
  if(AddLong==true) return; // �����O���[�h�͌��ʉ����Đ����Ȃ��ŋA��
  
  PlaySE_Sequence(Count,AddLong);
}

static inline bool CB_Trigger_ins_CheckEnabled(ETriggerType TriggerType)
{
  if(TriggerType==ETT_AButton) return(false);
  return(true);
}

static void CB_Trigger_ProcStart(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_ProcStart(...); called.\n");
#endif
}

static void CB_Trigger_ProcEnd(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_ProcEnd(...); called.\n");
#endif
}

static void CB_Trigger_Down(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_Down(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
}

static void CB_Trigger_Up(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_Up(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
}

static void CB_Trigger_LongStart(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_LongStart(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  CB_Trigger_ins_PlaySE(TriggerType,0,true);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: case ETT_RButton: {
      LeftButtonPressing=true;
      REG_POWERCNT = POWER_ALL_2D | POWER_SWAP_LCDS;
      Preview_Draw();
    } break;
    case ETT_PhoneSwitch: {
    } break;
  }
}

static void CB_Trigger_LongEnd(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_LongEnd(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  
  LeftButtonPressing=false;
  REG_POWERCNT = POWER_ALL_2D;
  Preview_Draw();
}

static void CB_Trigger_SingleClick(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_SingleClick(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
//  CB_Trigger_ins_PlaySE(TriggerType,1,false);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: case ETT_RButton: {
    } break;
    case ETT_PhoneSwitch: {
    } break;
  }
}

static void CB_Trigger_SingleLongStart(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_SingleLongStart(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  CB_Trigger_ins_PlaySE(TriggerType,1,true);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: {
    } break;
    case ETT_RButton: {
    } break;
    case ETT_PhoneSwitch: {
      RelationalToNext();
    } break;
  }
}

static void CB_Trigger_SingleLongEnd(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_SingleLongEnd(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
}

static void CB_Trigger_DoubleClick(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_DoubleClick(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  CB_Trigger_ins_PlaySE(TriggerType,2,false);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: {
      RelationalToBack();
    } break;
    case ETT_RButton: {
      RelationalToNext();
    } break;
    case ETT_PhoneSwitch: {
      RelationalToBack();
    } break;
  }
}

static void CB_Trigger_DoubleLongStart(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_DoubleLongStart(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  CB_Trigger_ins_PlaySE(TriggerType,2,true);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: {
    } break;
    case ETT_RButton: {
    } break;
    case ETT_PhoneSwitch: {
    } break;
  }
}

static void CB_Trigger_DoubleLongEnd(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_DoubleLongEnd(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
}

static void CB_Trigger_TripleClick(ETriggerType TriggerType)
{
#ifdef EnableTriggerLog
  _consolePrintf("HPSwitch_TripleClick(...); called.\n");
#endif
  
  if(CB_Trigger_ins_CheckEnabled(TriggerType)==false) return;
  CB_Trigger_ins_PlaySE(TriggerType,3,false);
  
  switch(TriggerType){
    case ETT_AButton: break;
    case ETT_LButton: {
    } break;
    case ETT_RButton: {
    } break;
    case ETT_PhoneSwitch: {
    } break;
  }
}

