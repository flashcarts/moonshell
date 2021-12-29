
#define KeyRepeat_DelayCount (40)
#define KeyRepeat_RateCount (10)

static u32 KeyRepeatLastKey;
static bool KeyRepeatFlag;
static u32 KeyRepeatCount;

static void KeyRepeat_Flash(void)
{
  KeyRepeatLastKey=0;
  KeyRepeatFlag=false;
}

static u32 KeyRepeat_On(u32 NowKey)
{
  if(NowKey!=KeyRepeatLastKey) KeyRepeatFlag=false;
  KeyRepeatLastKey=NowKey;
  
  if(KeyRepeatFlag==false){ cwl();
    KeyRepeatFlag=true;
    KeyRepeatCount=KeyRepeat_DelayCount;
    }else{ cwl();
    if(KeyRepeatCount==0){ cwl();
      KeyRepeatCount=KeyRepeat_RateCount;
      }else{ cwl();
      NowKey=0;
    }
  }
  
  return(NowKey);
}

static u32 KeyRepeat_Proc(u32 NowKey,u32 VsyncCount)
{
  if(KeyRepeatFlag==true){ cwl();
    if(KeyRepeatCount<=VsyncCount){ cwl();
      KeyRepeatCount=0;
      }else{ cwl();
      KeyRepeatCount-=VsyncCount;
    }
  }
  
  if(NowKey==0){ cwl();
    KeyRepeat_Flash();
    }else{ cwl();
    NowKey=KeyRepeat_On(NowKey);
  }
  
  return(NowKey);
}

static void KeyRepeat_Delay(u32 Multiple)
{
  KeyRepeatCount=KeyRepeat_DelayCount*Multiple;
}

