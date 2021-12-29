/*
	ROMEO2 �w�b�h�z���R�l�N�^ �h���C�o
*/
#include <nds.h>
#include "romeo2.h"
#include "romeo2_hp.h"

/****************************************************************************
	�w�b�h�z���[�q�ɐڑ�����Ă���P�[�u���̎�ނ�Ԃ�
****************************************************************************/
int hp_sense_connect(void)
{
	u16 port_val = HP_DETECT;

	if((port_val & HP_DETECT_HDSET)==0)
	{
		// AUDIO CABLE
		return (port_val & HP_DETECT_STDET) ? HP_MONO : HP_STEREO;
	}

	// VIDEO OUT CABLE (not suppported)
	if((port_val & HP_DETECT_STDET)==0) return HP_VIDEO;

	// no-connect
	return HP_NO_CONNECT;
}

/****************************************************************************
	�w�b�h�z���[�q�́A�X�C�b�`�̏�Ԃ�Ԃ�
****************************************************************************/
int hp_sense_switch(void)
{
	return (HP_DETECT & HP_DETECT_HSSW) ? HP_SWITCH_OFF : HP_SWITCH_ON;
}
