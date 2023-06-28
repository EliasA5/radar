#ifndef _app_H_
#define _app_H_

enum FSMstate{idle=0,telemeter_s=1,file_rec_s=2,
			  sonic_d=3,ldr_d=4,dual_d=5,
			  file_1=6,file_2=7,file_3=8}; // global variable
enum SYSmode{mode0,mode1,mode2,mode3,mode4}; // global variable

#define MAKEACK(opcode) (0xc0 | (opcode))
#define MSGID(msg) (((msg) & 0xc0) >> 6)
#define MSGDATA(msg) ((msg) & 0x3f)

#endif
