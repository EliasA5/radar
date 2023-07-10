#ifndef _app_H_
#define _app_H_

typedef unsigned char uchar;

enum FSMstate{idle=0,telemeter_s=1,file_rec_s=2,
			  sonic_d=3,ldr_d=4,dual_d=5,
			  do_file=6}; // global variable
enum FILEstate{no_file=0, file_0=6, file_1=7, file_2=8};
enum SYSmode{mode0,mode1,mode2,mode3,mode4}; // global variable

#define FILE0PTR 0x0 // segment B
#define FILE1PTR 0x0 // segment C
#define FILE2PTR 0x0 // segment D

struct file_manager{
	uchar curr_file;
	uchar *file_curr[3];
	uchar valid[3];
	uchar file_to_replace;
};

#define MAKEACK(opcode) (0xc0 | (opcode))
#define MSGID(msg) (((msg) & 0xc0) >> 6)
#define MSGDATA(msg) ((msg) & 0x3f)
#define MAKEULTRASONIC(degree) (0x40 | (degree))

#endif
