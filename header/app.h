#ifndef _app_H_
#define _app_H_

typedef unsigned char uchar;

enum FSMstate{idle=0,telemeter_s=1,file_rec_s=2,
			  sonic_d=3,ldr_d=4,dual_d=5,
			  do_file=6}; // global variable
enum FILEstate{no_file=0, file_0=6, file_1=7, file_2=8};
enum SYSmode{mode0,mode1,mode2,mode3,mode4}; // global variable

#define SEGMENT_A (0x01000 + (3*0x40u))
#define SEGMENT_B (0x01000 + (2*0x40u))
#define SEGMENT_C (0x01000 + (1*0x40u))
#define SEGMENT_D (0x01000 + (0*0x40u))

#define FLASH_START (0x01000)
#define FLASH_END 	(0x010ff)

extern const uchar *const segments[3];

#define FILE0PTR (segments[0])
#define FILE1PTR (segments[1])
#define FILE2PTR (segments[2])

struct file_manager{
	uchar curr_file;
	uchar *file[3];
	uchar valid[3];
	uchar file_to_replace;
	uchar first_enter;
	uchar d;
};

#define MAKEACK(opcode) (0xc0 | (opcode))
#define MSGID(msg) (((msg) & 0xc0) >> 6)
#define MSGDATA(msg) ((msg) & 0x3f)
#define MAKEULTRASONIC(degree) (0x40 | ((degree) & 0x3f))
#define MAKELDR(degree) (0x80 | ((degree) & 0x3f))

#endif
