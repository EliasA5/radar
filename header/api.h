#ifndef _api_H_
#define _api_H_

extern void printSWs2LEDs(void);
extern void printArr2SWs(char Arr[], int size, unsigned int rate);
extern void printCharArr2SWs(char Arr[], int size, unsigned int rate);

extern void ADC10_handler(void);
extern char *print_int(unsigned int x);
extern char *print_qformat_2_8(unsigned int x);

extern void telemeter_s_enter();
extern int telemeter_s_handler();
extern void telemeter_s_leave();

extern void file_rec_s_enter();
extern int file_rec_s_handler(unsigned char next);
extern void file_rec_s_leave();

extern void sonic_d_enter();
extern int sonic_d_handler();
extern void sonic_d_leave();

extern void ldr_d_enter();
extern int ldr_d_handler();
extern void ldr_d_leave();

extern void dual_d_enter();
extern int dual_d_handler();
extern void dual_d_leave();

extern void file_enter();
extern int file_handler();
extern void file_leave();

#endif
