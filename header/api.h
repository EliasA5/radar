#ifndef _api_H_
#define _api_H_

extern void printSWs2LEDs(void);
extern void printArr2SWs(char Arr[], int size, unsigned int rate);
extern void printCharArr2SWs(char Arr[], int size, unsigned int rate);

extern void ADC10_handler(void);
extern char *print_int(unsigned int x);
extern char *print_qformat_2_8(unsigned int x);

extern void state1_enter();
extern int state1_handler();
extern void state1_leave();

extern void state2_enter();
extern int state2_handler();
extern void state2_leave();

extern void state3_enter();
extern int state3_handler();
extern void state3_leave();

extern void state4_enter();
extern int state4_handler();
extern void state4_leave();

extern void state5_enter();
extern int state5_handler();
extern void state5_leave();

extern void state6_enter();
extern int state6_handler();
extern void state6_leave();

extern void state7_enter();
extern int state7_handler();
extern void state7_leave();

extern void state8_enter();
extern int state8_handler();
extern void state8_leave();

#endif
