#ifndef _halGPIO_H_
#define _halGPIO_H_

#include  "../header/bsp.h"    		// private library - BSP layer
#include  "../header/app.h"    		// private library - APP layer

extern enum FSMstate state;   // global variable
extern enum SYSmode lpm_mode; // global variable

extern void sysConfig(void);

extern void print2LEDs(unsigned char);
extern void clrLEDs(void);
extern void toggleLEDs(char);
extern void setLEDs(char);
extern unsigned char readSWs(void);
extern inline void set_timer_interrupt(unsigned int t);
extern void set_pwm_speed(unsigned int s);

extern void delay(unsigned int);
extern void delayms(unsigned int);

extern void incLEDs(char);
extern void shiftLeftLEDs(char);
extern void shiftRightLEDs(char);

extern void enterLPM(unsigned char);
extern inline void enable_interrupts();
extern inline void disable_interrupts();

extern char readKeyPad();

extern void send_char(char ch);

#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=PORT1_VECTOR
extern __interrupt void PB_ISR(void);
#elif defined(__GNUC__)
void PB_ISR (void);
#else
#error Compiler not supported!
#endif


// USCI A0/B0 Receive ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=USCIAB0RX_VECTOR
__interrupt void USCI0RX_ISR(void);
#elif defined(__GNUC__)
void USCI0RX_ISR (void);
#else
#error Compiler not supported!
#endif

// USCI A0/B0 Transmit ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=USCIAB0TX_VECTOR
__interrupt void USCI0TX_ISR(void);
#elif defined(__GNUC__)
void USCI0TX_ISR (void);
#else
#error Compiler not supported!
#endif

//Timer0_A3 ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=TIMER0_A1_VECTOR
extern __interrupt void TIMER0_A1_ISR(void);
#elif defined(__GNUC__)
void TIMER0_A1_ISR(void);
#else
#error Compiler not supported!
#endif

//Timer1_A3 ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=TIMER1_A1_VECTOR
extern __interrupt void TIMER1_A1_ISR(void);
#elif defined(__GNUC__)
void TIMER1_A1_ISR(void);
#else
#error Compiler not supported!
#endif


// ADC10 Interrupt vector
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector = ADC10_VECTOR
__interrupt void ADC10_ISR (void);
#elif defined(__GNUC__)
void ADC10_ISR (void);
#else
#error Compiler not supported!
#endif

#endif
