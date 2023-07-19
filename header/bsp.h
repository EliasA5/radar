#ifndef _bsp_H_
#define _bsp_H_

#ifdef __MSP430FG4619__
#include <msp430fg4619.h>  // MSP430x4xx
#define __LABKIT__
#elif defined(__MSP430G2553__)
#include  <msp430g2553.h>  // MSP430x2xx
#define __HOMEKIT__
#else
#error device not supported
#endif

#define   debounceVal      250
#define   LEDs_SHOW_RATE   0xFFFF  // 62_5ms

// LEDs abstraction
#define LEDsArrPort        P1OUT
#define LEDsArrPortDir     P1DIR
#define LEDsArrPortSel     P1SEL

// Switches abstraction
#define SWsArrPort         P2IN
#define SWsArrPortDir      P2DIR
#define SWsArrPortSel      P2SEL
#define SWmask             0x70

// PushButtons abstraction
#define PBsArrPort	       P1IN
#define PBsArrIntPend	   P1IFG
#define PBsArrIntEn	       P1IE
#define PBsArrIntEdgeSel   P1IES
#define PBsArrPortSel      P1SEL
#define PBsArrPortDir      P1DIR
#define PB0                0x01
#define PB1                0x02
#define PB2                0x04
#define PB3                0x08

#define RGB_OUT            P2OUT
#define RGB_DIR            P2DIR
#define RGB_SEL            P2SEL

//Timer A0 abstraction
#define Timer0Ctl          TA0CTL
#define Timer0CapCtl       TA0CCTL2
#define Timer0CmpCtl       TA0CCTL1
#define TA0COMPARE0        TA0CCR0
#define TA0COMPARE1        TA0CCR1

//Timer A1 abstraction
#define Timer1Ctl          TA1CTL

#define Timer1Cmp_Servo    TA1CCTL1
#define Timer1Cap_Ultra    TA1CCTL2
#define Timer1Cmp_Ultra    TA1CCTL0
#define TA1COMPARE0        TA1CCR0
#define TA1COMPARE1        TA1CCR1

// ADC10 abstraction
#define ADCLDRCtl0        ADC10CTL0
#define ADCLDRCtl1        ADC10CTL1

// Ultrasonic sensor
#define UltrasonicPort     P2OUT
#define UltrasonicSel      P2SEL
#define UltrasonicDir      P2DIR
#define UltrasonicPinTrig  0x01;
#define UltrasonicPinEcho  0x10;

// Servo
#define ServoPort          P2OUT
#define ServoSel           P2SEL
#define ServoDir           P2DIR
#define ServoPin           0x04;


extern void GPIOconfig(void);
extern void TIMERconfig(void);
extern void ADCconfig(void);
extern void DMAconfig(void);
extern void USCIconfig(void);

#endif



