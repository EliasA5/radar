#include "../header/halGPIO.h"     // private library - HAL layer
#include "../header/LCD.h"

void sysConfig(void)
{
	GPIOconfig();
	TIMERconfig();
	ADCconfig();
#ifdef _LCD_H_
	lcd_init();
#endif
	USCIconfig();
	enable_interrupts();
}

void print2LEDs(unsigned char ch)
{
	LEDsArrPort = ch;
}

void clrLEDs(void)
{
	LEDsArrPort = 0x00;
}

void toggleLEDs(char ch)
{
	LEDsArrPort ^= ch;
}

void setLEDs(char ch)
{
	LEDsArrPort |= ch;
}

// t units of 10 ms
inline void set_timer_interrupt(unsigned int t)
{
	TA0CCR0 = t << 3;
}

void set_pwm_speed(unsigned int s)
{
	TA1COMPARE0 = s;
	TA1COMPARE1 = s >> 1;
}


unsigned char readSWs(void)
{
	unsigned char ch;

	ch = SWsArrPort;
	ch &= SWmask;  // mask the least 4-bit
	ch >>= 4;
	return ch;
}

void incLEDs(char val)
{
	LEDsArrPort += val;
}

void shiftLeftLEDs(char val)
{
	LEDsArrPort = (LEDsArrPort << val) | (LEDsArrPort >> (8 - val));
}

void shiftRightLEDs(char val)
{
	LEDsArrPort = (LEDsArrPort >> val) | (LEDsArrPort << (8 - val));
}

void send_char(char ch)
{
	while((IFG2 & UCA0TXIFG) == 0);
	UCA0TXBUF = ch;
}

void delayms(unsigned int t) // t[msec]
{
	volatile unsigned int i,j;

	for(i=t; i>0; i--)
		delay(1000);
}

void delay(unsigned int t)  // t[msec]
{
	volatile unsigned int i;

	for(i=t; i>0; i--)
		__no_operation();
}

void enterLPM(unsigned char LPM_level)
{
	if (LPM_level == 0x00)
		_BIS_SR(LPM0_bits); /* Enter Low Power Mode 0 */
	else if (LPM_level == 0x01)
		_BIS_SR(LPM1_bits); /* Enter Low Power Mode 1 */
	else if (LPM_level == 0x02)
		_BIS_SR(LPM2_bits); /* Enter Low Power Mode 2 */
	else if (LPM_level == 0x03)
		_BIS_SR(LPM3_bits); /* Enter Low Power Mode 3 */
	else if (LPM_level == 0x04)
		_BIS_SR(LPM4_bits); /* Enter Low Power Mode 4 */
}

inline void enable_interrupts()
{
	_BIS_SR(GIE);
}

inline void disable_interrupts()
{
	_BIC_SR(GIE);
}

//push buttons ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=PORT1_VECTOR
__interrupt void PB_ISR(void)
#elif defined(__GNUC__)
void PB_ISR (void)
#else
#error Compiler not supported!
#endif
{

	delay(debounceVal);
	if(PBsArrIntPend & PB0){
		state = state1;
		PBsArrIntPend &= ~PB0;
	}
	else if (PBsArrIntPend & PB1){
		state = state2;
		PBsArrIntPend &= ~PB1;
	}
	else if(PBsArrIntPend & PB2){
		state = state3;
		PBsArrIntPend &= ~PB2;
	}
	// else if(PBsArrIntPend & PB3){
	//   state = state3;
	//   PBsArrIntPend &= ~PB3;
	// }

	switch (lpm_mode)
	{
		case mode0:
			LPM0_EXIT; // must be called from ISR only
			break;

		case mode1:
			LPM1_EXIT; // must be called from ISR only
			break;

		case mode2:
			LPM2_EXIT; // must be called from ISR only
			break;

		case mode3:
			LPM3_EXIT; // must be called from ISR only
			break;

		case mode4:
			LPM4_EXIT; // must be called from ISR only
			break;
	}
}

int state1_handler();
int state2_handler();
int state3_handler();
int state4_handler();
int state5_handler();
int state6_handler();
int state7_handler();

// USCI A0/B0 Receive ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=USCIAB0RX_VECTOR
__interrupt void USCI0RX_ISR(void)
#elif defined(__GNUC__)
void USCI0RX_ISR (void)
#else
#error Compiler not supported!
#endif
{
	unsigned int wakeup = 0;
	unsigned int err = 0;
	unsigned char rec = UCA0RXBUF;

	if(UCA0STAT & UCRXERR){
		err = 1;
		goto reply;
	}

	switch(rec)
	{
		case 0:
			state = idle;
			wakeup = 1;
			break;
		case 1:
			state = state1;
			wakeup = 1;
			break;
		case 2:
			state = state2;
			wakeup = 1;
			break;
		case 3:
			state = state3;
			wakeup = 1;
			break;
		case 4:
			state = state4;
			wakeup = 1;
			break;
		case 5:
			state = state5;
			wakeup = 1;
			break;
		case 6:
			state = state6;
			wakeup = 1;
			break;
		case 7:
			state = state7;
			wakeup = 1;
			break;
		default:
			err = 2;
			break;
	}

reply:
	while((IFG2 & UCA0TXIFG) == 0);
	UCA0TXBUF = err;

	if(wakeup == 0)
		return;

	disable_interrupts();
	switch(lpm_mode)
	{
		case mode0:
			LPM0_EXIT; // must be called from ISR only
			break;

		case mode1:
			LPM1_EXIT; // must be called from ISR only
			break;

		case mode2:
			LPM2_EXIT; // must be called from ISR only
			break;

		case mode3:
			LPM3_EXIT; // must be called from ISR only
			break;

		case mode4:
			LPM4_EXIT; // must be called from ISR only
			break;
	}

}

// USCI A0/B0 Transmit ISR
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector=USCIAB0TX_VECTOR
__interrupt void USCI0TX_ISR(void)
#elif defined(__GNUC__)
void USCI0TX_ISR (void)
#else
#error Compiler not supported!
#endif
{
	unsigned int wakeup = 0;
	switch(state)
	{
		case idle: break;
		case state1: break;
		case state2: break;
		case state3: break;
		case state4: break;
		case state5: break;
		case state6: break;
		case state7: break;
		default: break;
	}

	if(wakeup == 0)
		return;

	disable_interrupts();
	switch(lpm_mode)
	{
		case mode0:
			LPM0_EXIT; // must be called from ISR only
			break;

		case mode1:
			LPM1_EXIT; // must be called from ISR only
			break;

		case mode2:
			LPM2_EXIT; // must be called from ISR only
			break;

		case mode3:
			LPM3_EXIT; // must be called from ISR only
			break;

		case mode4:
			LPM4_EXIT; // must be called from ISR only
			break;
	}
}


// TA0_A0 Interrupt vector
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector = TIMER0_A1_VECTOR
__interrupt void TIMER0_A1_ISR (void)
#elif defined(__GNUC__)
void TIMER0_A1_ISR (void)
#else
#error Compiler not supported!
#endif
{
	switch(__even_in_range(TA0IV,0x0A))
	{
	  case TA0IV_NONE: break;               // Vector  0:  No interrupt
	  case TA0IV_TACCR1: break;             // Vector  2:  TACCR1 CCIFG
	  case TA0IV_TACCR2: break;             // Vector  4:  TACCR2 CCIFG
	  case TA0IV_6: break;                  // Vector  6:  Reserved CCIFG
	  case TA0IV_8: break;                  // Vector  8:  Reserved CCIFG
	  case TA0IV_TAIFG:                     // Vector 10:  TAIFG
			switch(state)
			{
				case state1:
					state1_handler();
					break;
				case state2:
					state2_handler();
					break;
				case state3:
					state3_handler();
					break;
				case state5:
				    state5_handler();
					break;
				default:
					break;
			}
	  break;
	  default: break;
	}
}


// TA1_A1 Interrupt vector
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector = TIMER1_A1_VECTOR
__interrupt void TIMER1_A1_ISR (void)
#elif defined(__GNUC__)
void TIMER1_A1_ISR (void)
#else
#error Compiler not supported!
#endif
{
    switch(__even_in_range(TA1IV,0x0A))
  {
      case  TA1IV_NONE: break;              // Vector  0:  No interrupt
      case  TA1IV_TACCR1: break;            // Vector  2:  TACCR1 CCIFG
      case TA1IV_TACCR2: break;             // Vector  4:  TACCR2 CCIFG
      case TA1IV_6: break;                  // Vector  6:  Reserved CCIFG
      case TA1IV_8: break;                  // Vector  8:  Reserved CCIFG
      case TA1IV_TAIFG: break;              // Vector 10:  TAIFG
      default: 	break;
  }
}

void ADC10_handler(void);
// ADC10 Interrupt vector
#if defined(__TI_COMPILER_VERSION__) || defined(__IAR_SYSTEMS_ICC__)
#pragma vector = ADC10_VECTOR
__interrupt void ADC10_ISR (void)
#elif defined(__GNUC__)
void ADC10_ISR (void)
#else
#error Compiler not supported!
#endif
{
	ADC10_handler();
}
