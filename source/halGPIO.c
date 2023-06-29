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
	// we enable interrupts on idle state enter.
	// enable_interrupts();

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

void set_radar_deg(int degree)
{
	// TODO set degree value
}

void delayms(unsigned int t)
{
	volatile unsigned int i,j;

	for(i=t; i>0; i--)
		delay(1000);
}

void delay(unsigned int t)
{
	volatile unsigned int i;

	for(i=t; i>0; i--)
		__no_operation();
}


void write_flash(char *buf, char sz)
{
	// TODO implement this
}

void enterLPM(unsigned char LPM_level)
{

	switch (LPM_level){
		case 0x00:
			_BIS_SR(LPM0_bits);
			break;
		case 0x01:
			_BIS_SR(LPM1_bits);
			break;
		case 0x02:
			_BIS_SR(LPM2_bits);
			break;
		case 0x03:
			_BIS_SR(LPM3_bits);
			break;
		case 0x04:
			_BIS_SR(LPM4_bits);
			break;
		default: break;
	}
}

void enable_interrupts()
{
	_BIS_SR(GIE);
}

inline void disable_interrupts()
{
	_BIC_SR(GIE);
}

int telemeter_s_handler();
int file_rec_s_handler(unsigned char next);
int sonic_d_handler();
int ldr_d_handler();
int dual_d_handler();
int file_1_handler();
int file_2_handler();
int file_3_handler();

extern unsigned char telem_deg;
extern unsigned char file_size;
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

	// for now we assume no errors
	// if(UCA0STAT & UCRXERR){
	// 	add_ack_tx_queue(MAKEACK(61));
	// 	return;
	// }

	if(state == file_rec_s){
		if(file_rec_s_handler(rec) == 0){
			state = idle;
			wakeup = 1;
		}
		goto wakeup;
	}

	switch(MSGID(rec))
	{
		case 0:
			switch (MSGDATA(rec)) {
				case idle:
					state = idle;
					wakeup = 1;
					break;
				case sonic_d:
					state = sonic_d;
					wakeup = 1;
					break;
				case ldr_d:
					state = ldr_d;
					wakeup = 1;
					break;
				case dual_d:
					state = dual_d;
					wakeup = 1;
					break;
				case file_1:
					state = file_1;
					wakeup = 1;
					break;
				case file_2:
					state = file_2;
					wakeup = 1;
					break;
				case file_3:
					state = file_3;
					wakeup = 1;
					break;
				default:
					break;
			}
			break;
		case 1:
			// enter telemeter state
			telem_deg = MSGDATA(rec);
			telem_deg = (telem_deg << 1) + telem_deg;
			state = telemeter_s;
			wakeup = 1;
			break;
		case 2:
			// send ack at the end of rec_file_s enter
			file_size = MSGDATA(rec);
			state = file_rec_s;
			wakeup = 1;
			break;
		default:
			break;
	}

// maybe put reply at end of state enter
// reply:
// 	// TODO add ack maker function
// 	while((IFG2 & UCA0TXIFG) == 0);
// 	UCA0TXBUF = err;

wakeup:
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

unsigned char tx_buf[32];
unsigned char tx_head = 0;
unsigned char tx_tail = 0;
unsigned char tx_left = 32;

void add_ack_tx_queue(unsigned char opcode)
{

	if(tx_left == 0){
		// busy wait until we can send 1 byte
		while((IFG2 & UCA0TXIFG) == 0);
		USCI0TX_ISR();
	}

	add_msg_tx_queue(&opcode, 1);
}

void add_msg_tx_queue(unsigned char *buf, unsigned char len)
{
	unsigned char i;
	if(len > tx_left)
		return;
	for(i = 0; i < len; i++, tx_head = (tx_head + 1) & 0x1f)
		tx_buf[tx_head] = buf[i];

	tx_left -= len;
	IE2 |= UCA0TXIE;
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
	UCA0TXBUF = tx_buf[tx_tail];
	tx_tail = (tx_tail + 1) & 0x1f;

	if((++tx_left) == 32)
		IE2 &= ~UCA0TXIE;

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
				case idle: break;
				case telemeter_s: break;
				case file_rec_s: break;
				case sonic_d: break;
				case ldr_d: break;
				case dual_d: break;
				case file_1: break;
				case file_2: break;
				case file_3: break;
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
		case TA1IV_NONE: break;               // Vector  0:  No interrupt
		case TA1IV_TACCR1: break;             // Vector  2:  TACCR1 CCIFG
		case TA1IV_TACCR2: break;             // Vector  4:  TACCR2 CCIFG
		case TA1IV_6: break;                  // Vector  6:  Reserved CCIFG
		case TA1IV_8: break;                  // Vector  8:  Reserved CCIFG
		case TA1IV_TAIFG: break;              // Vector 10:  TAIFG
		default: break;
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

