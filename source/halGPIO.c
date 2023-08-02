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

// deg in [0,60]
static unsigned char current_radar_deg = 0;
static unsigned char maximum_degree = 60;
void set_radar_deg(unsigned char degree)
{
	current_radar_deg = degree;
	degree = (degree << 1) + degree;
	TA1COMPARE0 = 26250;
	TA1COMPARE1 = (25620 - ((degree << 3) + (degree << 1) + degree));
}

void set_max_radar_deg(unsigned char degree)
{
	maximum_degree = degree;
}

unsigned char get_radar_deg(void)
{
	return current_radar_deg;
}

unsigned char update_degree(void)
{
	unsigned char new_deg = current_radar_deg + 1;	
	if(new_deg >= maximum_degree)
		return 1;
	set_radar_deg(new_deg);
	return 0;
}

extern int adc10_samples[16];
void activate_ldr(void)
{
	ADC10DTC1 = 16; // number of transfers
	ADC10SA  =    adc10_samples;
	ADCLDRCtl0 |= (ENC + ADC10ON + ADC10IE);
	ADCLDRCtl0 &= ~ADC10IFG;
	// ADCLDRCtl0 |= ADC10SC;
}

void deactivate_ldr(void)
{
	ADC10DTC1 = 0;
	ADC10SA = 0;
	ADCLDRCtl0 &= ~ADC10IFG;
	ADCLDRCtl0 &= ~(ADC10ON + ENC + ADC10IE + ADC10SC);
}

void trigger_ldr(void)
{
	ADCLDRCtl0 &= ~(ENC);
	if((ADCLDRCtl1 & ADC10BUSY) != 0)
		return;
	ADCLDRCtl0 |= ADC10SC + ENC;
}

void enable_ultrasonic(void)
{
	Timer1Cap_Ultra = CM_3 + CAP + CCIE + CCIS_0 + SCS; //capture mode CCIB1
	// Timer1Ctl |= TAIE;
}
void disable_ultrasonic(void)
{
	Timer1Cap_Ultra = 0;
}

void trigger_ultrasonic(void)
{
	UltrasonicPort |= UltrasonicPinTrig;
	delay(30);
	UltrasonicPort &= ~UltrasonicPinTrig;
}

void handle_ultrasonic(unsigned int time)
{
	if((Timer1Cap_Ultra & CCI) != 0){
		Timer1Ctl |= TACLR;
		return;
	}
	unsigned char msg[3];
	msg[0] = MAKEULTRASONIC(get_radar_deg());
	msg[1] = ((unsigned char *) &time)[0];
	msg[2] = ((unsigned char *) &time)[1];
	add_msg_tx_queue(msg, 3);
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

int write_flash(uchar *seg, uchar *buf, uchar sz)
{
	int i;
	if(sz > 64)
		return 1;
	if(seg != segments[0] &&
	   seg != segments[1] &&
	   seg != segments[2])
		return 1;
	FCTL2 = FWKEY + FSSEL_2 + FN1;
	FCTL3 = FWKEY;
	FCTL1 = FWKEY + ERASE;
	seg[0] = 'x';
	FCTL1 = FWKEY + WRT;
	//write
	for(i = 0; i <= sz; i++)
		seg[i] = buf[i];
	FCTL1 = FWKEY;
	FCTL3 = FWKEY + LOCK;
	return 0;
}

void enable_t0timer(unsigned char d)
{
	set_timer_interrupt(d);
	Timer0Ctl |= TAIE + TACLR;
	Timer0Ctl |= ID_1 + MC_3;
	Timer0Ctl &= ~TAIFG; // Clear  Timer Flag
}

void disable_t0timer(void)
{
	// set_timer_interrupt(0);
	Timer0Ctl &= ~(TAIE + TAIFG);
	Timer0Ctl |= TACLR;
	Timer0Ctl &= ~MC_3;
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
int file_handler();

extern struct file_manager fmanager;

extern unsigned char telem_deg;
extern unsigned char file_size;
extern unsigned char telem_deg;
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
				case file_0:
					if(state != do_file || fmanager.curr_file != 0){
						fmanager.first_enter = 1;
						fmanager.file[0] = (uchar *) FILE0PTR;
					}
					state = do_file;
					fmanager.curr_file = 0;
					wakeup = 1;
					break;
				case file_1:
					if(state != do_file || fmanager.curr_file != 1){
						fmanager.first_enter = 1;
						fmanager.file[1] = (uchar *) FILE1PTR;
					}
					state = do_file;
					fmanager.curr_file = 1;
					wakeup = 1;
					break;
				case file_2:
					if(state != do_file || fmanager.curr_file != 2){
						fmanager.first_enter = 1;
						fmanager.file[2] = (uchar *) FILE2PTR;
					}
					state = do_file;
					fmanager.curr_file = 2;
					wakeup = 1;
					break;
				default:
					break;
			}
			break;
		case 1:
			// enter telemeter state
			telem_deg = MSGDATA(rec);
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

	adc_set_calibrate();
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
	unsigned char wakeup = 0;
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
				case telemeter_s:
					wakeup = telemeter_s_handler();
					break;
				case file_rec_s: break;
				case sonic_d:
					wakeup = sonic_d_handler();
					break;
				case ldr_d:
					wakeup = ldr_d_handler();
					break;
				case dual_d:
					wakeup = dual_d_handler();
					break;
				case do_file:
					wakeup = file_handler();
					break;
			}
			break;
	  default: break;
	}

wakeup:
	if(wakeup == 0)
		return;

	disable_interrupts();
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
		case TA1IV_TACCR1:                    // Vector  2:  TACCR1 CCIFG
			break;
		case TA1IV_TACCR2:              	  // Vector  4:  TACCR2 CCIFG
		    handle_ultrasonic(TA1CCR2);
		break;
		case TA1IV_6: break;                  // Vector  6:  Reserved CCIFG
		case TA1IV_8: break;                  // Vector  8:  Reserved CCIFG
		case TA1IV_TAIFG: break;              // Vector 10:  TAIFG
		default: break;
	}
}

void ADC10_handler(int a0, int a3);
void calib_cm_array(int a0, int a3);
static uchar calib = 0;

void adc_set_calibrate()
{
	calib = 1;
}

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
	int a3 = adc10_samples[0] + adc10_samples[4] + adc10_samples[8] + adc10_samples[12];
	int a0 = adc10_samples[3] + adc10_samples[7] + adc10_samples[11] + adc10_samples[15];
	ADC10SA  =    adc10_samples;
	if(calib){
		calib_cm_array(a0, a3);
		calib = 0;
	}
	switch(state)
	{
		case idle:
			break;
		case telemeter_s:
			break;
		case file_rec_s:
			break;
		case sonic_d:
			break;
		case ldr_d:
			ADC10_handler(a0, a3);
			break;
		case dual_d:
			ADC10_handler(a0, a3);
			break;
		case do_file:
			ADC10_handler(a0, a3);
			break;
		default: break;
	}
}

