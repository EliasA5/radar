#include  "../header/bsp.h"    // private library - BSP layer

void GPIOconfig(void)
{
    WDTCTL = WDTHOLD | WDTPW;                 // Stop WDT

	if (CALBC1_1MHZ==0xFF)                    // If calibration constant erased
	{
		while(1);                             // do not load, trap CPU!!
	}
	DCOCTL = 0;                               // Select lowest DCOx and MODx settings
	BCSCTL1 = CALBC1_1MHZ;                    // Set DCO
	DCOCTL = CALDCO_1MHZ;
	// TODO lower the speed to accomodate flash writing
	// RGB_DIR |= 0x07;
	// RGB_SEL &= ~0x07;
	// RGB_OUT &= ~0x07;

	P1SEL |= BIT1 + BIT2;                     // RX and TX
	P1SEL2 |= BIT1 + BIT2;

	// Ultrasonic Trigger
	UltrasonicDir |= UltrasonicPinTrig;
	UltrasonicSel &= ~UltrasonicPinTrig;

	// Ultasonic Echo
	UltrasonicDir &= ~UltrasonicPinEcho;
	UltrasonicSel |= UltrasonicPinEcho;

	// Servo pwm
	ServoDir |= ServoPin;
	ServoSel |= ServoPin;


	// LEDs 8-bit Array Port configuration
	// LEDsArrPortSel &= ~0xFF;               // GPIO capability
	// LEDsArrPortDir |= 0xFF;                // output dir
	// LEDsArrPort = 0x00;

	// PushButtons Setup
	// Inputs and interrupts
	// PBsArrPortSel &= ~0x07;
	// PBsArrPortDir &= ~0x07;
	// PBsArrIntEdgeSel |= 0x03;              // pull-up mode
	// PBsArrIntEdgeSel &= ~0x0C;             // pull-down mode
	// PBsArrIntEn |= 0x07;
	// PBsArrIntPend &= ~0x07;                // clear pending interrupts

}

void TIMERconfig(void)
{

	// Timer A0 config
	Timer0Ctl  = TASSEL_1 + MC_3 + ID_1;
	Timer0Ctl &= ~TAIFG; // Clear  Timer Flag

	Timer1Ctl = TASSEL_2 + MC_1;

	Timer1Cmp_Servo = OUTMOD_3;
	//Timer1Cmp_Servo &= ~CCIFG;

	//Timer1Cap_Ultra = CM_3 + CAP + CCIE; capture mode CCIA2
	Timer1Cap_Ultra = 0;

	Timer1Ctl &= ~TAIFG;
}

void ADCconfig(void)
{
	ADCLDRCtl0 = SREF_0 + ADC10SHT_0 + MSC;
	ADCLDRCtl1 = INCH_3 + ADC10SSEL_1 + CONSEQ_3 + ADC10DIV_0;
	ADC10AE0 = 0x09; // Enable A3 + A0 Analog input
}

void USCIconfig(void)
{
	UCA0CTL1 |= UCSSEL_2;                     // CLK = SMCLK
	UCA0BR0 = 104;
	UCA0BR1 = 0x00;
	UCA0MCTL = UCBRS0;                        // Modulation UCBRSx = 3
	UCA0CTL1 &= ~UCSWRST;                     // **Initialize USCI state machine**
	IE2 |= UCA0RXIE;// + UCA0TXIE;            // Enable USCI_A0 RX interrupt
}

