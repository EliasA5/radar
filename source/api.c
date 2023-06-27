#include "../header/api.h"    	    // private library - API layer
#include "../header/halGPIO.h"      // private library - HAL layer
#include "../header/LCD.h"


static char digits[] = "0123456789ABCDEF";
char *print_int(unsigned int x)
{
	unsigned int bases[] = {10000, 1000, 100, 10, 1};
	static char buf[6];
	int i = 0,k,j,found = -1;
	x++;
	for(k = 0; k < sizeof(bases)/sizeof(unsigned int); k++){
		for(j = 0; x > bases[k]; x -= bases[k], j++);
		if(found == -1 && j)
			found = i;
		buf[i++] = digits[j];
	}
	if(found == -1)
		found = 4;
	return &buf[found];
}


// we use qformat q2.8 on the output of the adc
char *print_qformat_2_8(unsigned int x)
{
	unsigned int bases[] = {100, 10, 1};
	static char buf[6];
	static unsigned int prev = 0xffff;
	unsigned int i = 0,j,k;
	if(x == prev)
		return buf;
	prev = x;
	x &= 0x3ff;
	buf[i++] = digits[(x >> 8)];
	buf[i++] = '.';
	// keep only the q bits
	x &= 0x00ff;
	// they are from 0-255, multiply by 4
	// and cut off at 999.
	// we can then print the fraction
	x <<= 2;
	if(x > 999)
		x = 1000;
	for(k = 0; k < 3; k++){
		for(j = 0; x > bases[k]; x -= bases[k], j++);
		buf[i++] = digits[j];
	}

	return buf;
}

unsigned int telem_deg = 0;

void state1_enter()
{

}

void state1_leave()
{

}

int state1_handler()
{

	return 0;
}

void state2_enter()
{

}

void state2_leave()
{

}

int state2_handler()
{

	return 0;
}

void state3_enter()
{

}

void state3_leave()
{

}

int state3_handler()
{

	return 0;
}

void state4_enter()
{

}

void state4_leave()
{

}

int state4_handler()
{

	return 0;
}

void state5_enter()
{

}

void state5_leave()
{

}

int state5_handler()
{

	return 0;
}

void ADC10_handler()
{

}

void state6_enter()
{

}

void state6_leave()
{

}

int state6_handler()
{

	return 0;
}

void state7_enter()
{

}

int state7_handler()
{

	return 0;
}

void state7_leave()
{

}

