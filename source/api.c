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
void telemeter_s_enter()
{
	add_ack_tx_queue(MAKEACK(telemeter_s));
}

void telemeter_s_leave()
{

}

int telemeter_s_handler()
{

	return 0;
}

unsigned char file_buf[60] = {0};
unsigned char file_buf_idx = 0;
unsigned char file_size = 0;
void file_rec_s_enter()
{
	file_buf_idx = 0;
	add_ack_tx_queue(MAKEACK(file_rec_s));
}

void file_rec_s_leave()
{
	// write_flash(file_buf, file_size);
	add_ack_tx_queue(MAKEACK(file_rec_s));
}

int file_rec_s_handler(unsigned char next)
{
	file_buf[file_buf_idx] = next;
	if((++file_buf_idx) >= file_size)
		return 0;
	add_ack_tx_queue(MAKEACK(file_rec_s));
	return 1;
}

void sonic_d_enter()
{
	add_ack_tx_queue(MAKEACK(sonic_d));
}

void sonic_d_leave()
{

}

int sonic_d_handler()
{

	return 0;
}

void ldr_d_enter()
{
	add_ack_tx_queue(MAKEACK(ldr_d));
}

void ldr_d_leave()
{

}

int ldr_d_handler()
{

	return 0;
}

void dual_d_enter()
{
	add_ack_tx_queue(MAKEACK(dual_d));
}

void dual_d_leave()
{

}

int dual_d_handler()
{

	return 0;
}

void ADC10_handler()
{

}

void file_1_enter()
{
	add_ack_tx_queue(MAKEACK(file_1));
}

void file_1_leave()
{

}

int file_1_handler()
{

	return 0;
}

void file_2_enter()
{
	add_ack_tx_queue(MAKEACK(file_2));
}

void file_2_leave()
{

}

int file_2_handler()
{

	return 0;
}

void file_3_enter()
{
	add_ack_tx_queue(MAKEACK(file_3));
}

void file_3_leave()
{

}

int file_3_handler()
{

	return 0;
}

