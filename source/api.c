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

static unsigned int lcd_max = 0;
int inc_lcd()
{
	static unsigned int x = 0;
	lcd_clear();
	lcd_puts(print_int(x));
	x++;
	if(x >= lcd_max){
		x = 0;
		return 1;
	}
	return 0;
}

static unsigned int lcd_dec_x = 0;
int dec_lcd()
{
	lcd_clear();
	lcd_puts(print_int(lcd_dec_x));
	lcd_dec_x--;
	if(lcd_dec_x == 0)
		return 1;
	return 0;
}

unsigned char rra_char = '\0';
int rra_lcd()
{
	static int lcd_index = 0;
	if(lcd_index == 0){
		lcd_putchar(rra_char);
		goto ret;
	}
	if(lcd_index == 16){
		lcd_putchar(' ');
		lcd_new_line();
		lcd_putchar(rra_char);
		goto ret;
	}
	lcd_cursor_left();
	lcd_putchar(' ');
	lcd_putchar(rra_char);

ret:
	lcd_index++;
	if(lcd_index == 32){
		lcd_index = 0;
		// TODO check if needed
		lcd_clear();
		return 1;
	}
	return 0;
}

int adc10_samples[16] = {0};

unsigned char telem_deg = 0;
void telemeter_s_enter()
{
	enable_t0timer(10);
	set_radar_deg(telem_deg);
	enable_ultrasonic();
	add_ack_tx_queue(MAKEACK(telemeter_s));
}

void telemeter_s_leave()
{
	disable_ultrasonic();
	disable_t0timer();
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
	enable_t0timer(10);
	set_radar_deg(0);
	set_max_radar_deg(60);
	enable_ultrasonic();
	add_ack_tx_queue(MAKEACK(sonic_d));
}

void sonic_d_leave()
{
	set_radar_deg(0);
	disable_ultrasonic();
	disable_t0timer();
}

int sonic_d_handler()
{
	
	return 0;
}

void ldr_d_enter()
{
	enable_t0timer(10);
	set_radar_deg(0);
	set_max_radar_deg(60);
	activate_ldr();
	add_ack_tx_queue(MAKEACK(ldr_d));
}

void ldr_d_leave()
{
	set_radar_deg(0);
	deactivate_ldr();
	disable_t0timer();
}

int ldr_d_handler()
{

	return 0;
}

void dual_d_enter()
{
	enable_t0timer(10);
	set_radar_deg(0);
	set_max_radar_deg(60);
	activate_ldr();
	enable_ultrasonic();
	add_ack_tx_queue(MAKEACK(dual_d));

}

void dual_d_leave()
{
	set_radar_deg(0);
	deactivate_ldr();
	disable_ultrasonic();
	disable_t0timer();
}

int dual_d_handler()
{

	return 0;
}

void ADC10_handler(int a0, int a3)
{
	unsigned char sample = (a0 + a3) >> 5;
	unsigned char msg[2];
	msg[0] = MAKELDR(get_radar_deg());
	msg[1] = sample;
	add_msg_tx_queue(msg, 2);
}

void file_enter()
{
	add_ack_tx_queue(MAKEACK(file_1));
}

void file_leave()
{

}

int file_handler()
{

	return 0;
}
