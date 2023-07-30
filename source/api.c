#include "../header/api.h"    	    // private library - API layer
#include "../header/halGPIO.h"      // private library - HAL layer
#include "../header/LCD.h"
#include "../header/app.h"

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

char *print_uchar(uchar x)
{
	unsigned int bases[] = {100, 10, 1};
	static char buf[4];
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

static uchar lcd_max = 0;
static uchar lcd_curr = 0;
static void inc_lcd_enter(uchar max)
{
	lcd_max = max;	
	lcd_curr = 0;
	lcd_clear();
}

static void inc_lcd_leave()
{
	lcd_clear();
}

int inc_lcd()
{
	lcd_clear();
	lcd_puts(print_uchar(lcd_curr));
	lcd_curr++;
	if(lcd_curr >= lcd_max){
		lcd_curr = 0;
		return 1;
	}
	return 0;
}


static uchar lcd_dec_x = 0;
static void dec_lcd_enter(uchar lcd_dec_num)
{
	lcd_dec_x = lcd_dec_num;
	lcd_clear();
}

static void dec_lcd_leave()
{
	lcd_clear();
}

int dec_lcd()
{
	lcd_clear();
	lcd_puts(print_uchar(lcd_dec_x));
	lcd_dec_x--;
	if(lcd_dec_x == 0)
		return 1;
	return 0;
}

unsigned char rra_char = '\0';
static int lcd_index = 0;

static void rra_lcd_enter(uchar c)
{
	rra_char = c;
	lcd_clear();
	lcd_index = 0;
}

static void rra_lcd_leave()
{
	rra_char = '\0';
	lcd_clear();
	lcd_index = 0;
}

int rra_lcd()
{
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
	set_radar_deg(telem_deg);
	enable_ultrasonic();
	enable_t0timer(10);
	add_ack_tx_queue(MAKEACK(telemeter_s));
}

void telemeter_s_leave()
{
	disable_t0timer();
	disable_ultrasonic();
}

int telemeter_s_handler()
{
	trigger_ultrasonic();
	return 0;
}

extern struct file_manager fmanager;
uchar file_buf[61] = {0};
uchar file_buf_idx = 0;
uchar file_size = 0;
void file_rec_s_enter()
{
	file_buf_idx = 0;
	add_ack_tx_queue(MAKEACK(file_rec_s));
}

void file_rec_s_leave()
{
	uchar *seg = segments[fmanager.file_to_replace];
	write_flash(seg, file_buf, file_buf_idx);
	fmanager.valid[fmanager.file_to_replace] = 1;
	if((++fmanager.file_to_replace) > 2)
		fmanager.file_to_replace = 0;
	add_ack_tx_queue(MAKEACK(file_rec_s));
}

int file_rec_s_handler(unsigned char next)
{
	file_buf[file_buf_idx] = next;
	if((++file_buf_idx) >= file_size){
		file_buf[file_buf_idx] = 0;
		return 0;
	}
	add_ack_tx_queue(MAKEACK(file_rec_s));
	return 1;
}

void sonic_d_enter()
{
	set_radar_deg(0);
	set_max_radar_deg(60);
	enable_ultrasonic();
	enable_t0timer(10);
	add_ack_tx_queue(MAKEACK(sonic_d));
}

void sonic_d_leave()
{
	disable_t0timer();
	set_radar_deg(0);
	disable_ultrasonic();
}

int sonic_d_handler()
{
    trigger_ultrasonic();
    return update_degree();
}

void ldr_d_enter()
{
	set_radar_deg(0);
	set_max_radar_deg(60);
	activate_ldr();
	enable_t0timer(10);
	add_ack_tx_queue(MAKEACK(ldr_d));
}

void ldr_d_leave()
{
	disable_t0timer();
	set_radar_deg(0);
	deactivate_ldr();
}

int ldr_d_handler()
{
	trigger_ldr();
	return update_degree();
}

void dual_d_enter()
{
	set_radar_deg(0);
	set_max_radar_deg(60);
	activate_ldr();
	enable_ultrasonic();
	enable_t0timer(10);
	add_ack_tx_queue(MAKEACK(dual_d));

}

void dual_d_leave()
{
	disable_t0timer();
	set_radar_deg(0);
	deactivate_ldr();
	disable_ultrasonic();
}

int dual_d_handler()
{
	trigger_ultrasonic();
	trigger_ldr();
	return update_degree();
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
	uchar sleep = 1;
	if(fmanager.first_enter){
		fmanager.d = 50;
		fmanager.first_enter = 0;
		add_ack_tx_queue(MAKEACK(do_file + fmanager.curr_file));
	}

	if(fmanager.valid[fmanager.curr_file] == 0){
		state = idle;
		return;
	}

	uchar arg1 = (*(fmanager.file[fmanager.curr_file] + 1));
	uchar arg2 = (*(fmanager.file[fmanager.curr_file] + 2));
	switch(*fmanager.file[fmanager.curr_file])
	{
		case 0: // end of file
			fmanager.file[fmanager.curr_file] = segments[fmanager.curr_file];
			sleep = 0;
			break;
		case 1:
			inc_lcd_enter(arg1);
			enable_t0timer(fmanager.d);
			break;
		case 2:
			dec_lcd_enter(arg1);
			enable_t0timer(fmanager.d);
			break;
		case 3:
			rra_lcd_enter(arg1);
			enable_t0timer(fmanager.d);
			break;
		case 4:
			fmanager.d = arg1;
			sleep = 0;
			break;
		case 5:
			lcd_clear();
			sleep = 0;
			break;
		case 6:
            set_radar_deg(arg1);
            enable_ultrasonic();
            enable_t0timer(fmanager.d);
            break;
		case 7:
			set_radar_deg(arg1);
			set_max_radar_deg(arg2);
			enable_ultrasonic();
			enable_t0timer(fmanager.d);
			break;
		case 8:
			sleep = 0;
			break;
		default:
			sleep = 0;
			break;
	}

	if(sleep)
		enterLPM(lpm_mode);
}

void file_leave()
{
	uchar arg1 = (*(fmanager.file[fmanager.curr_file] + 1));
	uchar arg2 = (*(fmanager.file[fmanager.curr_file] + 2));
	switch(*fmanager.file[fmanager.curr_file])
	{
		case 1:
			disable_t0timer();
			inc_lcd_leave();
			fmanager.file[fmanager.curr_file] += 2;
			break;
		case 2:
			disable_t0timer();
			dec_lcd_leave();
			fmanager.file[fmanager.curr_file] += 2;
			break;
		case 3:
			disable_t0timer();
			rra_lcd_leave();
			fmanager.file[fmanager.curr_file] += 2;
			break;
		case 4:
			fmanager.file[fmanager.curr_file] += 2;
			break;
		case 5:
			fmanager.file[fmanager.curr_file] += 1;
			break;
		case 6:
			disable_t0timer();
			disable_ultrasonic();
			fmanager.file[fmanager.curr_file] += 2;
			break;
		case 7:
			disable_t0timer();
			disable_ultrasonic();
			fmanager.file[fmanager.curr_file] += 3;
			break;
		case 8:
			state = idle;
			break;
		default:
			state = idle;
			break;
	}
}

int file_handler()
{
	static uchar servo_deg_counter = 0;
	uchar wakeup = 0;
	switch(*fmanager.file[fmanager.curr_file])
	{
		case 1:
			wakeup = inc_lcd();
			break;
		case 2:
			wakeup = dec_lcd();
			break;
		case 3:
			wakeup = rra_lcd();
			break;
		case 4:
			
			break;
		case 5:

			break;
		case 6:
			if((++servo_deg_counter) >= 10){
				servo_deg_counter = 0;
				wakeup = 1;
				break;
			}
			trigger_ultrasonic();
			break;
		case 7:
			if((wakeup = update_degree()) != 0)
				break;
			trigger_ultrasonic();
			break;
		case 8:

			break;
		default:
			wakeup = 1;
			break;
	}

	return wakeup;
}
