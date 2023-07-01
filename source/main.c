#include "../header/api.h"
#include "../header/app.h"
#include "../header/LCD.h"
#include "../header/halGPIO.h"

enum FSMstate state;
enum SYSmode lpm_mode;

int main(void) {

	state = idle;
	lpm_mode = mode0;
	sysConfig();

	while (1) {
		switch (state) {
			case idle:
				enable_interrupts();
				enterLPM(lpm_mode);
				break;

			case telemeter_s:
				telemeter_s_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				telemeter_s_leave();
				break;

			case file_rec_s:
				file_rec_s_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				file_rec_s_leave();
				break;

			case sonic_d:
				sonic_d_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				sonic_d_leave();
				break;

			case ldr_d:
				ldr_d_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				ldr_d_leave();
				break;

			case dual_d:
				dual_d_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				dual_d_leave();
				break;

			case file_1:
				file_1_enter();
				enterLPM(lpm_mode);
				file_2_leave();
				break;

			case file_2:
				file_2_enter();
				enterLPM(lpm_mode);
				file_2_leave();
				break;

			case file_3:
				file_3_enter();
				enterLPM(lpm_mode);
				file_3_leave();
				break;

		}
	}
}

