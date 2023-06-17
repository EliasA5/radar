#include "../header/api.h"    		// private library - API layer
#include "../header/app.h"    		// private library - APP layer
#include "../header/LCD.h"
#include "../header/halGPIO.h"

enum FSMstate state;
enum SYSmode lpm_mode;

void main(void) {

	state = idle;     // start in idle state on RESET
	lpm_mode = mode0; // start in idle state on RESET
	sysConfig();

	while (1) {
		switch (state) {
			case idle:
				enable_interrupts();
				enterLPM(lpm_mode);
				break;

			case state1:
				state1_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state1_leave();
				break;

			case state2:
				state2_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state2_leave();
				break;

			case state3:
				state3_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state3_leave();
				break;

			case state4:
				state4_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state4_leave();
				break;

			case state5:
				state5_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state5_leave();
				break;

			case state6:
				state6_enter();
				state6_handler();
				state6_leave();
				break;

			case state7:
				state7_enter();
				enable_interrupts();
				enterLPM(lpm_mode);
				state7_leave();
				break;

		}
	}
}

