/* - - - - - - - LCD interface - - - - - - - - -
 *	This code will interface to a standard LCD controller
 *  It uses it in 4 or 8 bit mode.
 */
#include "../header/bsp.h"
#include "../header/LCD.h"
#include "../header/halGPIO.h"


void lcd_cmd(unsigned char c)
{

	LCD_WAIT; // may check LCD busy flag, or just delay a little, depending on lcd.h

	if (LCD_MODE == FOURBIT_MODE){
		LCD_DATA_WRITE &= ~OUTPUT_DATA;// clear bits before new write
		LCD_DATA_WRITE |= ((c >> 4) & 0x0F) << LCD_DATA_OFFSET;
		lcd_strobe();
		LCD_DATA_WRITE &= ~OUTPUT_DATA;
		LCD_DATA_WRITE |= (c & (0x0F)) << LCD_DATA_OFFSET;
		lcd_strobe();
	}
	else {
		LCD_DATA_WRITE = c;
		lcd_strobe();
	}
}

void lcd_data(unsigned char c)
{
	LCD_WAIT; // may check LCD busy flag, or just delay a little, depending on lcd.h

	LCD_DATA_WRITE &= ~OUTPUT_DATA;
	LCD_RS(1);
	if (LCD_MODE == FOURBIT_MODE){
		LCD_DATA_WRITE &= ~OUTPUT_DATA;
		LCD_DATA_WRITE |= ((c >> 4) & 0x0F) << LCD_DATA_OFFSET;
		lcd_strobe();
		LCD_DATA_WRITE &= (0xF0 << LCD_DATA_OFFSET) | (0xF0 >> (8 - LCD_DATA_OFFSET));
		LCD_DATA_WRITE &= ~OUTPUT_DATA;
		LCD_DATA_WRITE |= (c & 0x0F) << LCD_DATA_OFFSET;
		lcd_strobe();
	}
	else {
		LCD_DATA_WRITE = c;
		lcd_strobe();
	}

	LCD_RS(0);
}

void lcd_puts(const char * s)
{
	int max = 17;
	while(*s && (--max))
		lcd_data(*s++);
}

void lcd_init()
{
	char init_value;

	if (LCD_MODE == FOURBIT_MODE)
		init_value = 0x3 << LCD_DATA_OFFSET;
	else
		init_value = 0x3F;

	LCD_RS_DIR(OUTPUT_PIN);
	LCD_EN_DIR(OUTPUT_PIN);
	LCD_RW_DIR(OUTPUT_PIN);
	LCD_DATA_DIR |= OUTPUT_DATA;
	LCD_RS(0);
	LCD_EN(0);
	LCD_RW(0);

	delayms(15);
	LCD_DATA_WRITE &= ~OUTPUT_DATA;
	LCD_DATA_WRITE |= init_value;
	lcd_strobe();
	delayms(5);
	LCD_DATA_WRITE &= ~OUTPUT_DATA;
	LCD_DATA_WRITE |= init_value;
	lcd_strobe();
	delay(200);
	LCD_DATA_WRITE &= ~OUTPUT_DATA;
	LCD_DATA_WRITE |= init_value;
	lcd_strobe();

	if (LCD_MODE == FOURBIT_MODE){
		LCD_WAIT; // may check LCD busy flag, or just delay a little, depending on lcd.h
		LCD_DATA_WRITE &= ~OUTPUT_DATA;
		LCD_DATA_WRITE |= 0x2 << LCD_DATA_OFFSET; // Set 4-bit mode
		lcd_strobe();
		lcd_cmd(0x28); // Function Set
	}
	else
		lcd_cmd(0x3C); // 8bit,two lines,5x10 dots

	lcd_cmd(0xF); //Display On, Cursor On, Cursor Blink
	lcd_cmd(0x1); //Display Clear
	lcd_cmd(0x6); //Entry Mode
	lcd_cmd(0x80); //Initialize DDRAM address to zero
}

void lcd_strobe()
{
	LCD_EN(1);
	__no_operation();
	__no_operation();
	LCD_EN(0);
}

