/*
 * Common LCD routines for supported CPUs
 *
 * (C) Copyright 2001-2002
 * Wolfgang Denk, DENX Software Engineering -- wd@denx.de
 *
 * See file CREDITS for list of people who contributed to this
 * project.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

/************************************************************************/
/* ** HEADER FILES							*/
/************************************************************************/

/* #define DEBUG */

#include <config.h>
#include <common.h>
#include <command.h>
#include <version.h>
#include <stdarg.h>
#include <linux/types.h>
#include <devices.h>
#if defined(CONFIG_POST)
#include <post.h>
#endif
#include <lcd.h>
#include <watchdog.h>

#if defined(CONFIG_PXA250)
#include <asm/byteorder.h>
#endif

#if defined(CONFIG_MPC823)
#include <lcdvideo.h>
#endif

#include <common.h>
#include <regs.h>
#include <regs-lcd.h>

#ifdef CONFIG_LCD

#define S3CFB_HSW               41
#define S3CFB_HBP               2
#define S3CFB_HFP               2

#define S3CFB_VSW               10
#define S3CFB_VBP               2
#define S3CFB_VFP               2

#define S3CFB_HRES              480
#define S3CFB_VRES              272
#define S3CFB_VFRAME_FREQ       60
#if LCD_BPP == LCD_COLOR32
#define PIXELBITS               32
#else
#define PIXELBITS               16
#endif

#define S3CFB_IVCLK             0//CFG_LOW
#define S3CFB_IHSYNC            1//CFG_HIGH
#define S3CFB_IVSYNC            1//CFG_HIGH
#define S3CFB_IVDEN             0//CFG_LOW

#define S3CFB_PIXEL_CLOCK       (S3CFB_VFRAME_FREQ * (S3CFB_HFP + S3CFB_HSW + S3CFB_HBP + S3CFB_HRES) * (S3CFB_VFP + S3CFB_VSW + S3CFB_VBP + S3CFB_VRES))

int lcd_line_length;

int lcd_color_fg;
int lcd_color_bg;

/*
 * Frame buffer memory information
 */
void *lcd_base;			/* Start of framebuffer memory  */
void *lcd_console_address;	/* Start of console buffer      */
uchar *osd_frame_buffer;

short console_col = 0;
short console_row = 0;

typedef struct vidinfo {
    ushort    vl_col;
    ushort    vl_row;
    ushort    vl_width;
    ushort    vl_height;
    u_char    vl_bpix;
} vidinfo_t;

vidinfo_t panel_info = {
	S3CFB_HRES,
	S3CFB_VRES,
	0,
	0,
	PIXELBITS,
};

/************************************************************************/
/* ** FONT DATA								*/
/************************************************************************/
#include <video_font.h>		/* Get font data, width and height	*/

/************************************************************************/
/* ** LOGO DATA								*/
/************************************************************************/
#ifdef CONFIG_LCD_LOGO
# include <bmp_logo.h>		/* Get logo data, width and height	*/
# if (CONSOLE_COLOR_WHITE >= BMP_LOGO_OFFSET)
#  error Default Color Map overlaps with Logo Color Map
# endif
#endif

DECLARE_GLOBAL_DATA_PTR;

ulong lcd_setmem (ulong addr);
ulong calc_fbsize (void);

static void lcd_drawchars (ushort x, ushort y, uchar *str, int count);
static inline void lcd_puts_xy (ushort x, ushort y, uchar *s);
static inline void lcd_putc_xy (ushort x, ushort y, uchar  c);

static int lcd_init (void *lcdbase);

static int lcd_clear (cmd_tbl_t * cmdtp, int flag, int argc, char *argv[]);
extern void lcd_enable (void);
static void *lcd_logo (void);


#if LCD_BPP == LCD_COLOR8
extern void lcd_setcolreg (ushort regno,
				ushort red, ushort green, ushort blue);
#endif
#if LCD_BPP == LCD_MONOCHROME
extern void lcd_initcolregs (void);
#endif

static int lcd_getbgcolor (void);
static void lcd_setfgcolor (int color);
static void lcd_setbgcolor (int color);

char lcd_is_enabled = 0;
extern vidinfo_t panel_info;

#ifdef	NOT_USED_SO_FAR
static void lcd_getcolreg (ushort regno,
				ushort *red, ushort *green, ushort *blue);
static int lcd_getfgcolor (void);
#endif	/* NOT_USED_SO_FAR */

/************************************************************************/

/*----------------------------------------------------------------------*/

static void console_scrollup (void)
{
#if 1
	lcd_console_address = (void*)osd_frame_buffer;

	/* Copy up rows ignoring the first one */
	memcpy (CONSOLE_ROW_FIRST, CONSOLE_ROW_SECOND, CONSOLE_SCROLL_SIZE);

	/* Clear the last one */
	memset (CONSOLE_ROW_LAST, COLOR_MASK(lcd_color_bg), CONSOLE_ROW_SIZE);
#else
	/*
	 * Poor attempt to optimize speed by moving "long"s.
	 * But the code is ugly, and not a bit faster :-(
	 */
	ulong *t = (ulong *)CONSOLE_ROW_FIRST;
	ulong *s = (ulong *)CONSOLE_ROW_SECOND;
	ulong    l = CONSOLE_SCROLL_SIZE / sizeof(ulong);
	uchar  c = lcd_color_bg & 0xFF;
	ulong val= (c<<24) | (c<<16) | (c<<8) | c;

	while (l--)
		*t++ = *s++;

	t = (ulong *)CONSOLE_ROW_LAST;
	l = CONSOLE_ROW_SIZE / sizeof(ulong);

	while (l-- > 0)
		*t++ = val;
#endif
}

/*----------------------------------------------------------------------*/

static inline void console_back (void)
{
	if (--console_col < 0) {
		console_col = CONSOLE_COLS-1 ;
		if (--console_row < 0) {
			console_row = 0;
		}
	}

	lcd_putc_xy (console_col * VIDEO_FONT_WIDTH,
		     console_row * VIDEO_FONT_HEIGHT,
		     ' ');
}

/*----------------------------------------------------------------------*/

static inline void console_newline (void)
{
	++console_row;
	console_col = 0;

	/* Check if we need to scroll the terminal */
	if (console_row >= CONSOLE_ROWS) {
		/* Scroll everything up */
		console_scrollup () ;
		--console_row;
	}
}

/*----------------------------------------------------------------------*/

void lcd_putc (const char c)
{
#if 0
	if (!lcd_is_enabled) {
		serial_putc(c);
		return;
	}
#else
	serial_putc(c);
#endif

	switch (c) {
	case '\r':
			console_col = 0;
			return;
	case '\n':
			console_newline();
			return;
	case '\t':/* Tab (8 chars alignment) */
			console_col +=  8;
			console_col &= ~7;

			if (console_col >= CONSOLE_COLS) {
				console_newline();
			}
			return;
	case '\b':
			console_back();
			return;
	default:
			lcd_putc_xy (console_col * VIDEO_FONT_WIDTH,
			             console_row * VIDEO_FONT_HEIGHT,
				         c);
			if (++console_col >= CONSOLE_COLS) {
				console_newline();
			}
			return;
	}
	/* NOTREACHED */
}

/*----------------------------------------------------------------------*/

void lcd_puts (const char *s)
{
	if (!lcd_is_enabled) {
		serial_puts (s);
		return;
	}

	while (*s) {
		lcd_putc (*s++);
	}
}

/************************************************************************/
/* ** Low-Level Graphics Routines					*/
/************************************************************************/

static void lcd_drawchars (ushort x, ushort y, uchar *str, int count)
{
	uchar *dest;
	ushort off, row;

	dest = (uchar *)(osd_frame_buffer + y * lcd_line_length + x * (1 << LCD_BPP) / 8);
	off  = x * (1 << LCD_BPP) % 8;

	for (row=0;  row < VIDEO_FONT_HEIGHT;  ++row, dest += lcd_line_length)  {
		uchar *s = str;
		int i;
#if LCD_BPP == LCD_COLOR32
	unsigned int *d = (unsigned int*)dest;
#elif LCD_BPP == LCD_COLOR16
	ushort *d = (ushort *)dest;
#else
	uchar *d = dest;
#endif

#if LCD_BPP == LCD_MONOCHROME
		uchar rest = *d & -(1 << (8-off));
		uchar sym;
#endif
		for (i=0; i<count; ++i) {
			uchar c, bits;

			c = *s++;
			bits = video_fontdata[c * VIDEO_FONT_HEIGHT + row];

#if LCD_BPP == LCD_MONOCHROME
			sym  = (COLOR_MASK(lcd_color_fg) & bits) |
			       (COLOR_MASK(lcd_color_bg) & ~bits);

			*d++ = rest | (sym >> off);
			rest = sym << (8-off);
#elif LCD_BPP == LCD_COLOR8
			for (c=0; c<8; ++c) {
				*d++ = (bits & 0x80) ?
						lcd_color_fg : lcd_color_bg;
				bits <<= 1;
			}
#elif LCD_BPP == LCD_COLOR16
			for (c=0; c<8; ++c) {
				*d++ = (bits & 0x80) ?
						lcd_color_fg : lcd_color_bg;
				bits <<= 1;
			}
#elif LCD_BPP == LCD_COLOR32
			for (c=0; c<8; ++c) {
				*d++ = (bits & 0x80) ?
						lcd_color_fg : lcd_color_bg;
				bits <<= 1;
			}
#endif
		}
#if LCD_BPP == LCD_MONOCHROME
		*d  = rest | (*d & ((1 << (8-off)) - 1));
#endif
	}
}

/*----------------------------------------------------------------------*/

static inline void lcd_puts_xy (ushort x, ushort y, uchar *s)
{
#if defined(CONFIG_LCD_LOGO) && !defined(CONFIG_LCD_INFO_BELOW_LOGO)
	lcd_drawchars (x, y+BMP_LOGO_HEIGHT, s, strlen ((char *)s));
#else
	lcd_drawchars (x, y, s, strlen ((char *)s));
#endif
}

/*----------------------------------------------------------------------*/

static inline void lcd_putc_xy (ushort x, ushort y, uchar c)
{
#if defined(CONFIG_LCD_LOGO) && !defined(CONFIG_LCD_INFO_BELOW_LOGO)
	lcd_drawchars (x, y+BMP_LOGO_HEIGHT, &c, 1);
#else
	lcd_drawchars (x, y, &c, 1);
#endif
}

/************************************************************************/
/**  Small utility to check that you got the colours right		*/
/************************************************************************/
#ifdef LCD_TEST_PATTERN

#define	N_BLK_VERT	2
#define	N_BLK_HOR	3

static int test_colors[N_BLK_HOR*N_BLK_VERT] = {
	CONSOLE_COLOR_RED,	CONSOLE_COLOR_GREEN,	CONSOLE_COLOR_YELLOW,
	CONSOLE_COLOR_BLUE,	CONSOLE_COLOR_MAGENTA,	CONSOLE_COLOR_CYAN,
};

static void test_pattern (void)
{
	ushort v_max  = panel_info.vl_row;
	ushort h_max  = panel_info.vl_col;
	ushort v_step = (v_max + N_BLK_VERT - 1) / N_BLK_VERT;
	ushort h_step = (h_max + N_BLK_HOR  - 1) / N_BLK_HOR;
	ushort v, h;
	uchar *pix = (uchar *)lcd_base;

	printf ("[LCD] Test Pattern: %d x %d [%d x %d]\n",
		h_max, v_max, h_step, v_step);

	/* WARNING: Code silently assumes 8bit/pixel */
	for (v=0; v<v_max; ++v) {
		uchar iy = v / v_step;
		for (h=0; h<h_max; ++h) {
			uchar ix = N_BLK_HOR * iy + (h/h_step);
			*pix++ = test_colors[ix];
		}
	}
}
#endif /* LCD_TEST_PATTERN */

void lcd_disable (void)
{
 	S3C_VIDCON0 &= (~(S3C_VIDCON0_ENVID_ENABLE | S3C_VIDCON0_ENVID_F_ENABLE));
}

void lcd_enable (void)
{
	S3C_VIDCON0 |= (S3C_VIDCON0_ENVID_ENABLE | S3C_VIDCON0_ENVID_F_ENABLE);
}

void lcd_panel_disable(void)
{
	MIFPCON_REG |= SEL_BYPASS_MASK;
}

/************************************************************************/
/* ** GENERIC Initialization Routines					*/
/************************************************************************/

int drv_lcd_init (void)
{
	device_t lcddev;
	int rc;

	lcd_base = (void *)(gd->fb_base);
	osd_frame_buffer=(void*)((char*)lcd_base + calc_fbsize() * 1);

	lcd_line_length = (panel_info.vl_col * /*NBITS*/ (panel_info.vl_bpix)) / 8;

#if LCD_BPP == LCD_COLOR32
    lcd_color_fg = 0xFFFFFF;
    lcd_color_bg = 0x000000;
#else
    lcd_color_fg = 0xFFFF;
    lcd_color_bg = 0x0000;
#endif

	lcd_init (lcd_base);		/* LCD initialization */

#ifdef CONFIG_NO_VIDEO_CONSOLE
    return 0;
#endif

	/* Device initialization */
	memset (&lcddev, 0, sizeof (lcddev));

	strcpy (lcddev.name, "lcd");
	lcddev.ext   = 0;			/* No extensions */
	lcddev.flags = DEV_FLAGS_OUTPUT;	/* Output only */
	lcddev.putc  = lcd_putc;		/* 'putc' function */
	lcddev.puts  = lcd_puts;		/* 'puts' function */

	rc = device_register (&lcddev);
	return (rc == 0) ? 1 : rc;
}

/*----------------------------------------------------------------------*/
static int lcd_clear (cmd_tbl_t * cmdtp, int flag, int argc, char *argv[])
{
#if LCD_BPP == LCD_MONOCHROME
	/* Setting the palette */
	lcd_initcolregs();

#elif LCD_BPP == LCD_COLOR8
	/* Setting the palette */
	lcd_setcolreg  (CONSOLE_COLOR_BLACK,       0,    0,    0);
	lcd_setcolreg  (CONSOLE_COLOR_RED,	0xFF,    0,    0);
	lcd_setcolreg  (CONSOLE_COLOR_GREEN,       0, 0xFF,    0);
	lcd_setcolreg  (CONSOLE_COLOR_YELLOW,	0xFF, 0xFF,    0);
	lcd_setcolreg  (CONSOLE_COLOR_BLUE,        0,    0, 0xFF);
	lcd_setcolreg  (CONSOLE_COLOR_MAGENTA,	0xFF,    0, 0xFF);
	lcd_setcolreg  (CONSOLE_COLOR_CYAN,	   0, 0xFF, 0xFF);
	lcd_setcolreg  (CONSOLE_COLOR_GREY,	0xAA, 0xAA, 0xAA);
	lcd_setcolreg  (CONSOLE_COLOR_WHITE,	0xFF, 0xFF, 0xFF);
#endif

#ifndef CFG_WHITE_ON_BLACK
	lcd_setfgcolor (CONSOLE_COLOR_BLACK);
	lcd_setbgcolor (CONSOLE_COLOR_WHITE);
#else
	lcd_setfgcolor (CONSOLE_COLOR_WHITE);
	lcd_setbgcolor (CONSOLE_COLOR_BLACK);
#endif	/* CFG_WHITE_ON_BLACK */

#ifdef	LCD_TEST_PATTERN
	test_pattern();
#else
	/* set framebuffer to background color */
	memset ((char *)lcd_base,
		COLOR_MASK(lcd_getbgcolor()),
		lcd_line_length*panel_info.vl_row);
#endif
	/* Paint the logo and retrieve LCD base address */
	debug ("[LCD] Drawing the logo...\n");
	lcd_console_address = lcd_logo ();

	console_col = 0;
	console_row = 0;

	return (0);
}

U_BOOT_CMD(
	cls,	1,	1,	lcd_clear,
	"cls     - clear screen\n",
	NULL
);

/*----------------------------------------------------------------------*/

static int lcd_init (void *lcdbase)
{
	/* Initialize the lcd controller */
	debug ("[LCD] Initializing LCD frambuffer at %p\n", lcdbase);

	lcd_ctrl_init (lcdbase);
//	lcd_clear (NULL, 1, 1, NULL);	/* dummy args */
//	lcd_enable ();

	/* Initialize the console */
	console_col = 0;
#ifdef CONFIG_LCD_INFO_BELOW_LOGO
	console_row = 7 + BMP_LOGO_HEIGHT / VIDEO_FONT_HEIGHT;
#else
	console_row = 1;	/* leave 1 blank line below logo */
#endif
	lcd_is_enabled = 1;

	return 0;
}


/************************************************************************/
/* ** ROM capable initialization part - needed to reserve FB memory	*/
/************************************************************************/
/*
 * This is called early in the system initialization to grab memory
 * for the LCD controller.
 * Returns new address for monitor, after reserving LCD buffer memory
 *
 * Note that this is running from ROM, so no write access to global data.
 */
ulong lcd_setmem (ulong addr)
{
	ulong size;
	int line_length = (panel_info.vl_col * NBITS (panel_info.vl_bpix)) / 8;

	debug ("LCD panel info: %d x %d, %d bit/pix\n",panel_info.vl_col, panel_info.vl_row, NBITS (panel_info.vl_bpix) );

	size = line_length * panel_info.vl_row;

	/* Round up to nearest full page */
	size = (size + (PAGE_SIZE - 1)) & ~(PAGE_SIZE - 1);

	/* Allocate pages for the frame buffer. */
	addr -= size;

	debug ("Reserving %ldk for LCD Framebuffer at: %08lx\n", size>>10, addr);

	return (addr);
}

/*----------------------------------------------------------------------*/

static void lcd_setfgcolor (int color)
{
	lcd_color_fg = color;
}

/*----------------------------------------------------------------------*/

static void lcd_setbgcolor (int color)
{
	lcd_color_bg = color;
}

/*----------------------------------------------------------------------*/

#ifdef	NOT_USED_SO_FAR
static int lcd_getfgcolor (void)
{
	return lcd_color_fg;
}
#endif	/* NOT_USED_SO_FAR */

/*----------------------------------------------------------------------*/

static int lcd_getbgcolor (void)
{
	return lcd_color_bg;
}

/*----------------------------------------------------------------------*/

/************************************************************************/
/* ** Chipset depending Bitmap / Logo stuff...                          */
/************************************************************************/
#ifdef CONFIG_LCD_LOGO
void bitmap_plot (int x, int y)
{
	ushort *cmap;
	ushort i, j;
	uchar *bmap;
	uchar *fb;
	ushort *fb16;
#if defined(CONFIG_PXA250)
	struct pxafb_info *fbi = &panel_info.pxa;
#elif defined(CONFIG_MPC823)
	volatile immap_t *immr = (immap_t *) CFG_IMMR;
	volatile cpm8xx_t *cp = &(immr->im_cpm);
#endif

	debug ("Logo: width %d  height %d  colors %d  cmap %d\n",
		BMP_LOGO_WIDTH, BMP_LOGO_HEIGHT, BMP_LOGO_COLORS,
		sizeof(bmp_logo_palette)/(sizeof(ushort)));

	bmap = &bmp_logo_bitmap[0];
	fb   = (uchar *)(lcd_base + y * lcd_line_length + x);

	if (NBITS(panel_info.vl_bpix) < 12) {
		/* Leave room for default color map */
#if defined(CONFIG_PXA250)
		cmap = (ushort *)fbi->palette;
#elif defined(CONFIG_MPC823)
		cmap = (ushort *)&(cp->lcd_cmap[BMP_LOGO_OFFSET*sizeof(ushort)]);
#endif

		WATCHDOG_RESET();

		/* Set color map */
		for (i=0; i<(sizeof(bmp_logo_palette)/(sizeof(ushort))); ++i) {
			ushort colreg = bmp_logo_palette[i];
#ifdef  CFG_INVERT_COLORS
			*cmap++ = 0xffff - colreg;
#else
			*cmap++ = colreg;
#endif
		}

		WATCHDOG_RESET();

		for (i=0; i<BMP_LOGO_HEIGHT; ++i) {
			memcpy (fb, bmap, BMP_LOGO_WIDTH);
			bmap += BMP_LOGO_WIDTH;
			fb   += panel_info.vl_col;
		}
	}
	else { /* true color mode */
		fb16 = (ushort *)(lcd_base + y * lcd_line_length + x);
		for (i=0; i<BMP_LOGO_HEIGHT; ++i) {
			for (j=0; j<BMP_LOGO_WIDTH; j++) {
				fb16[j] = bmp_logo_palette[(bmap[j])];
				}
			bmap += BMP_LOGO_WIDTH;
			fb16 += panel_info.vl_col;
		}
	}

	WATCHDOG_RESET();
}
#endif /* CONFIG_LCD_LOGO */

/*----------------------------------------------------------------------*/
#if (CONFIG_COMMANDS & CFG_CMD_BMP) || defined(CONFIG_SPLASH_SCREEN)
/*
 * Display the BMP file located at address bmp_image.
 * Only uncompressed.
 */
int lcd_display_bitmap(ulong bmp_image, int x, int y)
{
#if !defined(CONFIG_MCC200)
	ushort *cmap;
#endif
	ushort i, j;
	uchar *fb;
	bmp_image_t *bmp=(bmp_image_t *)bmp_image;
	uchar *bmap;
	ushort padded_line;
	unsigned long width, height;
	unsigned long pwidth = panel_info.vl_col;
	unsigned colors,bpix;
	unsigned long compression;
#if defined(CONFIG_PXA250)
	struct pxafb_info *fbi = &panel_info.pxa;
#elif defined(CONFIG_MPC823)
	volatile immap_t *immr = (immap_t *) CFG_IMMR;
	volatile cpm8xx_t *cp = &(immr->im_cpm);
#endif

	if (!((bmp->header.signature[0]=='B') &&
		(bmp->header.signature[1]=='M'))) {
		printf ("Error: no valid bmp image at %lx\n", bmp_image);
		return 1;
}

	width = le32_to_cpu (bmp->header.width);
	height = le32_to_cpu (bmp->header.height);
	colors = 1<<le16_to_cpu (bmp->header.bit_count);
	compression = le32_to_cpu (bmp->header.compression);

	bpix = NBITS(panel_info.vl_bpix);

	if ((bpix != 1) && (bpix != 8)) {
		printf ("Error: %d bit/pixel mode not supported by U-Boot\n",
			bpix);
		return 1;
	}

	if (bpix != le16_to_cpu(bmp->header.bit_count)) {
		printf ("Error: %d bit/pixel mode, but BMP has %d bit/pixel\n",
			bpix,
			le16_to_cpu(bmp->header.bit_count));
		return 1;
	}

	debug ("Display-bmp: %d x %d  with %d colors\n",
		(int)width, (int)height, (int)colors);

#if !defined(CONFIG_MCC200)
	/* MCC200 LCD doesn't need CMAP, supports 1bpp b&w only */
	if (bpix==8) {
#if defined(CONFIG_PXA250)
		cmap = (ushort *)fbi->palette;
#elif defined(CONFIG_MPC823)
		cmap = (ushort *)&(cp->lcd_cmap[255*sizeof(ushort)]);
#else
# error "Don't know location of color map"
#endif

		/* Set color map */
		for (i=0; i<colors; ++i) {
			bmp_color_table_entry_t cte = bmp->color_table[i];
			ushort colreg =
				( ((cte.red)   << 8) & 0xf800) |
				( ((cte.green) << 3) & 0x07e0) |
				( ((cte.blue)  >> 3) & 0x001f) ;
#ifdef CFG_INVERT_COLORS
			*cmap = 0xffff - colreg;
#else
			*cmap = colreg;
#endif
#if defined(CONFIG_PXA250)
			cmap++;
#elif defined(CONFIG_MPC823)
			cmap--;
#endif
		}
	}
#endif

	/*
	 *  BMP format for Monochrome assumes that the state of a
	 * pixel is described on a per Bit basis, not per Byte.
	 *  So, in case of Monochrome BMP we should align widths
	 * on a byte boundary and convert them from Bit to Byte
	 * units.
	 *  Probably, PXA250 and MPC823 process 1bpp BMP images in
	 * their own ways, so make the converting to be MCC200
	 * specific.
	 */
#if defined(CONFIG_MCC200)
	if (bpix==1)
	{
		width = ((width + 7) & ~7) >> 3;
		x     = ((x + 7) & ~7) >> 3;
		pwidth= ((pwidth + 7) & ~7) >> 3;
	}
#endif

	padded_line = (width&0x3) ? ((width&~0x3)+4) : (width);
	if ((x + width)>pwidth)
		width = pwidth - x;
	if ((y + height)>panel_info.vl_row)
		height = panel_info.vl_row - y;

	bmap = (uchar *)bmp + le32_to_cpu (bmp->header.data_offset);
	fb   = (uchar *) (lcd_base +
		(y + height - 1) * lcd_line_length + x);
	for (i = 0; i < height; ++i) {
		WATCHDOG_RESET();
		for (j = 0; j < width ; j++)
#if defined(CONFIG_PXA250)
			*(fb++)=*(bmap++);
#elif defined(CONFIG_MPC823) || defined(CONFIG_MCC200)
			*(fb++)=255-*(bmap++);
#endif
		bmap += (width - padded_line);
		fb   -= (width + lcd_line_length);
	}

	return (0);
}
#endif /* (CONFIG_COMMANDS & CFG_CMD_BMP) || CONFIG_SPLASH_SCREEN */


static void *lcd_logo (void)
{
#ifdef CONFIG_LCD_INFO
	char info[80];
	char temp[32];
#endif /* CONFIG_LCD_INFO */

#ifdef CONFIG_SPLASH_SCREEN
	char *s;
	ulong addr;
	static int do_splash = 1;

	if (do_splash && (s = getenv("splashimage")) != NULL) {
		addr = simple_strtoul(s, NULL, 16);
		do_splash = 0;

		if (lcd_display_bitmap (addr, 0, 0) == 0) {
			return ((void *)lcd_base);
		}
	}
#endif /* CONFIG_SPLASH_SCREEN */

#ifdef CONFIG_LCD_LOGO
	bitmap_plot (0, 0);
#endif /* CONFIG_LCD_LOGO */

#ifdef CONFIG_MPC823
# ifdef CONFIG_LCD_INFO
	sprintf (info, "%s (%s - %s) ", U_BOOT_VERSION, __DATE__, __TIME__);
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y, (uchar *)info, strlen(info));

	sprintf (info, "(C) 2004 DENX Software Engineering");
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y + VIDEO_FONT_HEIGHT,
					(uchar *)info, strlen(info));

	sprintf (info, "    Wolfgang DENK, wd@denx.de");
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y + VIDEO_FONT_HEIGHT * 2,
					(uchar *)info, strlen(info));
#  ifdef CONFIG_LCD_INFO_BELOW_LOGO
	sprintf (info, "MPC823 CPU at %s MHz",
		strmhz(temp, gd->cpu_clk));
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y + VIDEO_FONT_HEIGHT * 3,
					info, strlen(info));
	sprintf (info, "  %ld MB RAM, %ld MB Flash",
		gd->ram_size >> 20,
		gd->bd->bi_flashsize >> 20 );
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y + VIDEO_FONT_HEIGHT * 4,
					info, strlen(info));
#  else
	/* leave one blank line */

	sprintf (info, "MPC823 CPU at %s MHz, %ld MB RAM, %ld MB Flash",
		strmhz(temp, gd->cpu_clk),
		gd->ram_size >> 20,
		gd->bd->bi_flashsize >> 20 );
	lcd_drawchars (LCD_INFO_X, LCD_INFO_Y + VIDEO_FONT_HEIGHT * 4,
					(uchar *)info, strlen(info));

#  endif /* CONFIG_LCD_INFO_BELOW_LOGO */
# endif /* CONFIG_LCD_INFO */
#endif /* CONFIG_MPC823 */

#if defined(CONFIG_LCD_LOGO) && !defined(CONFIG_LCD_INFO_BELOW_LOGO)
	return ((void *)((ulong)lcd_base + BMP_LOGO_HEIGHT * lcd_line_length));
#else
	return ((void *)lcd_base);
#endif /* CONFIG_LCD_LOGO && !CONFIG_LCD_INFO_BELOW_LOGO */
}

ulong calc_fbsize (void)
{
	ulong size;

	int line_length = (panel_info.vl_col * panel_info.vl_bpix) / 8;
	size = line_length * panel_info.vl_row;

	return size;
}

void lcd_ctrl_init(void *lcdbase)
{
	ulong freq_lcdclk;
	ulong freq_Hclk;
	ulong fb_size;
	unsigned char nn;
	unsigned short *pp;
	int i;

	GPICON_REG = 0xaaaaaaaa;
	GPIPUD_REG = 0xaaaaaaaa;
	GPJCON_REG = 0xaaaaaaaa;
	GPJPUD_REG = 0xaaaaaaaa;

	lcd_disable();
	S3C_WINCON0 &= (~(S3C_WINCONx_ENWIN_F_ENABLE));

	MIFPCON_REG &= (~SEL_BYPASS_MASK);
	SPCON_REG &= (~LCD_SEL_MASK);
	SPCON_REG |= (RGB_IF_STYLE_MASK);

	freq_lcdclk = S3CFB_PIXEL_CLOCK;
	freq_Hclk = get_HCLK();

	nn = (unsigned char)(freq_Hclk / freq_lcdclk) - 1;
	if(freq_lcdclk < freq_Hclk/2)
	{
		S3C_VIDCON0 = S3C_VIDCON0_INTERLACE_F_PROGRESSIVE + S3C_VIDCON0_VIDOUT_RGB_IF + \
		        S3C_VIDCON0_PNRMODE_RGB_P + S3C_VIDCON0_CLKVALUP_ST_FRM + S3C_VIDCON0_CLKVAL_F(nn) + \
		        S3C_VIDCON0_CLKDIR_DIVIDED + S3C_VIDCON0_CLKSEL_F_HCLK;
	}else
	{
		S3C_VIDCON0 = S3C_VIDCON0_INTERLACE_F_PROGRESSIVE + S3C_VIDCON0_VIDOUT_RGB_IF + \
		        S3C_VIDCON0_PNRMODE_RGB_P + S3C_VIDCON0_CLKVALUP_ST_FRM + S3C_VIDCON0_CLKVAL_F(0) + \
		        S3C_VIDCON0_CLKDIR_DIRECTED  + S3C_VIDCON0_CLKSEL_F_HCLK;
	}

	nn = 0;
	if(S3CFB_IVCLK)
	{
		nn += S3C_VIDCON1_IVCLK_RISE_EDGE;
	}
	if(S3CFB_IHSYNC)
	{
		nn += S3C_VIDCON1_IHSYNC_INVERT;
	}
	if(S3CFB_IVSYNC)
	{
		nn += S3C_VIDCON1_IVSYNC_INVERT;
	}
	if(S3CFB_IVDEN)
	{
		nn += S3C_VIDCON1_IVDEN_INVERT;
	}
	S3C_VIDCON1 = (unsigned int)nn;
	S3C_VIDCON2 = 0;

	S3C_VIDTCON0 = S3C_VIDTCON0_VBPD(S3CFB_VBP - 1) | S3C_VIDTCON0_VFPD(S3CFB_VFP - 1) | S3C_VIDTCON0_VSPW(S3CFB_VSW - 1);
	S3C_VIDTCON1 = S3C_VIDTCON1_HBPD(S3CFB_HBP - 1) | S3C_VIDTCON1_HFPD(S3CFB_HFP -1) | S3C_VIDTCON1_HSPW(S3CFB_HSW - 1);
	S3C_VIDTCON2 = S3C_VIDTCON2_LINEVAL(S3CFB_VRES - 1) | S3C_VIDTCON2_HOZVAL(S3CFB_HRES - 1);

#if LCD_BPP == LCD_COLOR32
	S3C_WINCON0 = S3C_WINCONx_BPPMODE_F_24BPP_888;
	S3C_WINCON1 = S3C_WINCONx_BPPMODE_F_24BPP_888 | S3C_WINCONx_BLD_PIX_PIXEL;
#else
	S3C_WINCON0 = S3C_WINCONx_BPPMODE_F_16BPP_565 | S3C_WINCONx_HAWSWP_ENABLE;
	S3C_WINCON1 = S3C_WINCONx_BPPMODE_F_16BPP_565 | S3C_WINCONx_HAWSWP_ENABLE | S3C_WINCONx_BLD_PIX_PIXEL;
#endif

	S3C_WINCON2 = 0;
	S3C_WINCON3 = 0;
	S3C_WINCON4 = 0;

	S3C_VIDOSD0A = S3C_VIDOSDxA_OSD_LTX_F(0) + S3C_VIDOSDxA_OSD_LTY_F(0);
	S3C_VIDOSD0B = S3C_VIDOSDxB_OSD_RBX_F(S3CFB_HRES - 1) | S3C_VIDOSDxB_OSD_RBY_F(S3CFB_VRES - 1);
	S3C_VIDOSD0C = S3C_VIDOSD0C_OSDSIZE(S3CFB_HRES*S3CFB_VRES);

	S3C_VIDOSD1A = S3C_VIDOSDxA_OSD_LTX_F(0) + S3C_VIDOSDxA_OSD_LTY_F(0);
	S3C_VIDOSD1B = S3C_VIDOSDxB_OSD_RBX_F(S3CFB_HRES - 1) | S3C_VIDOSDxB_OSD_RBY_F(S3CFB_VRES - 1);
	S3C_VIDOSD1C = 0xDDD000;/*alpha blending*/
	S3C_VIDOSD1D = S3C_VIDOSD0C_OSDSIZE(S3CFB_HRES*S3CFB_VRES);

	S3C_VIDOSD2A = 0;
	S3C_VIDOSD2B = 0;
	S3C_VIDOSD2C = 0;
	S3C_VIDOSD2D = 0;
	S3C_VIDOSD3A = 0;
	S3C_VIDOSD3B = 0;
	S3C_VIDOSD3C = 0;
	S3C_VIDOSD4A = 0;
	S3C_VIDOSD4B = 0;
	S3C_VIDOSD4C = 0;

	fb_size = calc_fbsize();

	S3C_VIDW00ADD0B0 = virt_to_phys(lcdbase);
	S3C_VIDW00ADD0B1 = 0;
	S3C_VIDW01ADD0B0 = virt_to_phys(osd_frame_buffer);
	S3C_VIDW01ADD0B1 = 0;
	S3C_VIDW02ADD0 = 0;
	S3C_VIDW03ADD0 = 0;
	S3C_VIDW04ADD0 = 0;

	S3C_VIDW00ADD1B0 = virt_to_phys((unsigned int)(lcdbase) + fb_size);
	S3C_VIDW00ADD1B1 = 0;
	S3C_VIDW01ADD1B0 = virt_to_phys(osd_frame_buffer) + fb_size;
	S3C_VIDW01ADD1B1 = 0;
	S3C_VIDW02ADD1 = 0;
	S3C_VIDW03ADD1 = 0;
	S3C_VIDW04ADD1 = 0;

	S3C_VIDW00ADD2 = 0;//S3C_VIDWxxADD2_OFFSIZE_F(0) | (S3C_VIDWxxADD2_PAGEWIDTH_F(panel_info.vl_col*panel_info.vl_bpix/8));
	S3C_VIDW01ADD2 = 0;//S3C_VIDWxxADD2_OFFSIZE_F(0) | (S3C_VIDWxxADD2_PAGEWIDTH_F(panel_info.vl_col*panel_info.vl_bpix/8));
	S3C_VIDW02ADD2 = 0;
	S3C_VIDW03ADD2 = 0;
	S3C_VIDW04ADD2 = 0;

	S3C_W1KEYCON0  = S3C_WxKEYCON0_KEYBLEN_ENABLE | S3C_WxKEYCON0_KEYEN_F_ENABLE | S3C_WxKEYCON0_COMPKEY(0xFFFF);
	S3C_W1KEYCON1  = 0x00000000;/*color key*/

#if 1
	memset(lcdbase,0x00,fb_size*2);
#else
	pp = lcdbase;
	for(i=0;i< S3CFB_HRES * S3CFB_VRES;i++)
	{
		*pp = 0xf100;
		pp++;
	}
#endif
	lcd_enable();

	S3C_WINCON0 |= S3C_WINCONx_ENWIN_F_ENABLE;
	S3C_WINCON1 |= S3C_WINCONx_ENWIN_F_ENABLE;
	return (0);
}

lcd_setcolreg (ushort regno, ushort red, ushort green, ushort blue)
{
//	volatile immap_t *immr = (immap_t *) CFG_IMMR;
//	volatile cpm8xx_t *cp = &(immr->im_cpm);
//	unsigned short colreg, *cmap_ptr;
//
//	cmap_ptr = (unsigned short *)&cp->lcd_cmap[regno * 2];
//
//	colreg = ((red   & 0x0F) << 8) |
//		 ((green & 0x0F) << 4) |
//		  (blue  & 0x0F) ;
//#ifdef	CFG_INVERT_COLORS
//	colreg ^= 0x0FFF;
//#endif
//	*cmap_ptr = colreg;
//
//	debug ("setcolreg: reg %2d @ %p: R=%02X G=%02X B=%02X => %02X%02X\n",
//		regno, &(cp->lcd_cmap[regno * 2]),
//		red, green, blue,
//		cp->lcd_cmap[ regno * 2 ], cp->lcd_cmap[(regno * 2) + 1]);
}

/************************************************************************/
/************************************************************************/

#endif /* CONFIG_LCD */
