/*
 * $Id: nand_cp.c,v 1.4 2008/04/08 00:41:40 jsgood Exp $
 *
 * (C) Copyright 2006 Samsung Electronics
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

/*
 * You must make sure that all functions in this file are designed
 * to load only U-Boot image.
 *
 * So, DO NOT USE in common read.
 *
 * By scsuh.
 */


#include <common.h>

#ifdef CONFIG_S3C64XX
#include <asm/io.h>
#include <linux/mtd/nand.h>
#include <regs.h>

/*
 * address format
 *              17 16         9 8            0
 * --------------------------------------------
 * | block(12bit) | page(5bit) | offset(9bit) |
 * --------------------------------------------
 */

static int nandll_read_page (uchar *buf, ulong addr, int large_block)
{
    int i;
    int page_size = 512;

    if (large_block==1)
        page_size = 2048;
    if (large_block==2)
        page_size = 4096;
    if(large_block==3)
        page_size = 8192;

    NAND_ENABLE_CE();

    NFCMD_REG = NAND_CMD_READ0;

    /* Write Address */
    NFADDR_REG = 0;

    if (large_block)
        NFADDR_REG = 0;

    NFADDR_REG = (addr) & 0xff;
    NFADDR_REG = (addr >> 8) & 0xff;
    NFADDR_REG = (addr >> 16) & 0xff;

    if (large_block)
        NFCMD_REG = NAND_CMD_READSTART;

    NF_TRANSRnB();

    /* for compatibility(2460). u32 cannot be used. by scsuh */
    for(i=0; i < page_size; i++) {
        *buf++ = NFDATA8_REG;
    }

    NAND_DISABLE_CE();
    return 0;
}

/*
 * Read data from NAND.
 */
static int nandll_read_blocks (ulong dst_addr, ulong size, int large_block)
{
    uchar *buf = (uchar *)dst_addr;
    int i;
    uint page_shift = 9;

    if (large_block==1)
        page_shift = 11;

    if(large_block==2)
        page_shift = 12;

    if(large_block==3)
        page_shift =13;

    if(large_block == 2)
    {
        /* Read pages */
        for (i = 0; i < 4; i++, buf+=(1<<(page_shift-1))) {
            nandll_read_page(buf, i, large_block);
        }


        /* Read pages */
        for (i = 4; i < (0x3c000>>page_shift); i++, buf+=(1<<page_shift)) {
            nandll_read_page(buf, i, large_block);
        }

    }else if(large_block == 3)  //K9GAG08U0E
    {
        /* Read pages */
        for (i = 0; i < 4; i++, buf+=(1<<(page_shift-2))) {
            nandll_read_page(buf, i, large_block);
        }


        /* Read pages */
        for (i = 4; i < (0x3c000>>page_shift); i++, buf+=(1<<page_shift)) {
            nandll_read_page(buf, i, large_block);
        }
    }
    else
    {
        for (i = 0; i < (0x3c000>>page_shift); i++, buf+=(1<<page_shift)) {
            nandll_read_page(buf, i, large_block);
        }
    }
    return 0;
}

int copy_uboot_to_ram (void)
{


    int large_block = 0;
    int i;
    vu_char id;

    NAND_ENABLE_CE();
    NFCMD_REG=NAND_CMD_RESET;
    NF_TRANSRnB();


    NFCMD_REG = NAND_CMD_READID;
    NFADDR_REG =  0x00;

    NF_TRANSRnB();

    /* wait for a while */
    for (i=0; i<200; i++);

    int factory = NFDATA8_REG;
    id = NFDATA8_REG;

    int cellinfo=NFDATA8_REG;
    int tmp= NFDATA8_REG;

    //int childType=tmp & 0x03; //Page size
    int childType=cellinfo; //Page size

    if (id > 0x80)
    {
        large_block = 1;
    }

    if(id == 0xd5 && childType==0x94 )//K9GAG08U0D
    {
        large_block = 2;

    }
    if(id == 0xd5 && childType==0x14 )//K9GAG08U0M
    {
        large_block = 2;

    }
    if(id == 0xd5 && childType==0x84 )//K9GAG08U0E
    {
        large_block = 3;

    }
    if(id==0xd7)//K9LBG08U0D
    {
        large_block = 2;
    }
    if(factory==0x2c && id == 0x48) //MT29F16G08ABACAWP
    {
        large_block = 2;

    }if(factory==0x2c && id == 0x38) //MT29F8G08ABABAWP
    {
        large_block = 2;

    }


    /* read NAND Block.
     * 128KB ->240KB because of U-Boot size increase. by scsuh
     * So, read 0x3c000 bytes not 0x20000(128KB).
     */
    return nandll_read_blocks(CFG_PHY_UBOOT_BASE, 0x3c000, large_block);
}


#if 1

#define REG_GPFCON       (0x7F0080A0)
#define REG_GPFDAT       (0x7F0080A4)

void beep(void)
{

    uint reg_f_cfg=readl(REG_GPFCON) & 0x3FFFFFFF | (1<<30);
    writel(reg_f_cfg,REG_GPFCON);

    uint reg_f_on=readl(REG_GPFDAT) & 0xFFFF7FFF | (1<<15);
    uint reg_f_off=readl(REG_GPFDAT) & 0xFFFF7FFF;

    writel(reg_f_on,REG_GPFDAT);
}
#else
void  beep(void)
{

}
#endif

#endif

