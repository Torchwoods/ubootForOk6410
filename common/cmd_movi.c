#include <common.h>

#ifdef CONFIG_MOVINAND

#include <command.h>
#include <movi.h>

struct movi_offset_t ofsinfo;

#if 0
int do_testhsmmc(cmd_tbl_t *cmdtp, int flag, int argc, char *argv[])
{
	uint start_blk, blknum;
	start_blk = simple_strtoul(argv[1], NULL, 10);
	blknum = simple_strtoul(argv[2], NULL, 10);

	if(argc < 3)
		return 0;
	
	printf("COMMAND --> start_blk = 0x%x, blknum = %d\n", start_blk, blknum);	
	test_hsmmc(4, 1, start_blk, blknum);
	return 1;
}

U_BOOT_CMD(
	testhsmmc,	4,	0,	do_testhsmmc,
	"testhsmmc - hsmmc testing write/read\n",
	NULL
);
#endif

int do_movi(cmd_tbl_t * cmdtp, int flag, int argc, char *argv[])
{
	char *cmd;
	ulong addr, blk;
	uint last_blkpos, bytes;

	cmd = argv[1];	

	if (strcmp(cmd, "init") == 0) {
		if (argc < 4)
			goto usage;
		
		last_blkpos = (uint) simple_strtoul(argv[2], NULL, 10);
		movi_hc = (uint) simple_strtoul(argv[3], NULL, 10);

		if (movi_set_ofs(last_blkpos))
			movi_init();
	} else {
		if (argc == 4)
			addr = (ulong)simple_strtoul(argv[3], NULL, 16);
		else if (argc == 5)
			addr = (ulong)simple_strtoul(argv[4], NULL, 16);
		else
			goto usage;

		if (addr >= 0xc0000000)
			addr = virt_to_phys(addr);

		if (ofsinfo.last == 0) {
			printf("### Execute 'movi init' command first!\n");
			return -1;
		}
		
		if (strcmp(cmd, "read") == 0) {
			if (argc == 4) {
				if (strcmp(argv[2], "u-boot") == 0) {
					printf("Reading bootloader.. ");
					movi_read((uint) addr, ofsinfo.bl2, MOVI_BL2_BLKCNT);
					printf("completed\n");
				} else if (strcmp(argv[2], "kernel") == 0) {
					printf("Reading kernel from sector %d (%d sectors).. ", ofsinfo.kernel, MOVI_ZIMAGE_BLKCNT);
					movi_read((uint) addr, ofsinfo.kernel, MOVI_ZIMAGE_BLKCNT);
					printf("completed\n");
				} else if (strcmp(argv[2], "rootfs") == 0) {
					printf("Reading rootfs from sector %d (%d sectors).. ", ofsinfo.rootfs, MOVI_ROOTFS_BLKCNT);
					movi_read((uint) addr, ofsinfo.rootfs, MOVI_ROOTFS_BLKCNT);
					printf("completed\n");
				} else
					goto usage;
			} else {
				blk = (uint) simple_strtoul(argv[2], NULL, 10);
				bytes = (uint) simple_strtoul(argv[3], NULL, 16);

				if (blk >= 0 && blk <= ofsinfo.last) {
					printf("Reading data from sector %d (%d sectors).. ", blk, (int) (bytes / MOVI_BLKSIZE));
					movi_read((uint) addr, blk, (int) (bytes / MOVI_BLKSIZE));
					printf("completed\n");
				} else
					goto usage;
			}
		} else if (strcmp(cmd, "write") == 0) {
			if (argc == 4) {
				if (strcmp(argv[2], "u-boot") == 0) {
					printf("Writing 1st bootloader to sector %d (%d sectors).. ", ofsinfo.bl1, MOVI_BL1_BLKCNT);
					movi_write((uint) addr, ofsinfo.bl1, MOVI_BL1_BLKCNT);
					printf("completed\nWriting 2nd bootloader to sector %d (%d sectors).. ", ofsinfo.bl2, MOVI_BL2_BLKCNT);
					movi_write((uint) addr, ofsinfo.bl2, MOVI_BL2_BLKCNT);
					printf("completed\n");
				} else if (strcmp(argv[2], "kernel") == 0) {
					printf("Writing kernel to sector %d (%d sectors).. ", ofsinfo.kernel, MOVI_ZIMAGE_BLKCNT);
					movi_write((uint) addr, ofsinfo.kernel, MOVI_ZIMAGE_BLKCNT);
					printf("completed\n");
				} else if (strcmp(argv[2], "env") == 0) {
					printf("Writing env to sector %d (%d sectors).. ", ofsinfo.env, MOVI_ENV_BLKCNT);
					movi_write((uint) addr, ofsinfo.env, MOVI_ENV_BLKCNT);
					printf("completed\n");
				} else if (strcmp(argv[2], "rootfs") == 0) {
					printf("Writing rootfs to sector %d (%d sectors).. ", ofsinfo.rootfs, MOVI_ROOTFS_BLKCNT);
					movi_write((uint) addr, ofsinfo.rootfs, MOVI_ROOTFS_BLKCNT);
					printf("completed\n");
				} else
					goto usage;
			} else {
				blk = (uint) simple_strtoul(argv[2], NULL, 10);
				bytes = (uint) simple_strtoul(argv[3], NULL, 16);

				if (blk >= 0 && blk <= ofsinfo.last) {
					printf("Writing data to sector %d (%d sectors).. ", blk, (int) (bytes / MOVI_BLKSIZE));
					movi_write((uint) addr, blk, (int) (bytes / MOVI_BLKSIZE));
					printf("completed\n");
				} else
					goto usage;
			}
		} else {
			goto usage;
		}
	}

	return 1;

usage:
	printf("Usage:\n%s\n", cmdtp->usage);
	return -1;
}

U_BOOT_CMD(
	movi,	5,	0,	do_movi,
	"movi\t- moviNAND sub-system\n",
	"init {total sectors} {hc(0/1)} - Initialize moviNAND\n"
	"movi read  {u-boot | kernel | rootfs} {addr} - Read data from moviNAND\n"
	"movi write {u-boot | kernel | rootfs} {addr} - Write data to moviNAND\n"
	"movi read  {sector#} {bytes(hex)} {addr} - Read data from moviNAND sector#\n"
	"movi write {sector#} {bytes(hex)} {addr} - Write data to moviNAND sector#\n"
);

#endif /* (CONFIG_COMMANDS & CFG_CMD_MOVINAND) */
