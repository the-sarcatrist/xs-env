/*
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2020 Western Digital Corporation or its affiliates.
 *
 * Authors:
 *   Anup Patel <anup.patel@wdc.com>
 */

#include <sbi/sbi_error.h>
#include <sbi/sbi_scratch.h>
#include <sbi_utils/fdt/fdt_helper.h>
#include <sbi_utils/ipi/fdt_ipi.h>

/* List of FDT ipi drivers generated at compile time */
extern struct fdt_ipi *fdt_ipi_drivers[];
extern unsigned long fdt_ipi_drivers_size;

int fdt_ipi_init(void)
{
	int pos, noff, rc;
	struct fdt_ipi *drv;
	const struct fdt_match *match;
	const void *fdt = fdt_get_address();

	for (pos = 0; pos < fdt_ipi_drivers_size; pos++) {
		drv = fdt_ipi_drivers[pos];

		noff = -1;
		while ((noff = fdt_find_match(fdt, noff,
					drv->match_table, &match)) >= 0) {
			if (!fdt_node_is_enabled(fdt, noff))
				continue;

			/* drv->cold_init must not be NULL */
			if (drv->cold_init == NULL)
				return SBI_EFAIL;

			rc = drv->cold_init(fdt, noff, match);
			if (rc == SBI_ENODEV)
				continue;
			if (rc)
				return rc;

			/*
			 * We will have multiple IPI devices on multi-die or
			 * multi-socket systems so we cannot break here.
			 */
		}
	}

	/*
	 * On some single-hart system there is no need for ipi,
	 * so we cannot return a failure here
	 */
	return 0;
}