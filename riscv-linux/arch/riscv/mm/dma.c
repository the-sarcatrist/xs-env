#include <linux/dma-mapping.h>
#include <linux/platform_device.h>

void arch_setup_pdev_archdata(struct platform_device *pdev)
{
	if (pdev->dev.coherent_dma_mask == DMA_MASK_NONE &&
	    pdev->dev.dma_mask == NULL) {
		pdev->dev.coherent_dma_mask = DMA_BIT_MASK(64);
		pdev->dev.dma_mask = &pdev->dev.coherent_dma_mask;
	}
}
