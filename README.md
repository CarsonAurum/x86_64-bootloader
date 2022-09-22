# x86_64 BootLoader

Most guides attempting to help guide users through the process of enabling a 64-bit 
operating mode on x86/AMD64 platforms often requires the use of protected 32-bit 
mode as an intermediate to facilitate the initialization of the C runtime *before*
moving to a 64-bit operating environment.

Conversely, this simple homegrown bootloader skips this process by immediately disabling
the memory paging function that is signature for protected 32-bit mode and proceeding 
directly to enable a long mode linear memory mapping.

This bootloader prepares the C runtime environment and calls a function named `kernel_main`
that should be defined by the user in C, or `extern "C"` if in C++.
