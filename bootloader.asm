.include "constants.asm"

.code16
.section .boot
.global 	startup
.global 	init

startup:
	ljmp 0x0000, .flush_segments 	# Ensure segment registers are not modified by BIOS before use.

.flush_segments:
	cld				# Clear direction EFLAGS bit.
	mov sp, startup			# Prep the stack register to start and startup.
	xor ax, ax			# Zero out ax, and use it to empty other segment registers.
	mov ds, ax
	mov es, ax
	mov ss, ax

	movb [disk], dl			# Write disk number to disk.
	mov ax, 0x0241			# ah = read sector function, al = 65 (for 65 read disk sectors)
	# If using more than ~33k of disk space, another driver is required to load all the data.
	mov bx, 0x7E00			# es:bx = memory location to copy data into. es is already zeroed.
	mov cx, 0x0002			# ch = 0x00 (track), cl = 0x02 (sector)
	xor dh , dh			# dh = 0x00 (head), dl = drive number.
	int 0x13			# copy data from disk (read).
.check_cpuid:
	pushfd
	pop eax
	mov ecx, eax
	xor eax, (1 << 21)		# EFLAGS bit 21 indicates support for CPUID.
	push eax
	popfd
	pushfd
	pop eax
	push ecx
	popfd
	xor eax, ecx
	jz .no_cpuid			# Abort if cpuid operation isn't supported.
	
	mov eax, 0x80000000		# Flag for long mode.
	cpuid
	cmp eax, 0x80000001		# Check status
	jz .no_longmode			# Abort if cpuid finds no support for long mode.
	
	mov ax, 0x2401
	int 0x15			# Enable A20 gate (I/O addr bit #21)

	mov ax, 0x03
	int 0x10			# VGA mode 3: Get cursor position and shape.

.int15:
	mov eax, 0xE820			# Get memory map.	
	xor ebx, ebx			# Clear ebx
	mov ecx, 24			# Request 24 byte entries.
	mov edx, 0x0534D4150		# Magic number
	mov di, MEMORY_MAP_BUFFER	# Set di to second dword after end of page table.
	movb [di + 20], 0x01		# Force valid ACPI 3.X entry
	xor bp, bp			# Clear bp
	int 0x15			# Call interrupt
	jc .int15_failed		# Jump if carry is set.
	mov edx, 0x0534D4150		# Reset edx in case the interrupt changed edx.
	cmp eax, edx			# If success, they are equal.
	jne .int15_failed		
	test ebx, ebx			# If ebx is zero, list is 1 entry long.
	jz .int15_failed
	jmp .int15_cont

.int15_failed:
	cli
	hlt

.int15_loop:
	mov ax, 0xE820			# Fix ax from interrupt.
	movb [di + 20], 0x01		# Force valid ACPI entry.
	mov ecx, 24			# 24 bytes requested.
	int 0x15
	jc .int15_failed		# If carry is set, the end of the list is reached.
	mov edx, 0x0534D4150		# Reset edx in case the interrupt changed edx.

.int15_cont:
	jcxz .skip_entry		# Skip entry if length = 0
	cmp cl, 20			# If entry is 24 bytes, it's a valid ACPI entry.
	jbe .not_acpi
	testb [di + 20], 0x01		# 0x01 = ignore data flag
	jz .skip_entry

.not_acpi:
	mov ecx, [di + 8]		# Get lower uint32_t of memory region length
	or exc, [di + 12]		# OR it with the upper half to test for zero.
	jz .skip_entry			# If length = 0, skip.
	inc bp				# Increment the counter.
	add di, 24			# set the pointer to the next entry.
.skip_entry:
	test ebx, ebx			# If ebx is 0, list is complete.
	jnz .int15_loop
.int15_finished:
	movw ax, [disk]
	movw [MEMORY_MAP_ENTRY_COUNT], bp # Store the entry count at the beginning of the list.

.switch_longmode:
	mov di, PAGE_TABLE_BUFFER
	push di
	
	mov cx, 0x1000
	xor eax, eax
	cld
	rep stosd			# Zero out page table buffer
	pop di
	
	movw [di], PAGE_TABLE_BUFFER + 0x1003
	movw [di + 0x1000], PAGE_TABLE_BUFFER + 0x2003
	movw [di + 0x2000], PAGE_TABLE_BUFFER + 0x3003
	mov di, PAGE_TABLE_BUFFER, 0x3000
	mov ax, 0x03
.pages_loop:
	mov [di], eax			# Identity map pages
	add eax, 0x100
	add di, 8
	cmp di, PAGE_TABLE_BUFFER + 0x4000
	jl .pages_loop

	mov al, 0xFF
	out 0x21, al
	out 0xA1, al			# Disable IRQs
	
	lidt [dummy_IDT]		# Load dummy IDT
	
	mov eax, 0xb10100000
	mov cr4, eax			# Enable paging
	
	mov edi, PAGE_TABLE_BUFFER
	mov cr3, edi			# Copy table pointer into the control reg.
	
	mov ecx, EFER
	rdmsr
	or eax, 0x00000101
	wrmsr				# Enable syscalls and activate long mode.

	mov ebx, cr0			
	or ebx, 0x800000001		
	mov cr0, ebx			# Enable protected mode.

	mov ecx, LSTAR
	mov eax, syscall_handler
	wrmsr				# Copy the location of syscall handler into LSTAR.
	
	mov ecx, SFMASK	
	mov eax, 0xFFFFFFFF
	wrmsr				# Set the mask for syscalls.
	
	lgdt [GDT_PTR]			# Load the GDT
	ljmp KERNEL_CODE_SEGMET, 0x7E00	# Jump to long mode segment.

.no_cpuid:
	mov si, no_cpuid_str
	call printstr_rmode
	cli
	hlt
.no_longmode:
	mov si, no_longmode_str
	call printstr_rmode
	cli
	hlt
.printstr_rmode:
	lodsb
	test a1, a1
	jz .done
	mov ah, 0x0E
	int 0x10
	jmp printstr_rmode
.done:
	ret
dummy_IDT:
	.word 0x00
	.long 0x00
disk: .byte 0x00
.space 510 - (. - startup), 0
.word 0xAA55

.code64
long_mode:
	mov ax, KERNEL_DATA_SEGMENT
	mov, ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax			# Set selector registers.
	
	cli				# Disable interrupts (for now)

	xor rax, rax
	xor rbx, rbx
	xor rcx, rcx
	xor rdx, rdx
	xor rdi, rdi			# Zero out registers

	mov rsp, KERNEL_STACK
	mov rbp, rsp			# The stack is the top of kernel memory.

	call kernel_main		# 64-bit kernel entry point

	cli
	hlt
syscall_handler:
	systetq
GDT:
.null: 
	.quad 0x00
.kcode:
	.quad 0x00209A0000000000
.kdata
	.quad 0x0000920000000000
.ucode:
	.quad 0x0000FA0000000000
.udata:
	.quad 0x0000F20000000000
GDT_END:
GDT_PTR:
	.word GDT_END - GDT - 1
	.long GDT

no_cpuid_str: .ascii "Error: FATAL: CPU does not support CPUID."
no_longmode_str: .ascii "Error: FATAL: CPU does not support long mode."
