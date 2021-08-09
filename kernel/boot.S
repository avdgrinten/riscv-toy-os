.section .text

.global _start
_start:
	# a0: HART ID
	# a1: DTB

	la sp, stack_top
	mv s0, zero

	jal kmain
	unimp

# void save_stack(void (*f)(void *, continuation), void *ctx)
.global save_stack
save_stack:
	addi sp, sp, -0x78

	sd gp,  0x00(sp)
	sd tp,  0x08(sp)
	sd s0,  0x10(sp)
	sd s1,  0x18(sp)
	sd s2,  0x20(sp)
	sd s3,  0x28(sp)
	sd s4,  0x30(sp)
	sd s5,  0x38(sp)
	sd s6,  0x40(sp)
	sd s7,  0x48(sp)
	sd s8,  0x50(sp)
	sd s9,  0x58(sp)
	sd s10, 0x60(sp)
	sd s11, 0x68(sp)
	sd ra, 0x70(sp)

	mv t0, a0
	mv a0, a1
	mv a1, sp
	jr t0

.global restore_stack
restore_stack:
	mv sp, a0
	ld gp,  0x00(sp)
	ld tp,  0x08(sp)
	ld s0,  0x10(sp)
	ld s1,  0x18(sp)
	ld s2,  0x20(sp)
	ld s3,  0x28(sp)
	ld s4,  0x30(sp)
	ld s5,  0x38(sp)
	ld s6,  0x40(sp)
	ld s7,  0x48(sp)
	ld s8,  0x50(sp)
	ld s9,  0x58(sp)
	ld s10, 0x60(sp)
	ld s11, 0x68(sp)
	ld ra, 0x70(sp)

	addi sp, sp, 0x78

	ret

# void enter_umode(void *entry);
.global enter_umode
enter_umode:
	# Do not trash x10 (= a0)
	mv x1, zero
	#mv x2, zero # SP
	mv x3, zero
	mv x4, zero
	mv x5, zero
	mv x6, zero
	mv x7, zero
	mv x8, zero
	mv x9, zero
	mv x11, zero
	mv x12, zero
	mv x13, zero
	mv x14, zero
	mv x15, zero
	mv x16, zero
	mv x17, zero
	mv x18, zero
	mv x19, zero
	mv x20, zero
	mv x21, zero
	mv x22, zero
	mv x23, zero
	mv x24, zero
	mv x25, zero
	mv x26, zero
	mv x27, zero
	mv x28, zero
	mv x29, zero
	mv x30, zero
	mv x31, zero

	csrw sepc, a0

	csrr a0, sstatus
	# Set SPIE.
	ori a0, a0, 0x20
	# Clear SPP.
	andi a0, a0, -1 - 0x100
	csrw sstatus, a0

	sret

.global isr
.align 2
isr:
	# Exchange s1 and sscratch.
	csrrw s1, sscratch, s1

	# Store all temporary registers.
	sd ra, 0x10(s1)
	sd a0, 0x18(s1)
	sd a1, 0x20(s1)
	sd a2, 0x28(s1)
	sd a3, 0x30(s1)
	sd a4, 0x38(s1)
	sd a5, 0x40(s1)
	sd a6, 0x48(s1)
	sd a7, 0x50(s1)
	sd t0, 0x58(s1)
	sd t1, 0x60(s1)
	sd t2, 0x68(s1)
	sd t3, 0x70(s1)
	sd t4, 0x78(s1)
	sd t5, 0x80(s1)
	sd t6, 0x88(s1)

	# Save sstatus (contains interrupt state etc.)
	csrr t0, sstatus
	sd t0, 0x98(s1)
	# Save sepc.
	csrr t0, sepc
	sd t0, 0xA0(s1)

	# Check if we interrupted user mode or supervisor mode.
#	csrr t0, scause
#	andi t0, t0, 0x100
#	bnez t0, 1f

	# TODO: If we interrupted user mode: also store callee-saved registers.
	#       (For example, to implement signals.)
	# TODO: If we interrupted user mode: load a kernel stack pointer.

1:
	# Store the next isr_frame in sccratch and re-load the saved s1 (which is still in sccratch).
	ld t0, 0x08(s1)
	beqz t0, 2f
	csrrw t1, sscratch, t0
	# Store the saved s1 in the isr_frame.
	sd t1, 0x90(s1)

	mv a0, s1
	jal handle_isr

	# Load the saved s1 into sscratch.
	ld t0, 0x90(s1)
	csrw sscratch, t0

	# Restore sstatus.
	ld t0, 0x98(s1)
	csrw sstatus, t0
	# Restore sepc.
	ld t0, 0xA0(s1)
	csrw sepc, t0

	# Store all temporary registers.
	ld ra, 0x10(s1)
	ld a0, 0x18(s1)
	ld a1, 0x20(s1)
	ld a2, 0x28(s1)
	ld a3, 0x30(s1)
	ld a4, 0x38(s1)
	ld a5, 0x40(s1)
	ld a6, 0x48(s1)
	ld a7, 0x50(s1)
	ld t0, 0x58(s1)
	ld t1, 0x60(s1)
	ld t2, 0x68(s1)
	ld t3, 0x70(s1)
	ld t4, 0x78(s1)
	ld t5, 0x80(s1)
	ld t6, 0x88(s1)

	# Exchange s1 and sscratch.
	csrrw s1, sscratch, s1

	# Returns from the trap.
	sret

2:
	jal isr_frame_overflow
	unimp

.section .bss
	.space 0x10000
stack_top:
