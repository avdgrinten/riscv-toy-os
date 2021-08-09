#include <stdarg.h>
#include <inttypes.h>

#define read_csr(csr) \
	({ \
		unsigned long __v; \
		asm volatile ("csrr %0, " csr : "=r"(__v) : : "memory");\
		__v;\
	})

#define write_csr(csr, v) \
	({ \
		unsigned long __v = (v); \
		asm volatile ("csrw " csr ", %0" : : "r"(__v) : "memory");\
	})

#define set_csr_bits(csr, bits) \
	({ \
		unsigned long __v = (bits); \
		asm volatile ("csrs " csr ", %0" : : "r"(__v) : "memory");\
	})

#define clear_csr_bits(csr, bits) \
	({ \
		unsigned long __v = (bits); \
		asm volatile ("csrc " csr ", %0" : : "r"(__v) : "memory");\
	})

typedef unsigned long sbi_word;

void sbi_call1(int ext, int func, sbi_word arg0) {
	register sbi_word rExt asm("a7") = ext;
	register sbi_word rFunc asm("a6") = func;
	register sbi_word rArg0 asm("a0") = arg0;
	register sbi_word rArg1 asm("a1");
	asm volatile("ecall" : "+r"(rArg0), "=r"(rArg1) : "r"(rExt), "r"(rFunc));
	if(rArg0)
		__builtin_trap();
}

void sbi_call2(int ext, int func, sbi_word arg0, sbi_word arg1) {
	register sbi_word rExt asm("a7") = ext;
	register sbi_word rFunc asm("a6") = func;
	register sbi_word rArg0 asm("a0") = arg0;
	register sbi_word rArg1 asm("a1") = arg1;
	asm volatile("ecall" : "+r"(rArg0), "+r"(rArg1) : "r"(rExt), "r"(rFunc));
	if(rArg0)
		__builtin_trap();
}

void fmt(const char *f, ...) {
	va_list va;
	va_start(va, f);
	while(*f) {
		if(*f != '{') {
			sbi_call1(1, 0, *(f++));
			continue;
		}
		f++;

		while(*f != '}') {
			if(!(*f))
				return;
			f++;
		}
		f++;

		unsigned long v = va_arg(va, unsigned long);
		for(int i = 0; i < 16; ++i) {
			const char *digits = "0123456789abcdef";
			int d = (v >> (60 - i * 4)) & 0xF;
			sbi_call1(1, 0, digits[d]);
		}
	}
	va_end(va);
}

enum {
	pte_valid = 1 << 0,
	pte_r = 1 << 1,
	pte_w = 1 << 2,
	pte_x = 1 << 3,
	pte_user = 1 << 4,
	pte_access = 1 << 6,
	pte_dirty = 1 << 7,
};

enum {
	pte_ppn_shift = 10
};

struct pt {
	unsigned long ptes[512];
} __attribute__((aligned(4096)));

enum {
	satp_sv48 = 9UL << 60
};

const char *exception_strings[] = {
	"instruction misaligned",
	"instruction access fault",
	"illegal instruction",
	"breakpoint",
	"load misaligned",
	"load access fault",
	"store misaligned",
	"store access fault",
	"u-mode ecall",
	"s-mode ecall",
	"reserved (10)",
	"reserved (11)",
	"instruction page fault",
	"load page fault",
	"reserved (14)",
	"store page fault",
};

enum {
	sie_s_software = (1 << 1),
	sie_s_timer = (1 << 5)
};

enum {
	sstatus_sie = (1 << 1)
};

void cli() {
	clear_csr_bits("sstatus", sstatus_sie);
}

void sti() {
	set_csr_bits("sstatus", sstatus_sie);
}

typedef struct {
	void *sp;
} continuation;

struct isr_frame {
	struct task *task;
	struct isr_frame *next;
	unsigned long ra;   // Offset 0x10.
	unsigned long a[8]; // Offset 0x18.
	unsigned long t[7]; // Offset 0x58.
	unsigned long s1;   // Offset 0x90.
	unsigned long sstatus; // Offset 0x98.
	unsigned long sepc; // Offset 0xA0.
};

enum {
	kernel_stack_size = 0x10000
};

struct task {
	continuation cont;
	struct isr_frame isr_frames[2];
	struct isr_frame *next_isr;
	char kernel_stack[kernel_stack_size];
};

struct task task1;
struct task task2;
struct task *current_task;

continuation prepare_stack(void *s_top, void (*mainfn)(continuation)) {
	void *sp = s_top - 0x78;
	*((uint64_t *)(sp + 0x70)) = (uint64_t)mainfn;
	return (continuation){.sp = sp};
}

void save_stack(void (*f)(void *, continuation), void *ctx);
void restore_stack(continuation c);

void create_task(struct task *task, void (*mainfn)) {
	task->isr_frames[0].task = task;
	task->isr_frames[1].task = task;
	task->isr_frames[0].next = &task->isr_frames[1];
	task->isr_frames[1].next = 0;
	task->next_isr = &task->isr_frames[0];

	task->cont = prepare_stack(task->kernel_stack + kernel_stack_size, mainfn);
}

void switch_task(void *ctx, continuation c) {
	struct task *task = ctx;
	if(task) {
		task->next_isr = (struct isr_frame *)read_csr("sscratch");
		task->cont = c;
	}

	struct task *next;
	if(task == &task1) {
		next = &task2;
	}else{
		next = &task1;
	}
	current_task = next;

	write_csr("sscratch", (unsigned long)next->next_isr);

	restore_stack(next->cont);
}

void isr();

void handle_isr(struct isr_frame *frame) {
	unsigned long cause = read_csr("scause");
	unsigned long code = cause & ~(1UL << 63);
	if(cause & (1UL << 63)) {
		if(code == 1) {
			fmt("it's an IPI\n");
			clear_csr_bits("sip", sie_s_software);
		}else if(code == 5) { // Timer interrupt.
			//clear_csr_bits("sie", sie_s_timer);
			unsigned long time = read_csr("time");
			sbi_call1(0x54494D45, 0, time + 10000);
			save_stack(switch_task, current_task);
		}else{
			fmt("it's an unhandled interrupt, code: {}\n", code);
		}
	}else{
		if(code == 8) { // Syscall.
			sbi_call1(1, 0, frame->a[0]);
		}else{
			unsigned long sepc = read_csr("sepc");
			fmt("it's an exception at {}\n", sepc);
			const char *s = exception_strings[cause];
			while(*s)
				sbi_call1(1, 0, *(s++));
			while(1)
				;
		}
	}
}

void isr_frame_overflow() {
	fmt("isr frame overflow\n");
	while(1)
		;
}

struct pt pml4;

void enter_umode(void *entry);

void syscall(int n, unsigned long arg0) {
	register sbi_word rArg0 asm("a0") = arg0;
	register sbi_word rN asm("a7") = n;
	asm volatile("ecall" : "+r"(rArg0) : "r"(rN) : "memory");
	if(rArg0)
		__builtin_trap();
}

void task1_umode() {
	while(1)
		syscall(0, '!');
}

void task1_main() {
	fmt("hello from task1\n");
	enter_umode(task1_umode);
}

void task2_main() {
	sti();
	fmt("hello from task2\n");
	while(1)
		fmt(".");
}

struct isr_frame global_isr_frames[2];

void kmain(unsigned long hart, void *dtb) {
	(void)hart;
	(void)dtb;
	cli();

	global_isr_frames[0].next = &global_isr_frames[1];
	write_csr("sscratch", (unsigned long)&global_isr_frames[0]);
	write_csr("stvec", ((unsigned long)&isr));

	const char *s;

	set_csr_bits("sie", sie_s_software | sie_s_timer);

	// Self-IPI.
	sbi_call2(0x735049, 0, 1 << hart, 0);

	sti();
	cli();

	fmt("hello world {}\n", 42);

	// Identity map the first 512GiB.
	pml4.ptes[0] = ((0UL >> 12) << pte_ppn_shift) | pte_valid
			| pte_r | pte_w | pte_x | pte_access | pte_dirty | pte_user;
	// Map 512GiB -> 0.
	pml4.ptes[1] = ((0UL >> 12) << pte_ppn_shift) | pte_valid
			| pte_r | pte_w | pte_x | pte_access | pte_dirty | pte_user;

	unsigned long pml4p = (unsigned long)&pml4;
	set_csr_bits("sstatus", 1 << 18);
	write_csr("satp", (pml4p >> 12) | satp_sv48);

	s = "paging works!\n";
	s += 512UL * 1024 * 1024 * 1024;
	while(*s)
		sbi_call1(1, 0, *(s++));

	create_task(&task1, task1_main);
	create_task(&task2, task2_main);

	unsigned long time = read_csr("time");
	sbi_call1(0x54494D45, 0, time + 10000);

	save_stack(switch_task, 0);

	sbi_call1(8, 0, 0);

	while(1)
		;
}
