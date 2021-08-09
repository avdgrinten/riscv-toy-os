project('riscv-toy-os', 'c')

kernel = executable('kernel',
	'kernel/boot.S',
	'kernel/kernel.c',
	link_args: [
		'-T' + (meson.source_root() / 'kernel/kernel.x'),
		'-static'
	])

run_target('qemu',
	command: [
		find_program('qemu-system-riscv64'),
		'-machine', 'virt',
		'-cpu', 'rv64',
		'-bios', 'opensbi-riscv64-generic-fw_dynamic.bin',
		'-m', '256m',
		'-serial', 'stdio',
		'-s',
		'-display', 'none',
		#'-d', 'int',
		'-kernel', kernel
	]
)
