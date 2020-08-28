/* linker should provide __start and __stop of sections; if not use this linker script */
SECTIONS
{
    .rel.rodata.test : {
        PROVIDE(__start_test = .);
        *(.test)
        PROVIDE(__stop_test = .);
    }
}
INSERT AFTER .text;
