from __future__ import annotations

from enum import IntEnum


class Opcode(IntEnum):
    # Stack and constants
    NOP = 0x00
    PUSH_NIL = 0x01
    PUSH_TRUE = 0x02
    PUSH_FALSE = 0x03
    PUSH_INT = 0x04  # imm32
    PUSH_CONST = 0x05  # u16 index
    DUP = 0x06
    POP = 0x07
    SWAP = 0x08

    # Locals / upvalues / globals
    LOAD_LOCAL = 0x10  # u16
    STORE_LOCAL = 0x11  # u16
    LOAD_GLOBAL = 0x12  # u16 (symbol constant index)
    STORE_GLOBAL = 0x13  # u16
    LOAD_UPVALUE = 0x14  # u16
    STORE_UPVALUE = 0x15  # u16
    CLOSE_UPVALUE = 0x16  # u16 (local index)

    # Control flow
    JUMP = 0x20  # s16
    JUMP_IF_TRUE = 0x21  # s16
    JUMP_IF_FALSE = 0x22  # s16
    JUMP_IF_NIL = 0x23  # s16
    RETURN = 0x24
    TAILCALL = 0x25  # u8 argc

    # Functions / closures
    MAKE_CLOSURE = 0x30  # u16 fidx, u8 ucount, then ucount*(u8 is_local, u16 index)
    MAKE_LAMBDA = 0x31  # u16 fidx (alias when no upvalues)

    # Calls
    CALL = 0x40  # u8 argc
    CALL_VAR = 0x41  # u8 argc (plus list on stack)
    CALL_KW = 0x42  # u8 argc, u8 kwc
    APPLY = 0x43  # u8 argc
    INVOKE = 0x44  # u16 mname_kidx, u8 argc

    # Lists / sequences
    CONS = 0x50
    LIST = 0x51  # u8 count
    APPEND = 0x52

    # Arithmetic / comparison
    ADD = 0x60
    SUB = 0x61
    MUL = 0x62
    DIV = 0x63
    MOD = 0x64
    LT = 0x65
    LE = 0x66
    GT = 0x67
    GE = 0x68
    EQ = 0x69
    NEQ = 0x6A
    NOT = 0x6B
    AND = 0x6C
    OR = 0x6D

    # Object / symbol / interop
    INTERN_SYM = 0x70  # u16 const idx
    LOAD_ATTR = 0x71  # u16 const idx
    STORE_ATTR = 0x72  # u16 const idx

    # Exceptions
    SETUP_CATCH = 0x80  # s16 target rel
    POP_CATCH = 0x81
    THROW = 0x82

    # Continuations
    CAPTURE_CONT = 0x90
    RESUME_CONT = 0x91
    CALL_CC = 0x92

    # Misc
    HALT = 0xFF
